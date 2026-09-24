package saferis.jdbc

import saferis.*

import zio.Chunk
import zio.Duration
import zio.IO
import zio.Ref
import zio.Scope
import zio.Trace
import zio.URLayer
import zio.UIO
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.ResultSetMetaData
import java.sql.SQLException
import java.util.Locale
import javax.sql.DataSource

/** JDBC driver settings. `configure` runs once per checkout, before `BEGIN` or any user statement. `retry` sees a
  * `ServerError`, the same type every driver classifies.
  */
final case class JdbcSessionConfig(
    defaultTimeout: Option[Duration] = None,
    configure: Connection => Unit = _ => (),
    retry: ServerError => Boolean = SqlState.defaultRetryable,
)

object JdbcSession:
  def layer(
      adapter: JdbcAdapter,
      config: JdbcSessionConfig = JdbcSessionConfig(),
  ): URLayer[DataSource, SqlSession] =
    ZLayer.fromFunction: (ds: DataSource) =>
      SqlSession.pooled(checkout(ds, adapter, config), config.defaultTimeout)

  private def checkout(
      ds: DataSource,
      adapter: JdbcAdapter,
      config: JdbcSessionConfig,
  ): ZIO[Scope, SaferisError, SqlConnection] =
    ZIO.acquireRelease(
      for
        conn <- ZIO.attemptBlocking(ds.getConnection()).mapError(t => SaferisError.ConnectionError(messageOf(t)))
        _    <- ZIO
          .attemptBlocking(config.configure(conn))
          .mapError(t => SaferisError.ConnectionError(messageOf(t)))
          .tapError(_ => ZIO.attemptBlocking(conn.close()).ignore)
        inTxn         <- Ref.make(false)
        autoCommitOff <- Ref.make(false)
      yield new JdbcConnection(conn, adapter, config, inTxn, autoCommitOff)
    )(_.close)

  /** Whole seconds, round up, minimum 1. `setQueryTimeout(0)` means no limit. Infinity is `Int.MaxValue` seconds. */
  private[jdbc] def toJdbcSeconds(d: Duration): Int =
    if d == Duration.Infinity then Int.MaxValue
    else
      val seconds       = d.getSeconds
      val nanosFraction = d.getNano
      if seconds <= 0L && nanosFraction <= 0 then 1
      else if seconds >= Int.MaxValue.toLong then Int.MaxValue
      else if nanosFraction > 0 then if seconds + 1L >= Int.MaxValue.toLong then Int.MaxValue else (seconds + 1L).toInt
      else seconds.toInt

  private[jdbc] def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end JdbcSession

private final class JdbcConnection(
    conn: Connection,
    adapter: JdbcAdapter,
    config: JdbcSessionConfig,
    inTxn: Ref[Boolean],
    autoCommitOff: Ref[Boolean],
) extends SqlConnection:

  import JdbcSession.*

  def execute(command: SqlCommand): IO[SaferisError, Long] =
    val sql = render(command)
    runExec(command, sql, command.timeout)

  def query(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
    val sql = render(command)
    runRows(command, sql, command.timeout)

  def queryAtMostOne(command: SqlCommand): IO[SaferisError, Option[SqlRow]] =
    val sql = render(command)
    runAtMostOne(command, sql, command.timeout)

  def cursor(command: SqlCommand): ZStream[Any, SaferisError, SqlRow] =
    runCursor(command, render(command), command.timeout)

  def begin: IO[SaferisError, Unit] =
    driver(None, ZIO.attemptBlocking(conn.setAutoCommit(false))) *>
      autoCommitOff.set(true) *>
      inTxn.set(true)

  def commit: IO[SaferisError, Unit] =
    ZIO
      .attemptBlocking(adapter.commitRejected(conn))
      .mapError(t => classifyThrowable(t, None))
      .flatMap:
        case Some(error) => ZIO.fail(SqlState.classify(error, None, config.retry))
        case None        => driver(None, ZIO.attemptBlocking(conn.commit())) *> inTxn.set(false)

  def rollback: UIO[Unit] =
    inTxn.get.flatMap: open =>
      ZIO.when(open)(ZIO.attemptBlocking(conn.rollback()).ignore *> inTxn.set(false)).unit

  /** Put autocommit back before the pool sees this connection again. */
  def close: UIO[Unit] =
    rollback *>
      autoCommitOff.get.flatMap: touched =>
        ZIO.when(touched)(ZIO.attemptBlocking(conn.setAutoCommit(true)).ignore) *>
          ZIO.attemptBlocking(conn.close()).ignore

  private def runExec(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Long] =
    withStatement(conn, command, sql, timeout, None): ps =>
      blocking(Some(sql), ps)(ps.executeLargeUpdate())

  private def runRows(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Chunk[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        blocking(Some(sql), ps)(ps.executeQuery())
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(JdbcReads.materialize(adapter, rs))).flatMap:
          case Left(err)   => ZIO.fail(err)
          case Right(rows) => ZIO.succeed(rows)

  private def runAtMostOne(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Option[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        blocking(Some(sql), ps)(ps.executeQuery())
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(rs.next())).flatMap: hasRow =>
          if !hasRow then ZIO.succeed(None)
          else
            driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(adapter, rs))).flatMap:
              case Left(err)  => ZIO.fail(err)
              case Right(row) => ZIO.succeed(Some(row))

  private def runCursor(
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
  ): ZStream[Any, SaferisError, SqlRow] =
    ZStream.unwrapScoped:
      for
        already <- inTxn.get
        strategy = adapter.cursor
        owned    = strategy match
          case CursorStrategy.Fetch(_, true) if !already => true
          case _                                         => false
        fetch = strategy match
          case CursorStrategy.Fetch(size, _) => Some(size)
          case CursorStrategy.Buffered       => None
        _  <- ZIO.when(owned)(begin)
        ps <- ZIO.acquireRelease(openStatement(conn, command, sql, timeout, fetch))(closeStatement)
        rs <- ZIO.acquireRelease(
          blocking(Some(sql), ps)(ps.executeQuery())
        )(rs => ZIO.attemptBlocking(rs.close()).ignore)
      yield
        val pulls = ZStream.repeatZIOOption:
          driver(Some(sql), ZIO.attemptBlocking(rs.next()))
            .mapError(err => Some(err))
            .flatMap: hasNext =>
              if !hasNext then ZIO.fail(None)
              else
                driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(adapter, rs)))
                  .mapError(err => Some(err))
                  .flatMap:
                    case Left(err)  => ZIO.fail(Some(err))
                    case Right(row) => ZIO.succeed(row)
        val commit =
          if owned then ZStream.execute(this.commit)
          else ZStream.empty
        pulls ++ commit

  private def openStatement(
      conn: Connection,
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      fetchSize: Option[Int],
  )(using Trace): IO[SaferisError, PreparedStatement] =
    driver(Some(sql), ZIO.attemptBlocking(conn.prepareStatement(sql))).flatMap: ps =>
      driver(
        Some(sql),
        ZIO.attemptBlocking:
          timeout.foreach(d => ps.setQueryTimeout(toJdbcSeconds(d)))
          fetchSize.foreach(ps.setFetchSize)
          bind(ps, command.pieces),
      ).foldCauseZIO(
        cause => ZIO.attemptBlocking(ps.close()).ignore *> ZIO.failCause(cause),
        _ => ZIO.succeed(ps),
      )

  private def withStatement[A](
      conn: Connection,
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      fetchSize: Option[Int],
  )(use: PreparedStatement => IO[SaferisError, A])(using Trace): IO[SaferisError, A] =
    ZIO.acquireReleaseWith(openStatement(conn, command, sql, timeout, fetchSize))(closeStatement)(use)

  private def closeStatement(ps: PreparedStatement): UIO[Unit] =
    ZIO.attemptBlocking(ps.close()).ignore

  /** Forks the call so the wait can be interrupted. `cancel` is what stops `pg_sleep`: pgjdbc ignores
    * `Thread.interrupt`.
    */
  private def blocking[A](sql: Option[String], ps: PreparedStatement)(thunk: => A)(using Trace): IO[SaferisError, A] =
    ZIO
      .attemptBlocking(thunk)
      .fork
      .flatMap { fiber =>
        fiber.join.onInterrupt(ZIO.attemptBlocking(ps.cancel()).ignore)
      }
      .mapError(t => classifyThrowable(t, sql))

  private def driver[A](sql: Option[String], effect: IO[Throwable, A])(using Trace): IO[SaferisError, A] =
    effect.mapError(t => classifyThrowable(t, sql))

  private def render(command: SqlCommand): String =
    command.render((_, _) => "?")

  private def classifyThrowable(t: Throwable, sql: Option[String]): SaferisError = t match
    case e: java.sql.SQLTimeoutException =>
      SqlState.classify(
        ServerError(Some("57014"), messageOf(e), None, Some(e.getErrorCode)),
        sql,
        config.retry,
      )
    case e: SQLException =>
      SqlState.classify(adapter.serverError(e), sql, config.retry)
    case e: java.io.IOException => SaferisError.ConnectionLost("08000", messageOf(e), sql)
    case e                      => SaferisError.Unexpected(messageOf(e))

  private def bind(ps: PreparedStatement, pieces: Chunk[SqlPiece]): Unit =
    val _ = pieces.foldLeft(1): (index, piece) =>
      piece match
        case SqlPiece.Text(_)      => index
        case SqlPiece.Param(value) =>
          adapter.bind(ps, index, value)
          index + 1
end JdbcConnection

private object JdbcReads:
  def materialize(adapter: JdbcAdapter, rs: ResultSet): Either[SaferisError, Chunk[SqlRow]] =
    val meta   = rs.getMetaData
    val width  = meta.getColumnCount
    val labels = Chunk.fromIterable((1 to width).map(i => Option(meta.getColumnLabel(i)).getOrElse("")))
    val rows   = Chunk.newBuilder[SqlRow]
    var failed: Option[SaferisError] = None
    while failed.isEmpty && rs.next() do
      readRow(adapter, rs, meta, labels, width) match
        case Left(err)  => failed = Some(err)
        case Right(row) => rows += row
    failed.fold[Either[SaferisError, Chunk[SqlRow]]](Right(rows.result()))(Left(_))
  end materialize

  def readRow(adapter: JdbcAdapter, rs: ResultSet): Either[SaferisError, SqlRow] =
    val meta   = rs.getMetaData
    val width  = meta.getColumnCount
    val labels = Chunk.fromIterable((1 to width).map(i => Option(meta.getColumnLabel(i)).getOrElse("")))
    readRow(adapter, rs, meta, labels, width)

  private def readRow(
      adapter: JdbcAdapter,
      rs: ResultSet,
      meta: ResultSetMetaData,
      labels: Chunk[String],
      width: Int,
  ): Either[SaferisError, SqlRow] =
    val cells = (1 to width).foldLeft[Either[SaferisError, Chunk[SqlValue]]](Right(Chunk.empty)):
      case (Left(err), _)      => Left(err)
      case (Right(acc), index) =>
        val name = typeName(meta, index)
        adapter.read(rs, index, name).map(acc :+ _)
    cells.map(values => SqlRow(labels, values))
  end readRow

  private def typeName(meta: ResultSetMetaData, index: Int): String =
    Option(meta.getColumnTypeName(index)).getOrElse("").toLowerCase(Locale.ROOT)
end JdbcReads
