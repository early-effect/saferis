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

  /** One checked-out connection, closed when the scope ends. Public so an adapter can wrap it, for example to serialize
    * checkouts on a database with one writer, and hand the result to [[SqlSession.pooled]].
    */
  def checkout(
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

  private[jdbc] def messageOf(t: Throwable): String = StandardJdbcAdapter.messageOf(t)
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

  private def runExec(command: SqlCommand, sql: SqlText, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Long] =
    withStatement(conn, command, sql, timeout, None): ps =>
      blocking(Some(sql), ps)(ps.executeLargeUpdate())

  private def runRows(command: SqlCommand, sql: SqlText, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Chunk[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        blocking(Some(sql), ps)(ps.executeQuery())
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(JdbcReads.materialize(adapter, rs))).flatMap:
          case Left(err)   => ZIO.fail(err)
          case Right(rows) => ZIO.succeed(rows)

  private def runAtMostOne(command: SqlCommand, sql: SqlText, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Option[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        blocking(Some(sql), ps)(ps.executeQuery())
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(rs.next())).flatMap: hasRow =>
          if !hasRow then ZIO.succeed(None)
          else
            driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(adapter, rs, JdbcReads.columns(rs)))).flatMap:
              case Left(err)  => ZIO.fail(err)
              case Right(row) => ZIO.succeed(Some(row))

  private def runCursor(
      command: SqlCommand,
      sql: SqlText,
      timeout: Option[Duration],
  ): ZStream[Any, SaferisError, SqlRow] =
    ZStream.unwrapScoped:
      for
        already <- inTxn.get
        (fetch, owned) = adapter.cursor match
          case CursorStrategy.Buffered                 => (None, false)
          case CursorStrategy.Fetch(size)              => (Some(size), false)
          case CursorStrategy.FetchInTransaction(size) => (Some(size), !already)
        _  <- ZIO.when(owned)(begin)
        ps <- ZIO.acquireRelease(openStatement(conn, command, sql, timeout, fetch))(closeStatement)
        rs <- ZIO.acquireRelease(
          blocking(Some(sql), ps)(ps.executeQuery())
        )(rs => ZIO.attemptBlocking(rs.close()).ignore)
        columns <- driver(Some(sql), ZIO.attemptBlocking(JdbcReads.columns(rs)))
      yield
        val pulls = ZStream.repeatZIOOption:
          driver(Some(sql), ZIO.attemptBlocking(rs.next()))
            .mapError(err => Some(err))
            .flatMap: hasNext =>
              if !hasNext then ZIO.fail(None)
              else
                driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(adapter, rs, columns)))
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
      sql: SqlText,
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
      ).flatMap(ZIO.fromEither(_))
        .foldCauseZIO(
          cause => ZIO.attemptBlocking(ps.close()).ignore *> ZIO.failCause(cause),
          _ => ZIO.succeed(ps),
        )

  private def withStatement[A](
      conn: Connection,
      command: SqlCommand,
      sql: SqlText,
      timeout: Option[Duration],
      fetchSize: Option[Int],
  )(use: PreparedStatement => IO[SaferisError, A])(using Trace): IO[SaferisError, A] =
    ZIO.acquireReleaseWith(openStatement(conn, command, sql, timeout, fetchSize))(closeStatement)(use)

  private def closeStatement(ps: PreparedStatement): UIO[Unit] =
    ZIO.attemptBlocking(ps.close()).ignore

  /** Interrupting the wait calls `Statement.cancel`. JDBC drivers stop a running statement that way, not through
    * `Thread.interrupt`.
    */
  private def blocking[A](sql: Option[SqlText], ps: PreparedStatement)(thunk: => A)(using Trace): IO[SaferisError, A] =
    ZIO
      .attemptBlockingCancelable(thunk)(ZIO.attemptBlocking(ps.cancel()).ignore)
      .mapError(t => classifyThrowable(t, sql))

  private def driver[A](sql: Option[SqlText], effect: IO[Throwable, A])(using Trace): IO[SaferisError, A] =
    effect.mapError(t => classifyThrowable(t, sql))

  private def render(command: SqlCommand): SqlText =
    command.render((_, _) => "?")

  private def classifyThrowable(t: Throwable, sql: Option[SqlText]): SaferisError = t match
    case e: java.sql.SQLTimeoutException =>
      SqlState.classify(
        ServerError(Some(SqlState.QueryCanceled), messageOf(e), None, Some(e.getErrorCode)),
        sql,
        config.retry,
      )
    case e: SQLException =>
      SqlState.classify(adapter.serverError(e), sql, config.retry)
    case e: java.io.IOException => SaferisError.ConnectionLost(SqlState.ConnectionException, messageOf(e), sql)
    case e                      => SaferisError.Unexpected(messageOf(e))

  /** Parameters bind in order. The first value the adapter refuses stops the bind. */
  private def bind(ps: PreparedStatement, pieces: Chunk[SqlPiece]): Either[SaferisError, Unit] =
    pieces
      .collect { case SqlPiece.Param(value) => value }
      .zipWithIndex
      .foldLeft[Either[SaferisError, Unit]](Right(())):
        case (Left(err), _)             => Left(err)
        case (Right(_), (value, index)) => adapter.bind(ps, index + 1, value)
end JdbcConnection

private object JdbcReads:
  /** Described once per result set, not once per row. */
  def columns(rs: ResultSet): Chunk[JdbcColumn] =
    val meta = rs.getMetaData
    Chunk.fromIterable:
      (1 to meta.getColumnCount).map: index =>
        JdbcColumn(
          index = index,
          label = ColumnName(Option(meta.getColumnLabel(index)).getOrElse("")),
          typeName = TypeName(Option(meta.getColumnTypeName(index)).getOrElse("").toLowerCase(Locale.ROOT)),
          jdbcType = meta.getColumnType(index),
        )
  end columns

  def materialize(adapter: JdbcAdapter, rs: ResultSet): Either[SaferisError, Chunk[SqlRow]] =
    val described                    = columns(rs)
    val rows                         = Chunk.newBuilder[SqlRow]
    var failed: Option[SaferisError] = None
    while failed.isEmpty && rs.next() do
      readRow(adapter, rs, described) match
        case Left(err)  => failed = Some(err)
        case Right(row) => rows += row
    failed.fold[Either[SaferisError, Chunk[SqlRow]]](Right(rows.result()))(Left(_))
  end materialize

  def readRow(adapter: JdbcAdapter, rs: ResultSet, columns: Chunk[JdbcColumn]): Either[SaferisError, SqlRow] =
    columns
      .foldLeft[Either[SaferisError, Chunk[SqlValue]]](Right(Chunk.empty)):
        case (Left(err), _)       => Left(err)
        case (Right(acc), column) => adapter.read(rs, column).map(acc :+ _)
      .map(values => SqlRow(columns.map(_.label), values))
end JdbcReads
