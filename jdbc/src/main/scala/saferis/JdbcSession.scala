package saferis.jdbc

import saferis.*

import org.postgresql.util.PGobject
import org.postgresql.util.PSQLException
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
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZoneOffset
import java.util.Locale
import java.util.UUID
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
  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    ZLayer.fromFunction: (ds: DataSource) =>
      SqlSession.pooled(checkout(ds, config), config.defaultTimeout)

  private def checkout(ds: DataSource, config: JdbcSessionConfig): ZIO[Scope, SaferisError, SqlConnection] =
    ZIO.acquireRelease(
      for
        conn      <- ZIO.attemptBlocking(ds.getConnection()).mapError(t => SaferisError.ConnectionError(messageOf(t)))
        inTxn     <- Ref.make(false)
        committed <- Ref.make(false)
        autoCommitOff <- Ref.make(false)
      yield new JdbcConnection(conn, config, inTxn, committed, autoCommitOff)
    )(_.close)

  private[jdbc] val FetchSize = 256

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

  private[jdbc] def jdbcType(tpe: SqlType): Int = tpe match
    case SqlType.Bool        => java.sql.Types.BOOLEAN
    case SqlType.Int2        => java.sql.Types.SMALLINT
    case SqlType.Int4        => java.sql.Types.INTEGER
    case SqlType.Int8        => java.sql.Types.BIGINT
    case SqlType.Float4      => java.sql.Types.REAL
    case SqlType.Float8      => java.sql.Types.DOUBLE
    case SqlType.Numeric     => java.sql.Types.NUMERIC
    case SqlType.VarChar     => java.sql.Types.VARCHAR
    case SqlType.Text        => java.sql.Types.LONGVARCHAR
    case SqlType.Bytea       => java.sql.Types.BINARY
    case SqlType.Date        => java.sql.Types.DATE
    case SqlType.Time        => java.sql.Types.TIME
    case SqlType.Timestamp   => java.sql.Types.TIMESTAMP
    case SqlType.Timestamptz => java.sql.Types.TIMESTAMP_WITH_TIMEZONE
    case SqlType.Jsonb       => java.sql.Types.OTHER
    case SqlType.Uuid        => java.sql.Types.OTHER
    case SqlType.Array(_)    => java.sql.Types.ARRAY
    case SqlType.Other(_)    => java.sql.Types.OTHER

  private[jdbc] def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end JdbcSession

private final class JdbcConnection(
    conn: Connection,
    config: JdbcSessionConfig,
    inTxn: Ref[Boolean],
    committed: Ref[Boolean],
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
    configure(conn) *>
      driver(None, ZIO.attemptBlocking(conn.setAutoCommit(false))) *>
      autoCommitOff.set(true) *>
      committed.set(false) *>
      inTxn.set(true)

  def commit: IO[SaferisError, Unit] =
    driver(None, ZIO.attemptBlocking(conn.commit())) *> committed.set(true) *> inTxn.set(false)

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
      driver(Some(sql), ZIO.attemptBlocking(ps.executeLargeUpdate()))

  private def runRows(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Chunk[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        driver(Some(sql), ZIO.attemptBlocking(ps.executeQuery()))
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(JdbcReads.materialize(rs))).flatMap:
          case Left(err)   => ZIO.fail(err)
          case Right(rows) => ZIO.succeed(rows)

  private def runAtMostOne(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Option[SqlRow]] =
    withStatement(conn, command, sql, timeout, None): ps =>
      ZIO.acquireReleaseWith(
        driver(Some(sql), ZIO.attemptBlocking(ps.executeQuery()))
      )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
        driver(Some(sql), ZIO.attemptBlocking(rs.next())).flatMap: hasRow =>
          if !hasRow then ZIO.succeed(None)
          else
            driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(rs))).flatMap:
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
        _       <- ZIO.unless(already)(begin)
        ps      <- ZIO.acquireRelease(openStatement(conn, command, sql, timeout, Some(FetchSize)))(closeStatement)
        rs      <- ZIO.acquireRelease(driver(Some(sql), ZIO.attemptBlocking(ps.executeQuery())))(rs =>
          ZIO.attemptBlocking(rs.close()).ignore
        )
      yield
        val pulls = ZStream.repeatZIOOption:
          driver(Some(sql), ZIO.attemptBlocking(rs.next()))
            .mapError(err => Some(err))
            .flatMap: hasNext =>
              if !hasNext then ZIO.fail(None)
              else
                driver(Some(sql), ZIO.attemptBlocking(JdbcReads.readRow(rs)))
                  .mapError(err => Some(err))
                  .flatMap:
                    case Left(err)  => ZIO.fail(Some(err))
                    case Right(row) => ZIO.succeed(row)
        val commit =
          if already then ZStream.empty
          else ZStream.execute(this.commit)
        pulls ++ commit

  private def configure(conn: Connection)(using Trace): IO[SaferisError, Unit] =
    ZIO.attemptBlocking(config.configure(conn)).mapError(t => SaferisError.ConnectionError(messageOf(t)))

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

  private def driver[A](sql: Option[String], effect: IO[Throwable, A])(using Trace): IO[SaferisError, A] =
    effect.mapError(t => classifyThrowable(t, sql))

  private def render(command: SqlCommand): String =
    command.render((_, _) => "?")

  private def classifyThrowable(t: Throwable, sql: Option[String]): SaferisError = t match
    case e: java.sql.SQLTimeoutException =>
      SqlState.classify(
        ServerError(Some("57014"), messageOf(e), constraintOf(e), Some(e.getErrorCode)),
        sql,
        config.retry,
      )
    case e: SQLException =>
      SqlState.classify(
        ServerError(
          Option(e.getSQLState),
          messageOf(e),
          constraintOf(e),
          Some(e.getErrorCode),
        ),
        sql,
        config.retry,
      )
    case e: java.io.IOException => SaferisError.ConnectionLost("08000", messageOf(e), sql)
    case e                      => SaferisError.Unexpected(messageOf(e))

  private def constraintOf(e: SQLException): Option[String] = e match
    case pg: PSQLException =>
      Option(pg.getServerErrorMessage).flatMap(message => Option(message.getConstraint))
    case _ => None

  private def bind(ps: PreparedStatement, pieces: Chunk[SqlPiece]): Unit =
    val _ = pieces.foldLeft(1): (index, piece) =>
      piece match
        case SqlPiece.Text(_)      => index
        case SqlPiece.Param(value) =>
          bindOne(ps, index, value)
          index + 1

  private def bindOne(ps: PreparedStatement, index: Int, value: SqlValue): Unit =
    value match
      case SqlValue.Null(tpe)      => ps.setNull(index, jdbcType(tpe))
      case SqlValue.Bool(v)        => ps.setBoolean(index, v)
      case SqlValue.Int2(v)        => ps.setShort(index, v)
      case SqlValue.Int4(v)        => ps.setInt(index, v)
      case SqlValue.Int8(v)        => ps.setLong(index, v)
      case SqlValue.Float4(v)      => ps.setFloat(index, v)
      case SqlValue.Float8(v)      => ps.setDouble(index, v)
      case SqlValue.Numeric(v)     => ps.setBigDecimal(index, v.bigDecimal)
      case SqlValue.VarChar(v)     => ps.setString(index, v)
      case SqlValue.Text(v)        => ps.setString(index, v)
      case SqlValue.Bytea(v)       => ps.setBytes(index, v.toArray)
      case SqlValue.Date(v)        => ps.setObject(index, v)
      case SqlValue.Time(v)        => ps.setObject(index, v)
      case SqlValue.Timestamp(v)   => ps.setObject(index, v)
      case SqlValue.Timestamptz(v) => ps.setObject(index, OffsetDateTime.ofInstant(v, ZoneOffset.UTC))
      case SqlValue.Jsonb(json)    =>
        val obj = new PGobject()
        obj.setType("jsonb")
        obj.setValue(json)
        ps.setObject(index, obj)
      case SqlValue.Uuid(uuid)             => ps.setObject(index, uuid)
      case SqlValue.Other(_, text)         => ps.setString(index, text)
      case SqlValue.Array(element, values) =>
        val members = values.map(arrayMember).toArray
        val array   = conn.createArrayOf(arrayTypeName(element), members)
        ps.setArray(index, array)

  private def arrayTypeName(tpe: SqlType): String = tpe match
    case SqlType.Bool        => "bool"
    case SqlType.Int2        => "int2"
    case SqlType.Int4        => "int4"
    case SqlType.Int8        => "int8"
    case SqlType.Float4      => "float4"
    case SqlType.Float8      => "float8"
    case SqlType.Numeric     => "numeric"
    case SqlType.VarChar     => "varchar"
    case SqlType.Text        => "text"
    case SqlType.Bytea       => "bytea"
    case SqlType.Date        => "date"
    case SqlType.Time        => "time"
    case SqlType.Timestamp   => "timestamp"
    case SqlType.Timestamptz => "timestamptz"
    case SqlType.Jsonb       => "jsonb"
    case SqlType.Uuid        => "uuid"
    case SqlType.Array(_)    => "text"
    case SqlType.Other(_)    => "text"

  private def arrayMember(value: SqlValue): Object = value match
    case SqlValue.Bool(v)    => Boolean.box(v)
    case SqlValue.Int2(v)    => Short.box(v)
    case SqlValue.Int4(v)    => Int.box(v)
    case SqlValue.Int8(v)    => Long.box(v)
    case SqlValue.Float4(v)  => Float.box(v)
    case SqlValue.Float8(v)  => Double.box(v)
    case SqlValue.Text(v)    => v
    case SqlValue.VarChar(v) => v
    case SqlValue.Jsonb(v)   => v
    case SqlValue.Uuid(v)    => v
    case SqlValue.Numeric(v) => v.bigDecimal
    case other               => other.toString

end JdbcConnection

private object JdbcReads:
  def materialize(rs: ResultSet): Either[SaferisError, Chunk[SqlRow]] =
    val meta   = rs.getMetaData
    val width  = meta.getColumnCount
    val labels = Chunk.fromIterable((1 to width).map(i => Option(meta.getColumnLabel(i)).getOrElse("")))
    val rows   = Chunk.newBuilder[SqlRow]
    var failed: Option[SaferisError] = None
    while failed.isEmpty && rs.next() do
      readRow(rs, meta, labels, width) match
        case Left(err)  => failed = Some(err)
        case Right(row) => rows += row
    failed.fold[Either[SaferisError, Chunk[SqlRow]]](Right(rows.result()))(Left(_))
  end materialize

  def readRow(rs: ResultSet): Either[SaferisError, SqlRow] =
    val meta   = rs.getMetaData
    val width  = meta.getColumnCount
    val labels = Chunk.fromIterable((1 to width).map(i => Option(meta.getColumnLabel(i)).getOrElse("")))
    readRow(rs, meta, labels, width)

  private def readRow(
      rs: ResultSet,
      meta: ResultSetMetaData,
      labels: Chunk[String],
      width: Int,
  ): Either[SaferisError, SqlRow] =
    val cells = (1 to width).foldLeft[Either[SaferisError, Chunk[SqlValue]]](Right(Chunk.empty)):
      case (Left(err), _)      => Left(err)
      case (Right(acc), index) =>
        val name = typeName(meta, index)
        readCell(rs, index, name).map(acc :+ _)
    cells.map(values => SqlRow(labels, values))
  end readRow

  private def typeName(meta: ResultSetMetaData, index: Int): String =
    Option(meta.getColumnTypeName(index)).getOrElse("").toLowerCase(Locale.ROOT)

  private def readCell(rs: ResultSet, index: Int, name: String): Either[SaferisError, SqlValue] =
    def nulled(tpe: SqlType): SqlValue = SqlValue.Null(tpe)
    name match
      case "bool" =>
        val value = rs.getBoolean(index)
        Right(if rs.wasNull() then nulled(SqlType.Bool) else SqlValue.Bool(value))
      case "int2" | "smallint" | "smallserial" =>
        val value = rs.getShort(index)
        Right(if rs.wasNull() then nulled(SqlType.Int2) else SqlValue.Int2(value))
      case "int4" | "integer" | "serial" =>
        val value = rs.getInt(index)
        Right(if rs.wasNull() then nulled(SqlType.Int4) else SqlValue.Int4(value))
      case "int8" | "bigint" | "bigserial" =>
        val value = rs.getLong(index)
        Right(if rs.wasNull() then nulled(SqlType.Int8) else SqlValue.Int8(value))
      case "float4" =>
        val value = rs.getFloat(index)
        Right(if rs.wasNull() then nulled(SqlType.Float4) else SqlValue.Float4(value))
      case "float8" =>
        val value = rs.getDouble(index)
        Right(if rs.wasNull() then nulled(SqlType.Float8) else SqlValue.Float8(value))
      case "numeric" =>
        val value = rs.getBigDecimal(index)
        Right(if rs.wasNull() || value == null then nulled(SqlType.Numeric) else SqlValue.Numeric(BigDecimal(value)))
      case "varchar" | "bpchar" | "name" =>
        val value = rs.getString(index)
        Right(if rs.wasNull() || value == null then nulled(SqlType.VarChar) else SqlValue.VarChar(value))
      case "text" | "unknown" =>
        val value = rs.getString(index)
        Right(if rs.wasNull() || value == null then nulled(SqlType.Text) else SqlValue.Text(value))
      case "bytea" =>
        val value = rs.getBytes(index)
        Right(if rs.wasNull() || value == null then nulled(SqlType.Bytea) else SqlValue.Bytea(Chunk.fromArray(value)))
      case "date" =>
        val value = rs.getObject(index, classOf[LocalDate])
        Right(if rs.wasNull() || value == null then nulled(SqlType.Date) else SqlValue.Date(value))
      case "time" =>
        val value = rs.getObject(index, classOf[LocalTime])
        Right(if rs.wasNull() || value == null then nulled(SqlType.Time) else SqlValue.Time(value))
      case "timetz" =>
        val value = rs.getObject(index, classOf[OffsetTime])
        Right(if rs.wasNull() || value == null then nulled(SqlType.Time) else SqlValue.Time(value.toLocalTime))
      case "timestamp" =>
        val value = rs.getObject(index, classOf[LocalDateTime])
        Right(if rs.wasNull() || value == null then nulled(SqlType.Timestamp) else SqlValue.Timestamp(value))
      case "timestamptz" =>
        try
          val value = rs.getObject(index, classOf[OffsetDateTime])
          Right(
            if rs.wasNull() || value == null then nulled(SqlType.Timestamptz) else SqlValue.Timestamptz(value.toInstant)
          )
        catch
          case _: SQLException =>
            val value = rs.getTimestamp(index)
            Right(
              if rs.wasNull() || value == null then nulled(SqlType.Timestamptz)
              else SqlValue.Timestamptz(value.toInstant)
            )
      case "json" | "jsonb" =>
        val value = rs.getObject(index)
        if rs.wasNull() || value == null then Right(nulled(SqlType.Jsonb))
        else
          value match
            case pg: PGobject => Right(SqlValue.Jsonb(Option(pg.getValue).getOrElse("")))
            case other        => Right(SqlValue.Jsonb(other.toString))
      case "uuid" =>
        val value = rs.getObject(index, classOf[UUID])
        Right(if rs.wasNull() || value == null then nulled(SqlType.Uuid) else SqlValue.Uuid(value))
      case _ =>
        val text = rs.getString(index)
        Right:
          if rs.wasNull() || text == null then nulled(SqlType.Other(ServerType.Named(name)))
          else SqlValue.Other(ServerType.Named(name), text)
    end match
  end readCell
end JdbcReads
