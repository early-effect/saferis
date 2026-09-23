package saferis

import org.postgresql.util.PGobject
import org.postgresql.util.PSQLException
import zio.Cause
import zio.Chunk
import zio.Clock
import zio.Duration
import zio.Exit
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
import scala.collection.mutable.ListBuffer

/** JDBC driver settings. `configure` runs once per checkout, before `BEGIN` or any user statement. */
final case class JdbcSessionConfig(
    defaultTimeout: Option[Duration] = None,
    configure: Connection => Unit = _ => (),
    retry: SQLException => Boolean = e => SqlState.defaultRetryable(Option(e.getSQLState)),
    listener: SqlListener = SqlListener.noop,
)

/** JDBC `DatabaseMetaData` lookup. Not a method on `SqlSession`. PR 2 deletes this. */
private[saferis] trait JdbcMetadataProbe:
  def introspect(table: String)(using Trace): IO[SaferisError, Option[DatabaseTable]]

object JdbcSession:
  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    ZLayer.fromFunction((ds: DataSource) => new JdbcSession(ds, config, None))

  private val FetchSize = 256

  /** Whole seconds, round up, minimum 1. `setQueryTimeout(0)` means no limit. Infinity is `Int.MaxValue` seconds. */
  private def toJdbcSeconds(d: Duration): Int =
    if d == Duration.Infinity then Int.MaxValue
    else
      val seconds       = d.getSeconds
      val nanosFraction = d.getNano
      if seconds <= 0L && nanosFraction <= 0 then 1
      else if seconds >= Int.MaxValue.toLong then Int.MaxValue
      else if nanosFraction > 0 then if seconds + 1L >= Int.MaxValue.toLong then Int.MaxValue else (seconds + 1L).toInt
      else seconds.toInt

  private def jdbcType(tpe: PgType): Int = tpe match
    case PgType.Bool        => java.sql.Types.BOOLEAN
    case PgType.Int2        => java.sql.Types.SMALLINT
    case PgType.Int4        => java.sql.Types.INTEGER
    case PgType.Int8        => java.sql.Types.BIGINT
    case PgType.Float4      => java.sql.Types.REAL
    case PgType.Float8      => java.sql.Types.DOUBLE
    case PgType.Numeric     => java.sql.Types.NUMERIC
    case PgType.VarChar     => java.sql.Types.VARCHAR
    case PgType.Text        => java.sql.Types.LONGVARCHAR
    case PgType.Bytea       => java.sql.Types.BINARY
    case PgType.Date        => java.sql.Types.DATE
    case PgType.Time        => java.sql.Types.TIME
    case PgType.Timestamp   => java.sql.Types.TIMESTAMP
    case PgType.Timestamptz => java.sql.Types.TIMESTAMP_WITH_TIMEZONE
    case PgType.Jsonb       => java.sql.Types.OTHER
    case PgType.Uuid        => java.sql.Types.OTHER

  private def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end JdbcSession

private final class Txn(val connection: Connection, val failure: Ref[Option[SaferisError]])

private final class JdbcSession(
    dataSource: DataSource,
    config: JdbcSessionConfig,
    txn: Option[Txn],
) extends SqlSession
    with JdbcMetadataProbe:

  import JdbcSession.*

  def defaultTimeout: Option[Duration] = config.defaultTimeout

  def exec(command: SqlCommand): IO[SaferisError, Long] =
    val sql     = render(command)
    val timeout = applied(command)
    observed(sql, timeout, runExec(command, sql, timeout), identity)

  def query[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): IO[SaferisError, Chunk[A]] =
    val sql     = render(command)
    val timeout = applied(command)
    observed(sql, timeout, runQuery(command, sql, timeout, read), rows => rows.length.toLong)

  def queryAtMostOne[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): IO[SaferisError, Option[A]] =
    val sql     = render(command)
    val timeout = applied(command)
    observed(sql, timeout, runQueryAtMostOne(command, sql, timeout, read), row => if row.isDefined then 1L else 0L)

  def stream[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): ZStream[Any, SaferisError, A] =
    val sql     = render(command)
    val timeout = applied(command)
    ZStream.unwrap:
      guard(sql).foldZIO(
        err =>
          config.listener
            .executed(SqlExecuted(sql, Duration.Zero, timeout, Left(err)))
            .as(ZStream.fail(err)),
        _ => ZIO.succeed(runStream(command, sql, timeout, read)),
      )
  end stream

  def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A] =
    txn match
      case Some(_) => body.provideSomeLayer[R](ZLayer.succeed[SqlSession](this))
      case None    =>
        ZIO.scoped:
          for
            conn      <- checkout
            _         <- configure(conn)
            _         <- ZIO.attemptBlocking(conn.setAutoCommit(false)).mapError(t => classifyThrowable(t, None))
            failure   <- Ref.make[Option[SaferisError]](None)
            committed <- Ref.make(false)
            _         <- ZIO.addFinalizer(
              committed.get.flatMap: done =>
                ZIO.unless(done)(ZIO.attemptBlocking(conn.rollback()).ignore)
            )
            child = new JdbcSession(dataSource, config, Some(new Txn(conn, failure)))
            exit   <- body.provideSomeLayer[R](ZLayer.succeed[SqlSession](child)).exit
            result <- exit match
              case Exit.Success(value) =>
                failure.get.flatMap:
                  case Some(err) => ZIO.fail(err)
                  case None      =>
                    ZIO.attemptBlocking(conn.commit()).mapError(t => classifyThrowable(t, None)) *>
                      committed.set(true).as(value)
              case Exit.Failure(cause) => ZIO.failCause(cause)
          yield result

  def introspect(table: String)(using Trace): IO[SaferisError, Option[DatabaseTable]] =
    val sql = s"introspect $table"
    guard(sql) *> withConnection: conn =>
      driver(sql, ZIO.attemptBlocking(JdbcMetadata.introspect(conn, table)))

  private def runExec(command: SqlCommand, sql: String, timeout: Option[Duration])(using
      Trace
  ): IO[SaferisError, Long] =
    guard(sql) *> withConnection: conn =>
      withStatement(conn, command, sql, timeout, None): ps =>
        driver(sql, ZIO.attemptBlocking(ps.executeLargeUpdate()))

  private def runQuery[A](
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      read: SqlRow => Either[SaferisError, A],
  )(using Trace): IO[SaferisError, Chunk[A]] =
    guard(sql) *> withConnection: conn =>
      withStatement(conn, command, sql, timeout, None): ps =>
        ZIO.acquireReleaseWith(
          driver(sql, ZIO.attemptBlocking(ps.executeQuery()))
        )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
          driver(sql, ZIO.attemptBlocking(JdbcReads.materialize(rs))).flatMap:
            case Left(err)   => ZIO.fail(err)
            case Right(rows) => ZIO.fromEither(decodeRows(rows, read))

  private def runQueryAtMostOne[A](
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      read: SqlRow => Either[SaferisError, A],
  )(using Trace): IO[SaferisError, Option[A]] =
    guard(sql) *> withConnection: conn =>
      withStatement(conn, command, sql, timeout, None): ps =>
        ZIO.acquireReleaseWith(
          driver(sql, ZIO.attemptBlocking(ps.executeQuery()))
        )(rs => ZIO.attemptBlocking(rs.close()).ignore): rs =>
          driver(sql, ZIO.attemptBlocking(rs.next())).flatMap: hasRow =>
            if !hasRow then ZIO.succeed(None)
            else
              driver(sql, ZIO.attemptBlocking(JdbcReads.readRow(rs))).flatMap:
                case Left(err)  => ZIO.fail(err)
                case Right(row) =>
                  read(row) match
                    case Left(err)    => ZIO.fail(err)
                    case Right(value) => ZIO.succeed(Some(value))

  private def runStream[A](
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      read: SqlRow => Either[SaferisError, A],
  ): ZStream[Any, SaferisError, A] =
    ZStream.unwrapScoped:
      for
        start     <- Clock.nanoTime
        count     <- Ref.make(0L)
        began     <- Ref.make(false)
        committed <- Ref.make(false)
        conn      <- txn match
          case Some(state) => ZIO.succeed(state.connection)
          case None        => checkout
        _ <- ZIO.addFinalizerExit(exit => finishStream(conn, began, committed, count, start, sql, timeout, exit))
        _ <- ZIO.when(txn.isEmpty)(configure(conn))
        _ <- ZIO.when(txn.isEmpty)(
          driver(sql, ZIO.attemptBlocking(conn.setAutoCommit(false))) *> began.set(true)
        )
        ps <- ZIO.acquireRelease(openStatement(conn, command, sql, timeout, Some(FetchSize)))(closeStatement)
        rs <- ZIO.acquireRelease(driver(sql, ZIO.attemptBlocking(ps.executeQuery())))(rs =>
          ZIO.attemptBlocking(rs.close()).ignore
        )
      yield
        val pulls = ZStream.repeatZIOOption:
          driver(sql, ZIO.attemptBlocking(rs.next()))
            .mapError(err => Some(err))
            .flatMap: hasNext =>
              if !hasNext then ZIO.fail(None)
              else
                driver(sql, ZIO.attemptBlocking(JdbcReads.readRow(rs)))
                  .mapError(err => Some(err))
                  .flatMap:
                    case Left(err)  => ZIO.fail(Some(err))
                    case Right(row) =>
                      read(row) match
                        case Left(err)    => ZIO.fail(Some(err))
                        case Right(value) => count.update(_ + 1).as(value)
        val commit =
          if txn.isEmpty then
            ZStream.execute(
              ZIO.attemptBlocking(conn.commit()).mapError(t => classifyThrowable(t, Some(sql))) *>
                committed.set(true)
            )
          else ZStream.empty
        pulls ++ commit

  /** Rollback only when a pool read transaction was opened and `COMMIT` did not happen. Cursor and connection close are
    * the other finalizers. A failed rollback does not replace the stream error. Statement failures are recorded by
    * `driver`, not here: interrupt, defect, and `DecodingError` do not abort the transaction.
    */
  private def finishStream(
      conn: Connection,
      began: Ref[Boolean],
      committed: Ref[Boolean],
      count: Ref[Long],
      start: Long,
      sql: String,
      timeout: Option[Duration],
      exit: Exit[Any, Any],
  )(using Trace): UIO[Unit] =
    for
      n       <- count.get
      started <- began.get
      done    <- committed.get
      failed = exit match
        case Exit.Success(_)     => None
        case Exit.Failure(cause) => Some(failureOf(cause))
      _   <- ZIO.when(started && !done)(ZIO.attemptBlocking(conn.rollback()).ignore)
      end <- Clock.nanoTime
      outcome = failed.fold[Either[SaferisError, Long]](Right(n))(Left(_))
      _ <- config.listener.executed(
        SqlExecuted(sql, Duration.fromNanos(math.max(0L, end - start)), timeout, outcome)
      )
    yield ()
  end finishStream

  private def observed[A](
      sql: String,
      timeout: Option[Duration],
      effect: IO[SaferisError, A],
      rows: A => Long,
  )(using Trace): IO[SaferisError, A] =
    for
      start <- Clock.nanoTime
      exit  <- effect.exit
      end   <- Clock.nanoTime
      outcome = exit match
        case Exit.Success(value) => Right(rows(value))
        case Exit.Failure(cause) => Left(failureOf(cause))
      _ <- config.listener.executed(
        SqlExecuted(sql, Duration.fromNanos(math.max(0L, end - start)), timeout, outcome)
      )
      value <- exit.foldExit(ZIO.failCause, ZIO.succeed)
    yield value

  private def failureOf(cause: Cause[Any]): SaferisError =
    cause.failureOption match
      case Some(err: SaferisError) => err
      case _                       => SaferisError.Unexpected("interrupted")

  private def withConnection[A](use: Connection => IO[SaferisError, A])(using Trace): IO[SaferisError, A] =
    txn match
      case Some(state) => use(state.connection)
      case None        =>
        ZIO.scoped:
          for
            conn <- checkout
            _    <- configure(conn)
            a    <- use(conn)
          yield a

  private def checkout(using Trace): ZIO[Scope, SaferisError, Connection] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(dataSource.getConnection()).mapError(t => SaferisError.ConnectionError(messageOf(t)))
    )(conn => ZIO.attemptBlocking(conn.close()).ignore)

  private def configure(conn: Connection)(using Trace): IO[SaferisError, Unit] =
    ZIO.attemptBlocking(config.configure(conn)).mapError(t => SaferisError.ConnectionError(messageOf(t)))

  private def openStatement(
      conn: Connection,
      command: SqlCommand,
      sql: String,
      timeout: Option[Duration],
      fetchSize: Option[Int],
  )(using Trace): IO[SaferisError, PreparedStatement] =
    driver(sql, ZIO.attemptBlocking(conn.prepareStatement(sql))).flatMap: ps =>
      driver(
        sql,
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

  private def driver[A](sql: String, effect: IO[Throwable, A])(using Trace): IO[SaferisError, A] =
    effect.mapError(t => classifyThrowable(t, Some(sql))).tapError(record)

  private def guard(sql: String)(using Trace): IO[SaferisError, Unit] =
    txn match
      case None        => ZIO.unit
      case Some(state) =>
        state.failure.get.flatMap:
          case None    => ZIO.unit
          case Some(_) =>
            ZIO.fail(
              SaferisError.QueryError(
                Some("25P02"),
                "current transaction is aborted, commands ignored until end of transaction block",
                Some(sql),
              )
            )

  private def record(err: SaferisError)(using Trace): UIO[Unit] =
    txn match
      case None        => ZIO.unit
      case Some(state) =>
        err match
          case SaferisError.QueryError(Some("25P02"), _, _) => ZIO.unit
          case SaferisError.DecodingError(_, _, _)          => ZIO.unit
          case other                                        => state.failure.update(prev => prev.orElse(Some(other)))

  private def render(command: SqlCommand): String =
    command.render((_, _) => "?")

  private def applied(command: SqlCommand): Option[Duration] =
    command.timeout.orElse(config.defaultTimeout)

  private def classifyThrowable(t: Throwable, sql: Option[String]): SaferisError = t match
    case e: SQLException =>
      val timedOut = e.isInstanceOf[java.sql.SQLTimeoutException] || Option(e.getSQLState).contains("57014")
      val vendor   = !timedOut && config.retry(e)
      SqlState.classify(
        Option(e.getSQLState),
        Option(e.getMessage).getOrElse(""),
        constraintOf(e),
        sql,
        vendor,
        timedOut,
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
      case SqlValue.Uuid(uuid) => ps.setObject(index, uuid)

  private def decodeRows[A](
      rows: Chunk[SqlRow],
      read: SqlRow => Either[SaferisError, A],
  ): Either[SaferisError, Chunk[A]] =
    rows.foldLeft[Either[SaferisError, Chunk[A]]](Right(Chunk.empty)):
      case (Left(err), _)    => Left(err)
      case (Right(acc), row) => read(row).map(acc :+ _)
end JdbcSession

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
        val label = labels(index - 1)
        val name  = typeName(meta, index)
        readCell(rs, index, name, label).map(acc :+ _)
    cells.map(values => SqlRow(labels, values))
  end readRow

  private def typeName(meta: ResultSetMetaData, index: Int): String =
    Option(meta.getColumnTypeName(index)).getOrElse("").toLowerCase(Locale.ROOT)

  private def readCell(rs: ResultSet, index: Int, name: String, label: String): Either[SaferisError, SqlValue] =
    def nulled(tpe: PgType): SqlValue = SqlValue.Null(tpe)
    def unrecognized                  = Left(SaferisError.DecodingError(label, name, s"unrecognized type $name"))
    name match
      case "bool" =>
        val value = rs.getBoolean(index)
        Right(if rs.wasNull() then nulled(PgType.Bool) else SqlValue.Bool(value))
      case "int2" | "smallint" | "smallserial" =>
        val value = rs.getShort(index)
        Right(if rs.wasNull() then nulled(PgType.Int2) else SqlValue.Int2(value))
      case "int4" | "integer" | "serial" =>
        val value = rs.getInt(index)
        Right(if rs.wasNull() then nulled(PgType.Int4) else SqlValue.Int4(value))
      case "int8" | "bigint" | "bigserial" =>
        val value = rs.getLong(index)
        Right(if rs.wasNull() then nulled(PgType.Int8) else SqlValue.Int8(value))
      case "float4" =>
        val value = rs.getFloat(index)
        Right(if rs.wasNull() then nulled(PgType.Float4) else SqlValue.Float4(value))
      case "float8" =>
        val value = rs.getDouble(index)
        Right(if rs.wasNull() then nulled(PgType.Float8) else SqlValue.Float8(value))
      case "numeric" =>
        val value = rs.getBigDecimal(index)
        Right(if rs.wasNull() || value == null then nulled(PgType.Numeric) else SqlValue.Numeric(BigDecimal(value)))
      case "varchar" | "bpchar" | "name" =>
        val value = rs.getString(index)
        Right(if rs.wasNull() || value == null then nulled(PgType.VarChar) else SqlValue.VarChar(value))
      case "text" | "unknown" =>
        val value = rs.getString(index)
        Right(if rs.wasNull() || value == null then nulled(PgType.Text) else SqlValue.Text(value))
      case "bytea" =>
        val value = rs.getBytes(index)
        Right(if rs.wasNull() || value == null then nulled(PgType.Bytea) else SqlValue.Bytea(Chunk.fromArray(value)))
      case "date" =>
        val value = rs.getObject(index, classOf[LocalDate])
        Right(if rs.wasNull() || value == null then nulled(PgType.Date) else SqlValue.Date(value))
      case "time" =>
        val value = rs.getObject(index, classOf[LocalTime])
        Right(if rs.wasNull() || value == null then nulled(PgType.Time) else SqlValue.Time(value))
      case "timetz" =>
        val value = rs.getObject(index, classOf[OffsetTime])
        Right(if rs.wasNull() || value == null then nulled(PgType.Time) else SqlValue.Time(value.toLocalTime))
      case "timestamp" =>
        val value = rs.getObject(index, classOf[LocalDateTime])
        Right(if rs.wasNull() || value == null then nulled(PgType.Timestamp) else SqlValue.Timestamp(value))
      case "timestamptz" =>
        try
          val value = rs.getObject(index, classOf[OffsetDateTime])
          Right(
            if rs.wasNull() || value == null then nulled(PgType.Timestamptz) else SqlValue.Timestamptz(value.toInstant)
          )
        catch
          case _: SQLException =>
            val value = rs.getTimestamp(index)
            Right(
              if rs.wasNull() || value == null then nulled(PgType.Timestamptz)
              else SqlValue.Timestamptz(value.toInstant)
            )
      case "json" | "jsonb" =>
        val value = rs.getObject(index)
        if rs.wasNull() || value == null then Right(nulled(PgType.Jsonb))
        else
          value match
            case pg: PGobject => Right(SqlValue.Jsonb(Option(pg.getValue).getOrElse("")))
            case other        => Right(SqlValue.Jsonb(other.toString))
      case "uuid" =>
        val value = rs.getObject(index, classOf[UUID])
        Right(if rs.wasNull() || value == null then nulled(PgType.Uuid) else SqlValue.Uuid(value))
      case _ => unrecognized
    end match
  end readCell
end JdbcReads

private object JdbcMetadata:
  def introspect(conn: Connection, tableName: String): Option[DatabaseTable] =
    val meta   = conn.getMetaData
    val schema = conn.getSchema
    val tables = meta.getTables(null, schema, tableName, Array("TABLE"))
    try
      if !tables.next() then
        val tablesLower = meta.getTables(null, schema, tableName.toLowerCase, Array("TABLE"))
        try
          if !tablesLower.next() then
            val tablesUpper = meta.getTables(null, schema, tableName.toUpperCase, Array("TABLE"))
            try
              if !tablesUpper.next() then None
              else Some(buildTable(meta, schema, tableName.toUpperCase))
            finally tablesUpper.close()
          else Some(buildTable(meta, schema, tableName.toLowerCase))
        finally tablesLower.close()
      else Some(buildTable(meta, schema, tableName))
    finally tables.close()
    end try
  end introspect

  private def buildTable(meta: java.sql.DatabaseMetaData, schema: String, tableName: String): DatabaseTable =
    DatabaseTable(
      tableName,
      columns(meta, schema, tableName),
      primaryKeys(meta, schema, tableName),
      indexes(meta, schema, tableName, primaryKeys(meta, schema, tableName)),
      Nil,
      foreignKeys(meta, schema, tableName),
    )

  private def columns(meta: java.sql.DatabaseMetaData, schema: String, tableName: String): Seq[DatabaseColumn] =
    val rs    = meta.getColumns(null, schema, tableName, null)
    val found = ListBuffer.empty[DatabaseColumn]
    try
      while rs.next() do
        found += DatabaseColumn(
          name = rs.getString("COLUMN_NAME"),
          dataType = rs.getString("TYPE_NAME"),
          isNullable = rs.getString("IS_NULLABLE") == "YES",
          isPrimaryKey = false,
          defaultValue = Option(rs.getString("COLUMN_DEF")),
          ordinalPosition = rs.getInt("ORDINAL_POSITION"),
        )
    finally rs.close()
    end try
    found.toSeq
  end columns

  private def primaryKeys(meta: java.sql.DatabaseMetaData, schema: String, tableName: String): Seq[String] =
    val rs   = meta.getPrimaryKeys(null, schema, tableName)
    val keys = ListBuffer.empty[(String, Int)]
    try while rs.next() do keys += (rs.getString("COLUMN_NAME") -> rs.getInt("KEY_SEQ"))
    finally rs.close()
    keys.sortBy(_._2).map(_._1).toSeq

  private def indexes(
      meta: java.sql.DatabaseMetaData,
      schema: String,
      tableName: String,
      keys: Seq[String],
  ): Seq[DatabaseIndex] =
    val rs    = meta.getIndexInfo(null, schema, tableName, false, false)
    val found = ListBuffer.empty[(String, String, Boolean, Int)]
    try
      while rs.next() do
        val indexName = rs.getString("INDEX_NAME")
        val colName   = rs.getString("COLUMN_NAME")
        if indexName != null && colName != null then
          found += ((indexName, colName, !rs.getBoolean("NON_UNIQUE"), rs.getInt("ORDINAL_POSITION")))
    finally rs.close()
    found
      .groupBy(_._1)
      .map { case (name, cols) =>
        val sorted = cols.sortBy(_._4).map(_._2).toSeq
        val unique = cols.headOption.exists(_._3)
        DatabaseIndex(name, sorted, unique, None)
      }
      .toSeq
      .filterNot(idx => idx.columns.map(_.toLowerCase) == keys.map(_.toLowerCase))
  end indexes

  private def foreignKeys(
      meta: java.sql.DatabaseMetaData,
      schema: String,
      tableName: String,
  ): Seq[DatabaseForeignKey] =
    val rs    = meta.getImportedKeys(null, schema, tableName)
    val found = ListBuffer.empty[(String, String, String, String, String, Int, String)]
    try
      while rs.next() do
        found += (
          (
            rs.getString("FK_NAME"),
            rs.getString("FKCOLUMN_NAME"),
            rs.getString("PKTABLE_NAME"),
            rs.getString("PKCOLUMN_NAME"),
            rule(rs.getShort("DELETE_RULE")),
            rs.getInt("KEY_SEQ"),
            rule(rs.getShort("UPDATE_RULE")),
          )
        )
    finally rs.close()
    end try
    found
      .groupBy(_._1)
      .map { case (name, cols) =>
        val sorted   = cols.sortBy(_._6)
        val fromCols = sorted.map(_._2).toSeq
        val toTable  = sorted.headOption.map(_._3).getOrElse("")
        val toCols   = sorted.map(_._4).toSeq
        val onDelete = sorted.headOption.map(_._5).getOrElse("NO ACTION")
        val onUpdate = sorted.headOption.map(_._7).getOrElse("NO ACTION")
        DatabaseForeignKey(name, fromCols, toTable, toCols, onDelete, onUpdate)
      }
      .toSeq
  end foreignKeys

  private def rule(value: Short): String = value match
    case java.sql.DatabaseMetaData.importedKeyCascade    => "CASCADE"
    case java.sql.DatabaseMetaData.importedKeySetNull    => "SET NULL"
    case java.sql.DatabaseMetaData.importedKeySetDefault => "SET DEFAULT"
    case java.sql.DatabaseMetaData.importedKeyRestrict   => "RESTRICT"
    case _                                               => "NO ACTION"
end JdbcMetadata
