package saferis.sqlite.jdbc

import saferis.*
import saferis.jdbc.JdbcAdapter
import saferis.jdbc.JdbcColumn
import saferis.jdbc.JdbcSession
import saferis.jdbc.JdbcSessionConfig
import saferis.jdbc.StandardJdbcAdapter

import org.sqlite.SQLiteErrorCode
import org.sqlite.SQLiteException
import zio.Chunk
import zio.URLayer

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Types
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.util.Locale
import java.util.UUID
import javax.sql.DataSource
import scala.util.control.NonFatal

/** SQLite over sqlite-jdbc. Pair it with `import saferis.sqlite.given` for the SQLite dialect.
  *
  * Every checkout turns on foreign keys, write-ahead logging, and a busy timeout. SQLite has one writer at a time, but
  * with WAL a reader never blocks a writer, and a writer that meets another waits up to the busy timeout instead of
  * failing at once. Checkouts are not serialized in the session: a stream that holds a connection while its rows write
  * through another would wait on itself forever.
  */
object SqliteJdbc:
  val adapter: JdbcAdapter = SqliteAdapter

  /** How long a writer waits for another before failing with a retryable `40001`. */
  val BusyTimeoutMillis = 5000

  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    JdbcSession.layer(
      adapter,
      config.copy(configure = conn =>
        pragmas(conn)
        config.configure(conn)),
    )

  private def pragmas(conn: Connection): Unit =
    val statement = conn.createStatement()
    try
      val _ = statement.execute("PRAGMA foreign_keys = ON")
      val _ = statement.execute("PRAGMA journal_mode = WAL")
      val _ = statement.execute(s"PRAGMA busy_timeout = $BusyTimeoutMillis")
    finally statement.close()
end SqliteJdbc

private object SqliteAdapter extends StandardJdbcAdapter:

  /** Values without a native SQLite storage class are ISO-8601 text, which also sorts in time order. A decimal is text,
    * so SQLite's `numeric` affinity decides how to store it.
    */
  override def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.Numeric(v)     => Right(ps.setString(index, v.bigDecimal.toPlainString))
      case SqlValue.Date(v)        => Right(ps.setString(index, v.toString))
      case SqlValue.Time(v)        => Right(ps.setString(index, v.toString))
      case SqlValue.Timestamp(v)   => Right(ps.setString(index, v.toString))
      case SqlValue.Timestamptz(v) => Right(ps.setString(index, v.toString))
      case other                   => super.bind(ps, index, other)

  /** A column reads by the type it was declared with (see `SQLiteDialect.columnType`). An expression has no declared
    * type, so it reads by the storage class of its value. SQLite integers are 64 bits, so every integer is `int8`; the
    * integer decoders narrow when the value fits.
    */
  override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    declared(column.typeName) match
      case "boolean" | "bool" => cell(SqlType.Bool, rs.getBoolean(i))(SqlValue.Bool(_))(rs)
      case "integer" | "int" | "smallint" | "bigint" | "tinyint" | "mediumint" =>
        cell(SqlType.Int8, rs.getLong(i))(SqlValue.Int8(_))(rs)
      // SQLite stores every `real` as 8 bytes, so reading 4 would drop a Double's precision.
      case "real" | "double" | "double precision" | "float" =>
        cell(SqlType.Float8, rs.getDouble(i))(SqlValue.Float8(_))(rs)
      case "numeric" | "decimal" => parsed(rs, column, SqlType.Numeric)(t => SqlValue.Numeric(BigDecimal(t)))
      case "varchar" | "char" | "character" | "nvarchar" =>
        ref(SqlType.VarChar, rs.getString(i))(SqlValue.VarChar(_))(rs)
      case "text" | "clob" => ref(SqlType.Text, rs.getString(i))(SqlValue.Text(_))(rs)
      case "blob"          => ref(SqlType.Bytea, rs.getBytes(i))(bytes => SqlValue.Bytea(Chunk.fromArray(bytes)))(rs)
      case "date"          => parsed(rs, column, SqlType.Date)(t => SqlValue.Date(LocalDate.parse(t)))
      case "time"          => parsed(rs, column, SqlType.Time)(t => SqlValue.Time(LocalTime.parse(t)))
      case "timestamp" | "datetime" =>
        parsed(rs, column, SqlType.Timestamp)(t => SqlValue.Timestamp(LocalDateTime.parse(t)))
      case "timestamptz"    => parsed(rs, column, SqlType.Timestamptz)(t => SqlValue.Timestamptz(Instant.parse(t)))
      case "json" | "jsonb" => ref(SqlType.Jsonb, rs.getString(i))(json => SqlValue.Jsonb(JsonText(json)))(rs)
      case "uuid"           => parsed(rs, column, SqlType.Uuid)(t => SqlValue.Uuid(UUID.fromString(t)))
      case _                => byStorageClass(rs, column)
    end match
  end read

  /** The declared name without its length or precision, `varchar(255)` to `varchar`. */
  private def declared(typeName: String): String =
    typeName.takeWhile(_ != '(').trim.toLowerCase(Locale.ROOT)

  private def byStorageClass(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.jdbcType match
      case Types.INTEGER | Types.BIGINT | Types.SMALLINT | Types.TINYINT =>
        cell(SqlType.Int8, rs.getLong(i))(SqlValue.Int8(_))(rs)
      case Types.REAL | Types.FLOAT | Types.DOUBLE => cell(SqlType.Float8, rs.getDouble(i))(SqlValue.Float8(_))(rs)
      case Types.VARCHAR | Types.CHAR              => ref(SqlType.Text, rs.getString(i))(SqlValue.Text(_))(rs)
      case Types.BLOB | Types.BINARY               =>
        ref(SqlType.Bytea, rs.getBytes(i))(bytes => SqlValue.Bytea(Chunk.fromArray(bytes)))(rs)
      case _ => other(rs, column)
  end byStorageClass

  /** Text that must parse. Text that does not is a decode failure naming the column, not a thrown exception. */
  private def parsed(rs: ResultSet, column: JdbcColumn, tpe: SqlType)(
      build: String => SqlValue
  ): Either[SaferisError, SqlValue] =
    Option(rs.getString(column.index)) match
      case None       => Right(SqlValue.Null(tpe))
      case Some(text) =>
        try Right(build(text))
        catch case NonFatal(e) => Left(SaferisError.DecodingError(column.label, column.typeName, e.getMessage))

  /** SQLite has result codes, not SQLSTATEs. Map the ones `SqlState.classify` names. */
  override def serverError(e: SQLException): ServerError =
    val base                = super.serverError(e)
    def as(state: SqlState) = base.copy(sqlState = Some(state))
    e match
      case sqlite: SQLiteException =>
        sqlite.getResultCode match
          case SQLiteErrorCode.SQLITE_CONSTRAINT_UNIQUE | SQLiteErrorCode.SQLITE_CONSTRAINT_PRIMARYKEY =>
            as(SqlState.UniqueViolation)
          case SQLiteErrorCode.SQLITE_CONSTRAINT_FOREIGNKEY                => as(SqlState.ForeignKeyViolation)
          case SQLiteErrorCode.SQLITE_CONSTRAINT_NOTNULL                   => as(SqlState.NotNullViolation)
          case SQLiteErrorCode.SQLITE_CONSTRAINT_CHECK                     => as(SqlState.CheckViolation)
          case SQLiteErrorCode.SQLITE_BUSY | SQLiteErrorCode.SQLITE_LOCKED => as(SqlState.SerializationFailure)
          case SQLiteErrorCode.SQLITE_INTERRUPT                            => as(SqlState.QueryCanceled)
          case SQLiteErrorCode.SQLITE_ERROR if base.message.contains("syntax error")   => as(SqlState.SyntaxError)
          case SQLiteErrorCode.SQLITE_ERROR if base.message.contains("no such table")  => as(SqlState.UndefinedTable)
          case SQLiteErrorCode.SQLITE_ERROR if base.message.contains("no such column") => as(SqlState.UndefinedColumn)
          case _                                                                       => base
      case _ => base
    end match
  end serverError
end SqliteAdapter
