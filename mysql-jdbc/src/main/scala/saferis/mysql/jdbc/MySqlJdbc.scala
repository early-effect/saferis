package saferis.mysql.jdbc

import saferis.*
import saferis.jdbc.CursorStrategy
import saferis.jdbc.JdbcAdapter
import saferis.jdbc.JdbcColumn
import saferis.jdbc.JdbcSession
import saferis.jdbc.JdbcSessionConfig
import saferis.jdbc.StandardJdbcAdapter

import zio.URLayer

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Types
import java.time.LocalDateTime
import java.time.ZoneOffset
import javax.sql.DataSource
import scala.util.matching.Regex

/** MySQL over Connector/J. Pair it with `import saferis.mysql.given`, which brings the MySQL dialect and the `char(36)`
  * UUID codecs.
  *
  * Streams read row by row (`setFetchSize(Integer.MIN_VALUE)`, Connector/J's streaming mode). While a stream is open,
  * its connection cannot run another statement, so inside `transact` finish the stream before the next statement.
  */
object MySqlJdbc:
  val adapter: JdbcAdapter = MySqlAdapter

  /** Every checkout sets the session time zone to UTC before `config.configure` runs, so a `timestamp` column stores
    * and returns UTC and an `Instant` round-trips whatever zone the JVM or the server is in.
    */
  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    JdbcSession.layer(
      adapter,
      config.copy(configure = conn =>
        utc(conn)
        config.configure(conn)),
    )

  private def utc(conn: Connection): Unit =
    val statement = conn.createStatement()
    try
      val _ = statement.execute("SET time_zone = '+00:00'")
    finally statement.close()
end MySqlJdbc

private object MySqlAdapter extends StandardJdbcAdapter:
  private val duplicateKey = """for key '([^']+)'""".r
  private val foreignKey   = """CONSTRAINT `([^`]+)`""".r
  private val check        = """[Cc]heck constraint '([^']+)'""".r

  /** Connector/J streams rows only for this fetch size. A positive size is ignored without `useCursorFetch`. */
  override def cursor: CursorStrategy = CursorStrategy.Fetch(Integer.MIN_VALUE)

  /** The session is UTC ([[MySqlJdbc.layer]]), so an instant binds as its UTC wall time. */
  override def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.Timestamptz(v) => Right(ps.setObject(index, LocalDateTime.ofInstant(v, ZoneOffset.UTC)))
      case other                   => super.bind(ps, index, other)

  override protected def jdbcType(tpe: SqlType): Int = tpe match
    case SqlType.Timestamptz => Types.TIMESTAMP
    case other               => super.jdbcType(other)

  override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.typeName match
      case "json"      => ref(SqlType.Jsonb, rs.getString(i))(json => SqlValue.Jsonb(JsonText(json)))(rs)
      case "timestamp" =>
        ref(SqlType.Timestamptz, rs.getObject(i, classOf[LocalDateTime]))(v =>
          SqlValue.Timestamptz(v.toInstant(ZoneOffset.UTC))
        )(rs)
      case "year" => cell(SqlType.Int2, rs.getShort(i))(SqlValue.Int2(_))(rs)
      case "tinyint unsigned" | "smallint unsigned" | "mediumint unsigned" =>
        cell(SqlType.Int4, rs.getInt(i))(SqlValue.Int4(_))(rs)
      case "int unsigned" | "integer unsigned" => cell(SqlType.Int8, rs.getLong(i))(SqlValue.Int8(_))(rs)
      case "bigint unsigned"                   =>
        ref(SqlType.Numeric, rs.getBigDecimal(i))(v => SqlValue.Numeric(BigDecimal(v)))(rs)
      case _ => super.read(rs, column)
    end match
  end read

  /** MySQL reports most integrity failures as SQLSTATE `23000`. The vendor code says which one. */
  override def serverError(e: SQLException): ServerError =
    val base                                                           = super.serverError(e)
    def as(state: SqlState, constraint: Option[ConstraintName] = None) =
      base.copy(sqlState = Some(state), constraint = constraint)
    e.getErrorCode match
      case 1062        => as(SqlState.UniqueViolation, first(duplicateKey, base.message).map(unqualified))
      case 1451 | 1452 => as(SqlState.ForeignKeyViolation, first(foreignKey, base.message))
      case 1048        => as(SqlState.NotNullViolation)
      case 3819        => as(SqlState.CheckViolation, first(check, base.message))
      case 1213        => as(SqlState.Deadlock)
      case 3024        => as(SqlState.QueryCanceled)
      case _           => base
  end serverError

  private def first(pattern: Regex, message: String): Option[ConstraintName] =
    pattern.findFirstMatchIn(message).map(m => ConstraintName(m.group(1)))

  /** MySQL 8 names the key `table.key`. */
  private def unqualified(key: ConstraintName): ConstraintName =
    ConstraintName(key.substring(key.lastIndexOf('.') + 1))
end MySqlAdapter
