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
  /** Connector/J streams rows only for this fetch size. A positive size is ignored without `useCursorFetch`. */
  override def cursor: CursorStrategy = CursorStrategy.Fetch(Integer.MIN_VALUE)

  /** The session is UTC ([[MySqlJdbc.layer]]), so an instant binds as its UTC wall time. */
  override def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.TimestampTz(v) => Right(ps.setObject(index, LocalDateTime.ofInstant(v, ZoneOffset.UTC)))
      case SqlValue.TimeTz(v)      => Right(ps.setString(index, v.toString))
      case other                   => super.bind(ps, index, other)

  override protected def jdbcType(tpe: SqlType): Int = tpe match
    case SqlType.TimestampTz => Types.TIMESTAMP
    case SqlType.TimeTz      => Types.VARCHAR
    case other               => super.jdbcType(other)

  override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.typeName match
      case "json"      => ref(SqlType.Json, rs.getString(i))(json => SqlValue.Json(JsonText(json)))(rs)
      case "timestamp" =>
        ref(SqlType.TimestampTz, rs.getObject(i, classOf[LocalDateTime]))(v =>
          SqlValue.TimestampTz(v.toInstant(ZoneOffset.UTC))
        )(rs)
      case "year" => cell(SqlType.SmallInt, rs.getShort(i))(SqlValue.SmallInt(_))(rs)
      case "tinyint unsigned" | "smallint unsigned" | "mediumint unsigned" =>
        cell(SqlType.Integer, rs.getInt(i))(SqlValue.Integer(_))(rs)
      case "int unsigned" | "integer unsigned" => cell(SqlType.BigInt, rs.getLong(i))(SqlValue.BigInt(_))(rs)
      case "bigint unsigned"                   =>
        ref(SqlType.Numeric, rs.getBigDecimal(i))(v => SqlValue.Numeric(BigDecimal(v)))(rs)
      case _ => super.read(rs, column)
    end match
  end read

  /** MySQL reports most integrity failures as SQLSTATE `23000`. The errno says which one. The SQLSTATE stays. */
  override def serverError(e: SQLException): ServerError =
    val base      = super.serverError(e)
    val condition = e.getErrorCode match
      case 1062        => SqlCondition.Unique(None)
      case 1451 | 1452 => SqlCondition.ForeignKey(None)
      case 1048        => SqlCondition.NotNull
      case 3819        => SqlCondition.Check(None)
      case 1213        => SqlCondition.Deadlock
      case 3024        => SqlCondition.Canceled
      case _           => base.condition
    base.copy(condition = condition)
  end serverError
end MySqlAdapter
