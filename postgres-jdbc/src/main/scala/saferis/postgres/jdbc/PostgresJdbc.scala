package saferis.postgres.jdbc

import saferis.*
import saferis.jdbc.CursorStrategy
import saferis.jdbc.JdbcAdapter
import saferis.jdbc.JdbcSession
import saferis.jdbc.JdbcSessionConfig
import saferis.postgres.PgText

import org.postgresql.core.TransactionState
import org.postgresql.jdbc.PgConnection
import org.postgresql.util.PGobject
import org.postgresql.util.PSQLException
import zio.Chunk
import zio.URLayer

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZoneOffset
import java.util.UUID
import javax.sql.DataSource

/** Postgres over JDBC. The only JVM module that depends on pgjdbc. */
object PostgresJdbc:
  val adapter: JdbcAdapter = PostgresAdapter

  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    JdbcSession.layer(adapter, config)

private object PostgresAdapter extends JdbcAdapter:
  private val FetchSize = 256

  def cursor: CursorStrategy = CursorStrategy.Fetch(FetchSize, inTransaction = true)

  def serverError(e: SQLException): ServerError =
    ServerError(
      Option(e.getSQLState),
      messageOf(e),
      constraintOf(e),
      Some(e.getErrorCode),
    )

  def commitRejected(conn: Connection): Option[ServerError] =
    val state = conn.unwrap(classOf[PgConnection]).getTransactionState
    if state == TransactionState.FAILED then
      Some(
        ServerError(
          Some("25P02"),
          "current transaction is aborted, commands ignored until end of transaction block",
        )
      )
    else None
  end commitRejected

  def bind(ps: PreparedStatement, index: Int, value: SqlValue): Unit =
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
      // pgjdbc treats Types.OTHER as an unspecified type and lets the server infer. Other drivers do not.
      case SqlValue.Other(_, text)         => ps.setObject(index, text, java.sql.Types.OTHER)
      case SqlValue.Array(element, values) =>
        val members = values.map(arrayMember).toArray
        val array   = ps.getConnection.createArrayOf(arrayTypeName(element), members)
        ps.setArray(index, array)

  def read(rs: ResultSet, index: Int, typeName: String): Either[SaferisError, SqlValue] =
    def nulled(tpe: SqlType): SqlValue = SqlValue.Null(tpe)
    typeName match
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
      case array if array.startsWith("_") || array.endsWith("[]") =>
        readArray(rs, index, array)
      case _ =>
        val text = rs.getString(index)
        Right:
          if rs.wasNull() || text == null then nulled(SqlType.Other(ServerType.Named(typeName)))
          else SqlValue.Other(ServerType.Named(typeName), text)
    end match
  end read

  private def readArray(rs: ResultSet, index: Int, typeName: String): Either[SaferisError, SqlValue] =
    val elementName =
      if typeName.endsWith("[]") then typeName.stripSuffix("[]") else typeName.drop(1)
    val decoded =
      for
        element <- scalarOid(elementName)
        array   <- PgText.arrayOid(element)
      yield
        val text = rs.getString(index)
        if rs.wasNull() || text == null then
          Right(SqlValue.Null(PgText.sqlType(array).getOrElse(SqlType.Array(SqlType.Other(ServerType.Oid(element))))))
        else PgText.decode(array, text).left.map(detail => SaferisError.DecodingError(typeName, typeName, detail))
    decoded.getOrElse:
      val text = rs.getString(index)
      Right:
        if rs.wasNull() || text == null then SqlValue.Null(SqlType.Other(ServerType.Named(typeName)))
        else SqlValue.Other(ServerType.Named(typeName), text)
  end readArray

  private def scalarOid(name: String): Option[Int] = name match
    case "bool" | "boolean"                  => Some(16)
    case "bytea"                             => Some(17)
    case "char"                              => Some(18)
    case "name"                              => Some(19)
    case "int8" | "bigint" | "bigserial"     => Some(20)
    case "int2" | "smallint" | "smallserial" => Some(21)
    case "int4" | "integer" | "serial"       => Some(23)
    case "text"                              => Some(25)
    case "float4" | "real"                   => Some(700)
    case "float8" | "double precision"       => Some(701)
    case "bpchar"                            => Some(1042)
    case "varchar"                           => Some(1043)
    case "date"                              => Some(1082)
    case "time"                              => Some(1083)
    case "timestamp"                         => Some(1114)
    case "timestamptz"                       => Some(1184)
    case "timetz"                            => Some(1266)
    case "numeric"                           => Some(1700)
    case "uuid"                              => Some(2950)
    case "json"                              => Some(114)
    case "jsonb"                             => Some(3802)
    case _                                   => None

  private def jdbcType(tpe: SqlType): Int = tpe match
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

  private def arrayTypeName(tpe: SqlType): String =
    PgText
      .cast(tpe)
      .getOrElse:
        tpe match
          case SqlType.Other(ServerType.Named(name))   => name
          case SqlType.Other(ServerType.Both(name, _)) => name
          case SqlType.Other(ServerType.Oid(oid))      => oid.toString
          case other                                   => other.toString

  /** SQL null is a Java null. `createArrayOf` has no other way to say that. */
  private def arrayMember(value: SqlValue): Object =
    PgText.encode(value).orNull

  private def constraintOf(e: SQLException): Option[String] = e match
    case pg: PSQLException =>
      Option(pg.getServerErrorMessage).flatMap(message => Option(message.getConstraint))
    case _ => None

  private def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end PostgresAdapter
