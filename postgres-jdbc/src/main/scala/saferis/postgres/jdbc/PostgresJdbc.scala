package saferis.postgres.jdbc

import saferis.*
import saferis.jdbc.CursorStrategy
import saferis.jdbc.JdbcAdapter
import saferis.jdbc.JdbcColumn
import saferis.jdbc.JdbcSession
import saferis.jdbc.JdbcSessionConfig
import saferis.jdbc.StandardJdbcAdapter
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
import java.sql.Types
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.util.UUID
import javax.sql.DataSource

/** Postgres over JDBC. The only JVM module that depends on pgjdbc. */
object PostgresJdbc:
  val adapter: JdbcAdapter = PostgresAdapter

  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    JdbcSession.layer(adapter, config)

private object PostgresAdapter extends StandardJdbcAdapter:
  private val FetchSize = 256

  override def cursor: CursorStrategy = CursorStrategy.FetchInTransaction(FetchSize)

  override def serverError(e: SQLException): ServerError =
    super.serverError(e).copy(constraint = constraintOf(e))

  /** A proxy that cannot unwrap to pgjdbc skips the early check. The server still rolls the transaction back. */
  override def commitRejected(conn: Connection): Option[ServerError] =
    if !conn.isWrapperFor(classOf[PgConnection]) then None
    else if conn.unwrap(classOf[PgConnection]).getTransactionState != TransactionState.FAILED then None
    else
      Some(
        ServerError(
          Some("25P02"),
          "current transaction is aborted, commands ignored until end of transaction block",
        )
      )

  override def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.Jsonb(json) =>
        val obj = new PGobject()
        obj.setType("jsonb")
        obj.setValue(json)
        Right(ps.setObject(index, obj))
      case SqlValue.Uuid(uuid) => Right(ps.setObject(index, uuid))
      // pgjdbc treats Types.OTHER as an unspecified type and lets the server infer. Other drivers do not.
      case SqlValue.Other(_, text)         => Right(ps.setObject(index, text, Types.OTHER))
      case SqlValue.Array(element, values) =>
        arrayTypeName(element).map: name =>
          ps.setArray(index, ps.getConnection.createArrayOf(name, values.map(arrayMember).toArray))
      case other => super.bind(ps, index, other)

  override protected def jdbcType(tpe: SqlType): Int = tpe match
    case SqlType.Bytea                => Types.BINARY
    case SqlType.Jsonb | SqlType.Uuid => Types.OTHER
    case other                        => super.jdbcType(other)

  override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.typeName match
      case "bool"                              => cell(SqlType.Bool, rs.getBoolean(i))(SqlValue.Bool(_))(rs)
      case "int2" | "smallint" | "smallserial" => cell(SqlType.Int2, rs.getShort(i))(SqlValue.Int2(_))(rs)
      case "int4" | "integer" | "serial"       => cell(SqlType.Int4, rs.getInt(i))(SqlValue.Int4(_))(rs)
      case "int8" | "bigint" | "bigserial"     => cell(SqlType.Int8, rs.getLong(i))(SqlValue.Int8(_))(rs)
      case "float4"                            => cell(SqlType.Float4, rs.getFloat(i))(SqlValue.Float4(_))(rs)
      case "float8"                            => cell(SqlType.Float8, rs.getDouble(i))(SqlValue.Float8(_))(rs)
      case "numeric"                           =>
        ref(SqlType.Numeric, rs.getBigDecimal(i))(v => SqlValue.Numeric(BigDecimal(v)))(rs)
      case "varchar" | "bpchar" | "name" => ref(SqlType.VarChar, rs.getString(i))(SqlValue.VarChar(_))(rs)
      case "text" | "unknown"            => ref(SqlType.Text, rs.getString(i))(SqlValue.Text(_))(rs)
      case "bytea"                       =>
        ref(SqlType.Bytea, rs.getBytes(i))(bytes => SqlValue.Bytea(Chunk.fromArray(bytes)))(rs)
      case "date"   => ref(SqlType.Date, rs.getObject(i, classOf[LocalDate]))(SqlValue.Date(_))(rs)
      case "time"   => ref(SqlType.Time, rs.getObject(i, classOf[LocalTime]))(SqlValue.Time(_))(rs)
      case "timetz" =>
        ref(SqlType.Time, rs.getObject(i, classOf[OffsetTime]))(v => SqlValue.Time(v.toLocalTime))(rs)
      case "timestamp" =>
        ref(SqlType.Timestamp, rs.getObject(i, classOf[LocalDateTime]))(SqlValue.Timestamp(_))(rs)
      case "timestamptz" =>
        ref(SqlType.Timestamptz, rs.getObject(i, classOf[OffsetDateTime]))(v => SqlValue.Timestamptz(v.toInstant))(rs)
      case "json" | "jsonb" =>
        ref(SqlType.Jsonb, rs.getObject(i))(value =>
          value match
            case pg: PGobject => SqlValue.Jsonb(Option(pg.getValue).getOrElse(""))
            case other        => SqlValue.Jsonb(other.toString)
        )(rs)
      case "uuid" => ref(SqlType.Uuid, rs.getObject(i, classOf[UUID]))(SqlValue.Uuid(_))(rs)
      case array if array.startsWith("_") || array.endsWith("[]") => readArray(rs, column)
      case _                                                      => other(rs, column)
    end match
  end read

  /** Array text through `PgText`, keyed by the element's OID. An element type `PgText` does not know is `Other`. */
  private def readArray(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val name        = column.typeName
    val elementName = if name.endsWith("[]") then name.stripSuffix("[]") else name.drop(1)
    val arrayOid    = PgText.oidOf(elementName).flatMap(PgText.arrayOid)
    arrayOid match
      case None      => other(rs, column)
      case Some(oid) =>
        val tpe = PgText.sqlType(oid).getOrElse(SqlType.Other(ServerType.Named(name)))
        Option(rs.getString(column.index)) match
          case None       => Right(SqlValue.Null(tpe))
          case Some(text) =>
            PgText.decode(oid, text).left.map(detail => SaferisError.DecodingError(column.label, name, detail))
  end readArray

  /** `createArrayOf` needs a name the server resolves. An array of a type known only by OID has none. */
  private def arrayTypeName(tpe: SqlType): Either[SaferisError, String] =
    PgText.cast(tpe) match
      case Some(name) => Right(name)
      case None       =>
        tpe match
          case SqlType.Other(ServerType.Named(name))   => Right(name)
          case SqlType.Other(ServerType.Both(name, _)) => Right(name)
          case other => Left(SaferisError.Unsupported(s"array of $other: the server type has no name to bind"))

  /** SQL null is a Java null. `createArrayOf` has no other way to say that. */
  private def arrayMember(value: SqlValue): Object =
    PgText.encode(value).orNull

  private def constraintOf(e: SQLException): Option[String] = e match
    case pg: PSQLException =>
      Option(pg.getServerErrorMessage).flatMap(message => Option(message.getConstraint))
    case _ => None
end PostgresAdapter
