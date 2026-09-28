package saferis.h2.jdbc

import saferis.*
import saferis.h2.H2Dialect
import saferis.jdbc.JdbcAdapter
import saferis.jdbc.JdbcColumn
import saferis.jdbc.JdbcSession
import saferis.jdbc.JdbcSessionConfig
import saferis.jdbc.StandardJdbcAdapter

import org.h2.jdbcx.JdbcDataSource
import zio.ULayer
import zio.URLayer
import zio.ZLayer

import java.nio.charset.StandardCharsets
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.util.UUID
import javax.sql.DataSource

/** H2 over its own JDBC driver. Pair it with `import saferis.h2.given` for the H2 dialect. */
object H2Jdbc:
  val adapter: JdbcAdapter = H2Adapter

  def layer(config: JdbcSessionConfig = JdbcSessionConfig()): URLayer[DataSource, SqlSession] =
    JdbcSession.layer(adapter, config)

  /** A named in-memory database that lives until the JVM exits. `DATABASE_TO_LOWER` folds unquoted names to lower case,
    * as Postgres does, so DDL that quotes a name finds the table DDL created without quotes.
    */
  def memory(name: String): ULayer[DataSource] =
    ZLayer.succeed:
      val ds = JdbcDataSource()
      ds.setURL(s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1;DATABASE_TO_LOWER=TRUE")
      ds
end H2Jdbc

private object H2Adapter extends StandardJdbcAdapter:

  /** H2 reads UTF-8 bytes bound to a `json` column as JSON text. A bound string would become a JSON string. */
  override def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.Json(json) => Right(ps.setBytes(index, json.getBytes(StandardCharsets.UTF_8)))
      case SqlValue.Uuid(uuid) => Right(ps.setObject(index, uuid))
      case SqlValue.Numeric(value) if !H2Dialect.fitsNumeric(value) =>
        Left(SaferisError.EncodingError(index, s"$value is outside numeric(100000, 50000)"))
      case other => super.bind(ps, index, other)

  override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.typeName match
      case "json" =>
        ref(SqlType.Json, rs.getBytes(i))(bytes => SqlValue.Json(JsonText(String(bytes, StandardCharsets.UTF_8))))(rs)
      case "uuid" => ref(SqlType.Uuid, rs.getObject(i, classOf[UUID]))(SqlValue.Uuid(_))(rs)
      case _      => super.read(rs, column)
end H2Adapter
