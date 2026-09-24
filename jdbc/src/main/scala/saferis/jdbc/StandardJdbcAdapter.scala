package saferis.jdbc

import saferis.*

import zio.Chunk

import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Types
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZoneOffset

/** Bind and read through what `java.sql` specifies (JDBC 4.2 `java.time` included), and read columns by their
  * `java.sql.Types` code. Arrays are not portable, so they are `Unsupported` unless an adapter's `bind` handles them.
  *
  * {{{
  *   object MyAdapter extends StandardJdbcAdapter:
  *     override def serverError(e: SQLException): ServerError = ...
  * }}}
  */
trait StandardJdbcAdapter extends JdbcAdapter:

  def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit] =
    value match
      case SqlValue.Null(tpe)      => Right(ps.setNull(index, jdbcType(tpe)))
      case SqlValue.Bool(v)        => Right(ps.setBoolean(index, v))
      case SqlValue.Int2(v)        => Right(ps.setShort(index, v))
      case SqlValue.Int4(v)        => Right(ps.setInt(index, v))
      case SqlValue.Int8(v)        => Right(ps.setLong(index, v))
      case SqlValue.Float4(v)      => Right(ps.setFloat(index, v))
      case SqlValue.Float8(v)      => Right(ps.setDouble(index, v))
      case SqlValue.Numeric(v)     => Right(ps.setBigDecimal(index, v.bigDecimal))
      case SqlValue.VarChar(v)     => Right(ps.setString(index, v))
      case SqlValue.Text(v)        => Right(ps.setString(index, v))
      case SqlValue.Bytea(v)       => Right(ps.setBytes(index, v.toArray))
      case SqlValue.Date(v)        => Right(ps.setObject(index, v))
      case SqlValue.Time(v)        => Right(ps.setObject(index, v))
      case SqlValue.Timestamp(v)   => Right(ps.setObject(index, v))
      case SqlValue.Timestamptz(v) => Right(ps.setObject(index, OffsetDateTime.ofInstant(v, ZoneOffset.UTC)))
      case SqlValue.Jsonb(v)       => Right(ps.setString(index, v))
      case SqlValue.Uuid(v)        => Right(ps.setString(index, v.toString))
      case SqlValue.Other(_, text) => Right(ps.setString(index, text))
      // No portable array binding exists. An adapter with arrays handles this case before delegating here.
      case SqlValue.Array(element, _) =>
        Left(SaferisError.Unsupported(s"array parameters of $element; use in(...) on this database"))

  def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val i = column.index
    column.jdbcType match
      case Types.BOOLEAN | Types.BIT      => cell(SqlType.Bool, rs.getBoolean(i))(SqlValue.Bool(_))(rs)
      case Types.TINYINT | Types.SMALLINT => cell(SqlType.Int2, rs.getShort(i))(SqlValue.Int2(_))(rs)
      case Types.INTEGER                  => cell(SqlType.Int4, rs.getInt(i))(SqlValue.Int4(_))(rs)
      case Types.BIGINT                   => cell(SqlType.Int8, rs.getLong(i))(SqlValue.Int8(_))(rs)
      case Types.REAL                     => cell(SqlType.Float4, rs.getFloat(i))(SqlValue.Float4(_))(rs)
      case Types.FLOAT | Types.DOUBLE     => cell(SqlType.Float8, rs.getDouble(i))(SqlValue.Float8(_))(rs)
      case Types.NUMERIC | Types.DECIMAL  =>
        ref(SqlType.Numeric, rs.getBigDecimal(i))(v => SqlValue.Numeric(BigDecimal(v)))(rs)
      case Types.CHAR | Types.VARCHAR | Types.NCHAR | Types.NVARCHAR =>
        ref(SqlType.VarChar, rs.getString(i))(SqlValue.VarChar(_))(rs)
      case Types.LONGVARCHAR | Types.LONGNVARCHAR | Types.CLOB | Types.NCLOB =>
        ref(SqlType.Text, rs.getString(i))(SqlValue.Text(_))(rs)
      case Types.BINARY | Types.VARBINARY | Types.LONGVARBINARY | Types.BLOB =>
        ref(SqlType.Bytea, rs.getBytes(i))(bytes => SqlValue.Bytea(Chunk.fromArray(bytes)))(rs)
      case Types.DATE      => ref(SqlType.Date, rs.getObject(i, classOf[LocalDate]))(SqlValue.Date(_))(rs)
      case Types.TIME      => ref(SqlType.Time, rs.getObject(i, classOf[LocalTime]))(SqlValue.Time(_))(rs)
      case Types.TIMESTAMP =>
        ref(SqlType.Timestamp, rs.getObject(i, classOf[LocalDateTime]))(SqlValue.Timestamp(_))(rs)
      case Types.TIMESTAMP_WITH_TIMEZONE =>
        ref(SqlType.Timestamptz, rs.getObject(i, classOf[OffsetDateTime]))(v => SqlValue.Timestamptz(v.toInstant))(rs)
      case Types.TIME_WITH_TIMEZONE =>
        ref(SqlType.Time, rs.getObject(i, classOf[OffsetTime]))(v => SqlValue.Time(v.toLocalTime))(rs)
      case _ => other(rs, column)
    end match
  end read

  /** A column this adapter does not model: its text, labeled with the driver's type name. */
  protected def other(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
    val tpe = SqlType.Other(ServerType.Named(column.typeName))
    ref(tpe, rs.getString(column.index))(text => SqlValue.Other(ServerType.Named(column.typeName), text))(rs)

  def serverError(e: SQLException): ServerError =
    ServerError(Option(e.getSQLState), StandardJdbcAdapter.messageOf(e), None, Some(e.getErrorCode))

  /** The `java.sql.Types` code for a typed null. */
  protected def jdbcType(tpe: SqlType): Int = tpe match
    case SqlType.Bool        => Types.BOOLEAN
    case SqlType.Int2        => Types.SMALLINT
    case SqlType.Int4        => Types.INTEGER
    case SqlType.Int8        => Types.BIGINT
    case SqlType.Float4      => Types.REAL
    case SqlType.Float8      => Types.DOUBLE
    case SqlType.Numeric     => Types.NUMERIC
    case SqlType.VarChar     => Types.VARCHAR
    case SqlType.Text        => Types.LONGVARCHAR
    case SqlType.Bytea       => Types.VARBINARY
    case SqlType.Date        => Types.DATE
    case SqlType.Time        => Types.TIME
    case SqlType.Timestamp   => Types.TIMESTAMP
    case SqlType.Timestamptz => Types.TIMESTAMP_WITH_TIMEZONE
    case SqlType.Jsonb       => Types.VARCHAR
    case SqlType.Uuid        => Types.VARCHAR
    case SqlType.Array(_)    => Types.ARRAY
    case SqlType.Other(_)    => Types.OTHER

  /** A primitive getter returns a default on SQL null, so `wasNull` decides. */
  protected def cell[A](tpe: SqlType, value: A)(wrap: A => SqlValue)(rs: ResultSet): Either[SaferisError, SqlValue] =
    Right(if rs.wasNull() then SqlValue.Null(tpe) else wrap(value))

  /** A reference getter returns `null` on SQL null. The null stays inside this check. */
  protected def ref[A <: AnyRef](tpe: SqlType, value: A)(wrap: A => SqlValue)(
      rs: ResultSet
  ): Either[SaferisError, SqlValue] =
    Right(if value == null || rs.wasNull() then SqlValue.Null(tpe) else wrap(value))
end StandardJdbcAdapter

object StandardJdbcAdapter:
  /** The message, or the exception's class name when the driver gives none. */
  def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
