package saferis

import zio.Chunk

import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.util.UUID

/** Postgres type identity. Dialects render one spelling each. This is not a JDBC code. */
enum PgType:
  case Bool
  case Int2
  case Int4
  case Int8
  case Float4
  case Float8
  case Numeric
  case VarChar
  case Text
  case Bytea
  case Date
  case Time
  case Timestamp
  case Timestamptz
  case Jsonb
  case Uuid
end PgType

/** A bound value. Null carries the Postgres type so the driver can bind a typed null. */
enum SqlValue:
  case Null(tpe: PgType)
  case Bool(value: Boolean)
  case Int2(value: Short)
  case Int4(value: Int)
  case Int8(value: Long)
  case Float4(value: Float)
  case Float8(value: Double)
  case Numeric(value: BigDecimal)
  case VarChar(value: String)
  case Text(value: String)
  case Bytea(value: Chunk[Byte])
  case Date(value: LocalDate)
  case Time(value: LocalTime)
  case Timestamp(value: LocalDateTime)
  case Timestamptz(value: java.time.Instant)
  case Jsonb(value: String)
  case Uuid(value: UUID)

  def pgType: PgType = this match
    case Null(tpe)      => tpe
    case Bool(_)        => PgType.Bool
    case Int2(_)        => PgType.Int2
    case Int4(_)        => PgType.Int4
    case Int8(_)        => PgType.Int8
    case Float4(_)      => PgType.Float4
    case Float8(_)      => PgType.Float8
    case Numeric(_)     => PgType.Numeric
    case VarChar(_)     => PgType.VarChar
    case Text(_)        => PgType.Text
    case Bytea(_)       => PgType.Bytea
    case Date(_)        => PgType.Date
    case Time(_)        => PgType.Time
    case Timestamp(_)   => PgType.Timestamp
    case Timestamptz(_) => PgType.Timestamptz
    case Jsonb(_)       => PgType.Jsonb
    case Uuid(_)        => PgType.Uuid
end SqlValue

object SqlValue:
  private val timeLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm:ss")

  private val timestampLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private val timestamptzLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS'Z'").withZone(ZoneOffset.UTC)

  /** Inlined form for `show` and DDL defaults. Not a bound parameter. */
  def literal(value: SqlValue): String = value match
    case Null(_)        => "null"
    case Bool(v)        => if v then "true" else "false"
    case Int2(v)        => v.toString
    case Int4(v)        => v.toString
    case Int8(v)        => v.toString
    case Float4(v)      => v.toString
    case Float8(v)      => v.toString
    case Numeric(v)     => v.toString
    case VarChar(v)     => quote(v)
    case Text(v)        => quote(v)
    case Bytea(bytes)   => s"'\\x${hex(bytes)}'"
    case Date(v)        => s"DATE '${v}'"
    case Time(v)        => s"TIME '${v.format(timeLiteral)}'"
    case Timestamp(v)   => s"TIMESTAMP '${v.format(timestampLiteral)}'"
    case Timestamptz(v) => s"TIMESTAMPTZ '${timestamptzLiteral.format(v)}'"
    case Jsonb(v)       => quote(v)
    case Uuid(v)        => quote(v.toString)

  def quote(text: String): String =
    s"'${text.replace("'", "''")}'"

  private def hex(bytes: Chunk[Byte]): String =
    val digits = "0123456789abcdef"
    val sb     = new StringBuilder(bytes.length * 2)
    bytes.foreach: b =>
      val v = b & 0xff
      sb.append(digits.charAt(v >>> 4))
      sb.append(digits.charAt(v & 0x0f))
    sb.toString

  def utcOffset(instant: java.time.Instant): OffsetDateTime =
    OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)

  def utcZoned(instant: java.time.Instant): ZonedDateTime =
    ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
end SqlValue
