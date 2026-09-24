package saferis

import zio.Chunk

import java.math.MathContext
import java.math.RoundingMode
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.UUID

/** A server type Saferis does not model. A driver may know a name, an OID, or both.
  *
  * The label is informational. JDBC reports `Named("mood")` and Node reports `Oid(16390)` for the same enum, and a user
  * enum's OID differs per database. Match text with [[Codec.pgEnum]], not this label. A later driver can report
  * [[ServerType.Both]] once it has resolved the OID.
  */
enum ServerType:
  case Named(name: String)
  case Oid(oid: Int)
  case Both(name: String, oid: Int)

/** Shared column type. Wire names avoid colliding with Scala and Java types. Each dialect renders its own spelling.
  * This is not a JDBC code.
  */
enum SqlType:
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
  case Array(element: SqlType)
  case Other(tpe: ServerType)
end SqlType

/** A bound value. Null carries its `SqlType` so the driver can bind a typed null. */
enum SqlValue:
  case Null(tpe: SqlType)
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
  case Array private[saferis] (element: SqlType, values: Chunk[SqlValue])
  case Other(tpe: ServerType, text: String)

  def sqlType: SqlType = this match
    case Null(tpe)         => tpe
    case Bool(_)           => SqlType.Bool
    case Int2(_)           => SqlType.Int2
    case Int4(_)           => SqlType.Int4
    case Int8(_)           => SqlType.Int8
    case Float4(_)         => SqlType.Float4
    case Float8(_)         => SqlType.Float8
    case Numeric(_)        => SqlType.Numeric
    case VarChar(_)        => SqlType.VarChar
    case Text(_)           => SqlType.Text
    case Bytea(_)          => SqlType.Bytea
    case Date(_)           => SqlType.Date
    case Time(_)           => SqlType.Time
    case Timestamp(_)      => SqlType.Timestamp
    case Timestamptz(_)    => SqlType.Timestamptz
    case Jsonb(_)          => SqlType.Jsonb
    case Uuid(_)           => SqlType.Uuid
    case Array(element, _) => SqlType.Array(element)
    case Other(tpe, _)     => SqlType.Other(tpe)
end SqlValue

object SqlValue:
  /** Every member is `element` or `Null(element)`, nested arrays included. A mismatch is not an array. */
  def array(element: SqlType, values: Chunk[SqlValue]): Either[String, SqlValue] =
    val candidate = Array(element, values)
    malformed(candidate).toLeft(candidate)

  /** Why an array value breaks the member rule, or `None`. Scalars are never malformed. */
  private[saferis] def malformed(value: SqlValue): Option[String] = value match
    case Array(element, values) =>
      val mismatch = values.indexWhere(member => !memberOf(element, member))
      if mismatch >= 0 then Some(s"element $mismatch is ${values(mismatch).sqlType}, expected $element")
      else values.iterator.map(malformed).collectFirst { case Some(detail) => detail }
    case _ => None

  private def memberOf(element: SqlType, value: SqlValue): Boolean = value match
    case Null(tpe) => tpe == element
    case other     => other.sqlType == element

  private val timeLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm:ss")

  private val timestampLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private val timestamptzLiteral: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS'Z'").withZone(ZoneOffset.UTC)

  /** Inlined form for `show` and DDL defaults. Not a bound parameter. */
  def literal(value: SqlValue): String = value match
    case Null(_)          => "null"
    case Bool(v)          => if v then "true" else "false"
    case Int2(v)          => v.toString
    case Int4(v)          => v.toString
    case Int8(v)          => v.toString
    case Float4(v)        => floatLiteral(v)
    case Float8(v)        => doubleLiteral(v)
    case Numeric(v)       => v.toString
    case VarChar(v)       => quote(v)
    case Text(v)          => quote(v)
    case Bytea(bytes)     => s"'\\x${hex(bytes)}'"
    case Date(v)          => s"DATE '${v}'"
    case Time(v)          => s"TIME '${v.format(timeLiteral)}'"
    case Timestamp(v)     => s"TIMESTAMP '${v.format(timestampLiteral)}'"
    case Timestamptz(v)   => s"TIMESTAMPTZ '${timestamptzLiteral.format(v)}'"
    case Jsonb(v)         => quote(v)
    case Uuid(v)          => quote(v.toString)
    case Array(_, values) => values.map(literal).mkString("ARRAY[", ", ", "]")
    case Other(_, text)   => quote(text)

  def quote(text: String): String =
    s"'${text.replace("'", "''")}'"

  private def quotedNonFinite(nan: Boolean, infinity: Boolean, positive: Boolean): Option[String] =
    if nan then Some("'NaN'")
    else if infinity then Some(if positive then "'Infinity'" else "'-Infinity'")
    else None

  private def floatLiteral(v: Float): String =
    quotedNonFinite(v.isNaN, v.isInfinity, v > 0.0f).getOrElse:
      if v == 0.0f then if 1.0f / v < 0.0f then "-0.0" else "0.0"
      else shortestFloat(v)

  private def doubleLiteral(v: Double): String =
    quotedNonFinite(v.isNaN, v.isInfinity, v > 0.0).getOrElse(v.toString)

  private def shortestFloat(v: Float): String =
    def attempt(sig: Int): String =
      val text =
        new java.math.BigDecimal(
          v.toDouble,
          new MathContext(sig, RoundingMode.HALF_EVEN),
        ).stripTrailingZeros.toPlainString
      if text.toFloat == v || sig == 9 then text
      else attempt(sig + 1)
    attempt(1)
  end shortestFloat

  private def hex(bytes: Chunk[Byte]): String =
    val digits = "0123456789abcdef"
    val sb     = new StringBuilder(bytes.length * 2)
    bytes.foreach: b =>
      val v = b & 0xff
      sb.append(digits.charAt(v >>> 4))
      sb.append(digits.charAt(v & 0x0f))
    sb.toString
end SqlValue
