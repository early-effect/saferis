package saferis

import zio.Chunk

import java.math.MathContext
import java.math.RoundingMode
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetTime
import java.time.ZoneOffset
import java.util.UUID

/** A server type Saferis does not model. A driver may know a name, an OID, or both.
  *
  * The label is informational. JDBC reports `Named("mood")` and Node reports `Oid(16390)` for the same enum, and a user
  * enum's OID differs per database. Match text with [[Codec.enumeration]], not this label. A later driver can report
  * [[ServerType.Both]] once it has resolved the OID.
  */
enum ServerType:
  case Named(name: TypeName)
  case Oid(oid: Int)
  case Both(name: TypeName, oid: Int)

/** Shared column type. A dialect spells it (`bytea`, `jsonb`, `int2`, `timetz`). This is not a JDBC code. */
enum SqlType:
  case Bool
  case SmallInt
  case Integer
  case BigInt
  case Real
  case DoublePrecision
  case Numeric
  case VarChar
  case Text
  case Binary
  case Date
  case Time
  case TimeTz
  case Timestamp
  case TimestampTz
  case Json
  case Uuid
  case Array(element: SqlType)
  case Other(tpe: ServerType)
end SqlType

/** A bound value. Null carries its `SqlType` so the driver can bind a typed null. */
enum SqlValue:
  case Null(tpe: SqlType)
  case Bool(value: Boolean)
  case SmallInt(value: Short)
  case Integer(value: Int)
  case BigInt(value: Long)
  case Real(value: Float)
  case DoublePrecision(value: Double)
  case Numeric(value: BigDecimal)
  case VarChar(value: String)
  case Text(value: String)
  case Binary(value: Chunk[Byte])
  case Date(value: LocalDate)
  case Time(value: LocalTime)
  case TimeTz(value: OffsetTime)
  case Timestamp(value: LocalDateTime)
  case TimestampTz(value: java.time.Instant)
  case Json(value: JsonText)
  case Uuid(value: UUID)
  case Array private[saferis] (element: SqlType, values: Chunk[SqlValue])
  case Other(tpe: ServerType, text: String)

  def sqlType: SqlType = this match
    case Null(tpe)          => tpe
    case Bool(_)            => SqlType.Bool
    case SmallInt(_)        => SqlType.SmallInt
    case Integer(_)         => SqlType.Integer
    case BigInt(_)          => SqlType.BigInt
    case Real(_)            => SqlType.Real
    case DoublePrecision(_) => SqlType.DoublePrecision
    case Numeric(_)         => SqlType.Numeric
    case VarChar(_)         => SqlType.VarChar
    case Text(_)            => SqlType.Text
    case Binary(_)          => SqlType.Binary
    case Date(_)            => SqlType.Date
    case Time(_)            => SqlType.Time
    case TimeTz(_)          => SqlType.TimeTz
    case Timestamp(_)       => SqlType.Timestamp
    case TimestampTz(_)     => SqlType.TimestampTz
    case Json(_)            => SqlType.Json
    case Uuid(_)            => SqlType.Uuid
    case Array(element, _)  => SqlType.Array(element)
    case Other(tpe, _)      => SqlType.Other(tpe)
end SqlValue

object SqlValue:
  /** Every member is `element` or `Null(element)`, nested arrays included. A mismatch is not an array. */
  def array(element: SqlType, values: Chunk[SqlValue]): SqlValue =
    Array(element, values)

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
  def literal(value: SqlValue): SqlText = SqlText:
    value match
      case Null(_)            => "null"
      case Bool(v)            => if v then "true" else "false"
      case SmallInt(v)        => v.toString
      case Integer(v)         => v.toString
      case BigInt(v)          => v.toString
      case Real(v)            => floatLiteral(v)
      case DoublePrecision(v) => doubleLiteral(v)
      case Numeric(v)         => v.toString
      case VarChar(v)         => quote(v)
      case Text(v)            => quote(v)
      case Binary(bytes)      => s"'\\x${hex(bytes)}'"
      case Date(v)            => s"DATE '${v}'"
      case Time(v)            => s"TIME '${v.format(timeLiteral)}'"
      case TimeTz(v)          => s"TIMETZ '${v.format(DateTimeFormatter.ISO_OFFSET_TIME)}'"
      case Timestamp(v)       => s"TIMESTAMP '${v.format(timestampLiteral)}'"
      case TimestampTz(v)     => s"TIMESTAMPTZ '${timestamptzLiteral.format(v)}'"
      case Json(v)            => quote(v)
      case Uuid(v)            => quote(v.toString)
      case Array(_, values)   => values.map(literal).mkString("ARRAY[", ", ", "]")
      case Other(_, text)     => quote(text)

  def quote(text: String): SqlText =
    SqlText(s"'${text.replace("'", "''")}'")

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
