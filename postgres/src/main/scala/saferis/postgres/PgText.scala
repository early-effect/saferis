package saferis.postgres

import saferis.ServerType
import saferis.SqlType
import saferis.SqlValue

import zio.Chunk

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.temporal.TemporalAccessor
import java.util.Locale
import java.util.UUID

import scala.util.control.NonFatal
import scala.util.matching.Regex

/** Postgres `DateStyle=ISO` text. No sockets and no `js.Any`. Drivers bind the result. */
object PgText:
  private val dateFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd", Locale.ROOT)
  private val timeFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm:ss", Locale.ROOT)
  private val timestampFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss", Locale.ROOT)

  /** Offset at the end of a `DateStyle=ISO` time or timestamptz, for example `+00` or `-05:30`. */
  private val zoneTail: Regex = """^(.+)([+-]\d{2}(?::\d{2}){0,2})$""".r

  def sqlType(oid: Int): Option[SqlType] = oid match
    case 16                    => Some(SqlType.Bool)
    case 21                    => Some(SqlType.Int2)
    case 23                    => Some(SqlType.Int4)
    case 20                    => Some(SqlType.Int8)
    case 700                   => Some(SqlType.Float4)
    case 701                   => Some(SqlType.Float8)
    case 1700                  => Some(SqlType.Numeric)
    case 1043 | 18 | 19 | 1042 => Some(SqlType.VarChar)
    case 25 | 705              => Some(SqlType.Text)
    case 17                    => Some(SqlType.Bytea)
    case 1082                  => Some(SqlType.Date)
    case 1083 | 1266           => Some(SqlType.Time)
    case 1114                  => Some(SqlType.Timestamp)
    case 1184                  => Some(SqlType.Timestamptz)
    case 114 | 3802            => Some(SqlType.Jsonb)
    case 2950                  => Some(SqlType.Uuid)
    case _                     => None

  def oid(tpe: SqlType): Option[Int] = tpe match
    case SqlType.Bool                          => Some(16)
    case SqlType.Int2                          => Some(21)
    case SqlType.Int4                          => Some(23)
    case SqlType.Int8                          => Some(20)
    case SqlType.Float4                        => Some(700)
    case SqlType.Float8                        => Some(701)
    case SqlType.Numeric                       => Some(1700)
    case SqlType.VarChar                       => Some(1043)
    case SqlType.Text                          => Some(25)
    case SqlType.Bytea                         => Some(17)
    case SqlType.Date                          => Some(1082)
    case SqlType.Time                          => Some(1083)
    case SqlType.Timestamp                     => Some(1114)
    case SqlType.Timestamptz                   => Some(1184)
    case SqlType.Jsonb                         => Some(3802)
    case SqlType.Uuid                          => Some(2950)
    case SqlType.Array(_)                      => None
    case SqlType.Other(ServerType.Oid(id))     => Some(id)
    case SqlType.Other(ServerType.Both(_, id)) => Some(id)
    case SqlType.Other(ServerType.Named(_))    => None

  /** Cast name for `$n::cast`. An array cast is `cast(element) + "[]"` at the call site. */
  def cast(tpe: SqlType): String = tpe match
    case SqlType.Bool        => "boolean"
    case SqlType.Int2        => "int2"
    case SqlType.Int4        => "int4"
    case SqlType.Int8        => "int8"
    case SqlType.Float4      => "float4"
    case SqlType.Float8      => "float8"
    case SqlType.Numeric     => "numeric"
    case SqlType.VarChar     => "varchar"
    case SqlType.Text        => "text"
    case SqlType.Bytea       => "bytea"
    case SqlType.Date        => "date"
    case SqlType.Time        => "time"
    case SqlType.Timestamp   => "timestamp"
    case SqlType.Timestamptz => "timestamptz"
    case SqlType.Jsonb       => "jsonb"
    case SqlType.Uuid        => "uuid"
    case SqlType.Array(_)    => "text"
    case SqlType.Other(_)    => "text"

  def typeLabel(oid: Int): String =
    sqlType(oid).fold(oid.toString)(_.productPrefix)

  /** Unknown OIDs are [[SqlValue.Other]], not a failed row. */
  def decode(oid: Int, text: String): Either[String, SqlValue] =
    def bad(detail: String): Left[String, SqlValue] = Left(detail)
    oid match
      case 16 =>
        text match
          case "t" => Right(SqlValue.Bool(true))
          case "f" => Right(SqlValue.Bool(false))
          case _   => bad(s"expected t or f, found $text")
      case 21 =>
        text.toShortOption match
          case Some(v) => Right(SqlValue.Int2(v))
          case None    => bad(s"not an int2: $text")
      case 23 =>
        text.toIntOption match
          case Some(v) => Right(SqlValue.Int4(v))
          case None    => bad(s"not an int4: $text")
      case 20 =>
        text.toLongOption match
          case Some(v) => Right(SqlValue.Int8(v))
          case None    => bad(s"not an int8: $text")
      case 700 =>
        text.toFloatOption match
          case Some(v) => Right(SqlValue.Float4(v))
          case None    => bad(s"not a float4: $text")
      case 701 =>
        text.toDoubleOption match
          case Some(v) => Right(SqlValue.Float8(v))
          case None    => bad(s"not a float8: $text")
      case 1700 =>
        numeric(text) match
          case Some(v) => Right(SqlValue.Numeric(v))
          case None    => bad(s"not a numeric: $text")
      case 1043 | 18 | 19 | 1042 => Right(SqlValue.VarChar(text))
      case 25 | 705              => Right(SqlValue.Text(text))
      case 17                    =>
        decodeBytea(text) match
          case Right(bytes) => Right(SqlValue.Bytea(bytes))
          case Left(detail) => bad(detail)
      case 1082 =>
        parseDate(text) match
          case Right(v)     => Right(SqlValue.Date(v))
          case Left(detail) => bad(detail)
      case 1083 =>
        parseTime(text) match
          case Right(v)     => Right(SqlValue.Time(v))
          case Left(detail) => bad(detail)
      case 1266 =>
        parseTimetz(text) match
          case Right(v)     => Right(SqlValue.Time(v))
          case Left(detail) => bad(detail)
      case 1114 =>
        parseTimestamp(text) match
          case Right(v)     => Right(SqlValue.Timestamp(v))
          case Left(detail) => bad(detail)
      case 1184 =>
        parseTimestamptz(text) match
          case Right(v)     => Right(SqlValue.Timestamptz(v))
          case Left(detail) => bad(detail)
      case 114 | 3802 => Right(SqlValue.Jsonb(text))
      case 2950       =>
        uuid(text) match
          case Some(v) => Right(SqlValue.Uuid(v))
          case None    => bad(s"not a uuid: $text")
      case _ => Right(SqlValue.Other(ServerType.Oid(oid), text))
    end match
  end decode

  def encode(value: SqlValue): String = value match
    case SqlValue.Null(_)          => ""
    case SqlValue.Bool(v)          => if v then "t" else "f"
    case SqlValue.Int2(v)          => v.toString
    case SqlValue.Int4(v)          => v.toString
    case SqlValue.Int8(v)          => v.toString
    case SqlValue.Float4(v)        => encodeFloat(v)
    case SqlValue.Float8(v)        => encodeDouble(v)
    case SqlValue.Numeric(v)       => v.underlying.toPlainString
    case SqlValue.VarChar(v)       => v
    case SqlValue.Text(v)          => v
    case SqlValue.Bytea(v)         => encodeBytea(v)
    case SqlValue.Date(v)          => v.toString
    case SqlValue.Time(v)          => formatTime(v)
    case SqlValue.Timestamp(v)     => formatTimestamp(v)
    case SqlValue.Timestamptz(v)   => s"${formatTimestamp(LocalDateTime.ofInstant(v, ZoneOffset.UTC))}+00"
    case SqlValue.Jsonb(v)         => v
    case SqlValue.Uuid(v)          => v.toString
    case SqlValue.Other(_, text)   => text
    case SqlValue.Array(_, values) => values.map(encode).mkString("{", ",", "}")

  private def encodeFloat(v: Float): String =
    if v.isNaN then "NaN"
    else if v.isInfinite then if v > 0.0f then "Infinity" else "-Infinity"
    else java.lang.Float.toString(v)

  private def encodeDouble(v: Double): String =
    if v.isNaN then "NaN"
    else if v.isInfinite then if v > 0.0 then "Infinity" else "-Infinity"
    else java.lang.Double.toString(v)

  private def encodeBytea(bytes: Chunk[Byte]): String =
    val hex = bytes.map(b => f"${b & 0xff}%02x").mkString
    s"\\x$hex"

  private def formatTime(value: LocalTime): String =
    val base   = f"${value.getHour}%02d:${value.getMinute}%02d:${value.getSecond}%02d"
    val micros = value.getNano / 1000
    if micros == 0 then base else f"$base.$micros%06d"

  private def formatTimestamp(value: LocalDateTime): String =
    val micros = value.getNano / 1000
    f"${value.getYear}%04d-${value.getMonthValue}%02d-${value.getDayOfMonth}%02d ${value.getHour}%02d:${value.getMinute}%02d:${value.getSecond}%02d.$micros%06d"

  private def numeric(text: String): Option[BigDecimal] =
    try Some(BigDecimal(text))
    catch case NonFatal(_) => None

  private def uuid(text: String): Option[UUID] =
    try Some(UUID.fromString(text))
    catch case NonFatal(_) => None

  private def parseDate(text: String): Either[String, LocalDate] =
    parsed(dateFmt, text, LocalDate.from, "date")

  private def parseTime(text: String): Either[String, LocalTime] =
    splitFraction(text).flatMap: (head, nanos) =>
      parsed(timeFmt, head, LocalTime.from, "time").map(_.withNano(nanos))

  private def parseTimetz(text: String): Either[String, LocalTime] =
    text match
      case zoneTail(body, _) => parseTime(body)
      case other             => parseTime(other)

  private def parseTimestamp(text: String): Either[String, LocalDateTime] =
    splitFraction(text).flatMap: (head, nanos) =>
      parsed(timestampFmt, head, LocalDateTime.from, "timestamp").map(_.withNano(nanos))

  private def parseTimestamptz(text: String): Either[String, Instant] =
    text match
      case zoneTail(body, offsetText) =>
        for
          offset <- parseOffset(offsetText)
          local  <- parseTimestamp(body)
        yield local.atOffset(offset).toInstant
      case _ => Left(s"not a timestamptz: $text")

  private def parseOffset(text: String): Either[String, ZoneOffset] =
    if text.isEmpty || (text.charAt(0) != '+' && text.charAt(0) != '-') then Left(s"not an offset: $text")
    else
      val sign  = if text.charAt(0) == '-' then -1 else 1
      val parts = text.substring(1).split(':')
      val hours = parts.headOption.flatMap(_.toIntOption)
      val mins  = if parts.length > 1 then parts(1).toIntOption else Some(0)
      val secs  = if parts.length > 2 then parts(2).toIntOption else Some(0)
      (hours, mins, secs) match
        case (Some(h), Some(m), Some(s))
            if parts.length <= 3 && h >= 0 && h <= 18 && m >= 0 && m < 60 && s >= 0 && s < 60 =>
          Right(ZoneOffset.ofTotalSeconds(sign * (h * 3600 + m * 60 + s)))
        case _ => Left(s"not an offset: $text")

  private def splitFraction(text: String): Either[String, (String, Int)] =
    text.indexOf('.') match
      case -1  => Right((text, 0))
      case dot =>
        val frac = text.substring(dot + 1)
        if frac.isEmpty || !frac.forall(_.isDigit) then Left(s"bad fraction in $text")
        else Right((text.substring(0, dot), digitsToNanos(frac)))

  private def digitsToNanos(digits: String): Int =
    (digits.take(9) + "000000000").take(9).toInt

  private def parsed[A](
      fmt: DateTimeFormatter,
      text: String,
      build: TemporalAccessor => A,
      kind: String,
  ): Either[String, A] =
    try Right(build(fmt.parse(text)))
    catch case _: DateTimeParseException => Left(s"not a $kind: $text")

  private def decodeBytea(text: String): Either[String, Chunk[Byte]] =
    if text.startsWith("\\x") || text.startsWith("\\X") then decodeHex(text.substring(2))
    else decodeEscape(text)

  private def decodeHex(hex: String): Either[String, Chunk[Byte]] =
    if (hex.length & 1) == 1 then Left("odd bytea hex")
    else
      val out = new Array[Byte](hex.length / 2)
      var i   = 0
      var bad = false
      while i < out.length && !bad do
        val hi = Character.digit(hex.charAt(i * 2), 16)
        val lo = Character.digit(hex.charAt(i * 2 + 1), 16)
        if hi < 0 || lo < 0 then bad = true
        else out(i) = ((hi << 4) | lo).toByte
        i += 1
      if bad then Left("bad bytea hex") else Right(Chunk.fromArray(out))

  private def decodeEscape(text: String): Either[String, Chunk[Byte]] =
    val out                   = Chunk.newBuilder[Byte]
    var i                     = 0
    var error: Option[String] = None
    while i < text.length && error.isEmpty do
      val c = text.charAt(i)
      if c != '\\' then
        out += c.toByte
        i += 1
      else if i + 1 >= text.length then error = Some("truncated bytea escape")
      else
        val n = text.charAt(i + 1)
        if n == '\\' then
          out += '\\'.toByte
          i += 2
        else
          octal(text, i + 1) match
            case Some(value) =>
              out += value.toByte
              i += 4
            case None => error = Some(s"bad bytea escape at $i")
      end if
    end while
    error.fold[Either[String, Chunk[Byte]]](Right(out.result()))(Left(_))
  end decodeEscape

  private def octal(text: String, at: Int): Option[Int] =
    if at + 3 > text.length then None
    else
      val slice = text.substring(at, at + 3)
      if slice.forall(c => c >= '0' && c <= '7') then Some(Integer.parseInt(slice, 8))
      else None
end PgText
