package saferis.postgres

import saferis.JsonText
import saferis.ServerType
import saferis.SqlType
import saferis.SqlValue
import saferis.TypeName

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

  /** Array OID to element OID. `1007` is `int4[]`, `1009` is `text[]`. */
  private val arrayElement: Map[Int, Int] = Map(
    1000 -> 16,
    1001 -> 17,
    1002 -> 18,
    1003 -> 19,
    1005 -> 21,
    1007 -> 23,
    1009 -> 25,
    1014 -> 1042,
    1015 -> 1043,
    1016 -> 20,
    1021 -> 700,
    1022 -> 701,
    1115 -> 1114,
    1182 -> 1082,
    1183 -> 1083,
    1185 -> 1184,
    1231 -> 1700,
    1270 -> 1266,
    199  -> 114,
    2951 -> 2950,
    3807 -> 3802,
  )

  private def elementOid(arrayOid: Int): Option[Int] =
    arrayElement.get(arrayOid)

  /** Array OID for an element OID, for example `23` (`int4`) to `1007` (`int4[]`). */
  def arrayOid(elementOid: Int): Option[Int] =
    arrayElement.collectFirst { case (array, element) if element == elementOid => array }

  def sqlType(oid: Int): Option[SqlType] =
    elementOid(oid) match
      case Some(element) =>
        Some(SqlType.Array(sqlType(element).getOrElse(SqlType.Other(ServerType.Oid(element)))))
      case None => scalarType(oid)

  /** Scalar OID for a Postgres type name, including the SQL-standard spellings. Drivers that report names, not OIDs,
    * use this.
    */
  def oidOf(typeName: TypeName): Option[Int] = typeName.toLowerCase(Locale.ROOT) match
    case "bool" | "boolean"                  => Some(16)
    case "bytea"                             => Some(17)
    case "char"                              => Some(18)
    case "name"                              => Some(19)
    case "int8" | "bigint" | "bigserial"     => Some(20)
    case "int2" | "smallint" | "smallserial" => Some(21)
    case "int4" | "integer" | "serial"       => Some(23)
    case "text"                              => Some(25)
    case "json"                              => Some(114)
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
    case "jsonb"                             => Some(3802)
    case _                                   => None

  private def scalarType(oid: Int): Option[SqlType] = oid match
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

  /** Cast name for `$n::cast`. `None` for [[SqlType.Other]]: an enum must not be cast to `text`. Arrays recurse. */
  def cast(tpe: SqlType): Option[TypeName] = tpe match
    case SqlType.Bool           => Some(TypeName("boolean"))
    case SqlType.Int2           => Some(TypeName("int2"))
    case SqlType.Int4           => Some(TypeName("int4"))
    case SqlType.Int8           => Some(TypeName("int8"))
    case SqlType.Float4         => Some(TypeName("float4"))
    case SqlType.Float8         => Some(TypeName("float8"))
    case SqlType.Numeric        => Some(TypeName("numeric"))
    case SqlType.VarChar        => Some(TypeName("varchar"))
    case SqlType.Text           => Some(TypeName("text"))
    case SqlType.Bytea          => Some(TypeName("bytea"))
    case SqlType.Date           => Some(TypeName("date"))
    case SqlType.Time           => Some(TypeName("time"))
    case SqlType.Timestamp      => Some(TypeName("timestamp"))
    case SqlType.Timestamptz    => Some(TypeName("timestamptz"))
    case SqlType.Jsonb          => Some(TypeName("jsonb"))
    case SqlType.Uuid           => Some(TypeName("uuid"))
    case SqlType.Array(element) => cast(element).map(name => TypeName(s"$name[]"))
    case SqlType.Other(_)       => None

  def typeLabel(oid: Int): TypeName =
    TypeName(sqlType(oid).fold(oid.toString)(_.productPrefix))

  /** Unknown OIDs are [[SqlValue.Other]], not a failed row. Array OIDs decode to [[SqlValue.Array]]. */
  def decode(oid: Int, text: String): Either[String, SqlValue] =
    elementOid(oid) match
      case Some(element) => decodeArray(element, text)
      case None          => decodeScalar(oid, text)

  private def decodeScalar(oid: Int, text: String): Either[String, SqlValue] =
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
      case 114 | 3802 => Right(SqlValue.Jsonb(JsonText(text)))
      case 2950       =>
        uuid(text) match
          case Some(v) => Right(SqlValue.Uuid(v))
          case None    => bad(s"not a uuid: $text")
      case _ => Right(SqlValue.Other(ServerType.Oid(oid), text))
    end match
  end decodeScalar

  /** `None` is SQL null. Null is not an empty string. */
  def encode(value: SqlValue): Option[String] = value match
    case SqlValue.Null(_)          => None
    case SqlValue.Bool(v)          => Some(if v then "t" else "f")
    case SqlValue.Int2(v)          => Some(v.toString)
    case SqlValue.Int4(v)          => Some(v.toString)
    case SqlValue.Int8(v)          => Some(v.toString)
    case SqlValue.Float4(v)        => Some(encodeFloat(v))
    case SqlValue.Float8(v)        => Some(encodeDouble(v))
    case SqlValue.Numeric(v)       => Some(v.underlying.toPlainString)
    case SqlValue.VarChar(v)       => Some(v)
    case SqlValue.Text(v)          => Some(v)
    case SqlValue.Bytea(v)         => Some(encodeBytea(v))
    case SqlValue.Date(v)          => Some(v.toString)
    case SqlValue.Time(v)          => Some(formatTime(v))
    case SqlValue.Timestamp(v)     => Some(formatTimestamp(v))
    case SqlValue.Timestamptz(v)   => Some(s"${formatTimestamp(LocalDateTime.ofInstant(v, ZoneOffset.UTC))}+00")
    case SqlValue.Jsonb(v)         => Some(v)
    case SqlValue.Uuid(v)          => Some(v.toString)
    case SqlValue.Other(_, text)   => Some(text)
    case SqlValue.Array(_, values) => Some(encodeArray(values))

  private def encodeArray(values: Chunk[SqlValue]): String =
    values.map(encodeMember).mkString("{", ",", "}")

  private def encodeMember(value: SqlValue): String = value match
    case SqlValue.Null(_)          => "NULL"
    case SqlValue.Array(_, nested) => encodeArray(nested)
    case other                     =>
      encode(other) match
        case None       => "NULL"
        case Some(text) => quoteArrayElement(text)

  /** Quote when empty, special, whitespace, or the unquoted null token. Escape `"` and `\` inside quotes. */
  private def quoteArrayElement(text: String): String =
    val quote = text.isEmpty ||
      text.equalsIgnoreCase("NULL") ||
      text.exists(c => c == '{' || c == '}' || c == ',' || c == '"' || c == '\\' || c.isWhitespace)
    if !quote then text
    else
      val out = new StringBuilder(text.length + 2)
      out.append('"')
      text.foreach: c =>
        if c == '"' || c == '\\' then out.append('\\')
        out.append(c)
      out.append('"')
      out.toString
  end quoteArrayElement

  private def decodeArray(element: Int, text: String): Either[String, SqlValue] =
    parseArray(text).flatMap: members =>
      val elementType = sqlType(element).getOrElse(SqlType.Other(ServerType.Oid(element)))
      val nested      = members.exists:
        case ArrayMember.Nested(_) => true
        case _                     => false
      val memberType = if nested then SqlType.Array(elementType) else elementType
      members
        .foldLeft[Either[String, Chunk[SqlValue]]](Right(Chunk.empty)):
          case (Left(err), _)       => Left(err)
          case (Right(acc), member) => decodeMember(element, elementType, nested, member).map(acc :+ _)
        .flatMap(values => SqlValue.array(memberType, values))

  private def decodeMember(
      element: Int,
      elementType: SqlType,
      nested: Boolean,
      member: ArrayMember,
  ): Either[String, SqlValue] = member match
    case ArrayMember.Null =>
      Right(SqlValue.Null(if nested then SqlType.Array(elementType) else elementType))
    case ArrayMember.Nested(raw) => decodeArray(element, raw)
    case ArrayMember.Text(raw)   =>
      if nested then Left(s"flat element in a nested array: $raw")
      else decode(element, raw)

  private enum ArrayMember:
    case Null
    case Text(value: String)
    case Nested(literal: String)

  private def parseArray(text: String): Either[String, Chunk[ArrayMember]] =
    if text.length < 2 || text.charAt(0) != '{' then Left(s"not an array: $text")
    else parseMembers(text, 1)

  private def parseMembers(text: String, start: Int): Either[String, Chunk[ArrayMember]] =
    val out                   = Chunk.newBuilder[ArrayMember]
    var i                     = start
    var error: Option[String] = None
    var closed                = false
    if start < text.length && text.charAt(start) == '}' then
      closed = true
      i = start + 1
    while i < text.length && error.isEmpty && !closed do
      val c = text.charAt(i)
      if c == '}' then
        closed = true
        i += 1
      else if c == '"' then
        quoted(text, i + 1) match
          case Left(detail)       => error = Some(detail)
          case Right(value, next) =>
            out += ArrayMember.Text(value)
            val step = separator(text, next)
            if step < 0 then error = Some(s"bad array near $next")
            else
              closed = text.charAt(next) == '}' || (step > next && text.charAt(step - 1) == '}')
              i = step
      else if c == '{' then
        val end = matchingBrace(text, i)
        if end < 0 then error = Some("unclosed nested array")
        else
          out += ArrayMember.Nested(text.substring(i, end + 1))
          val step = separator(text, end + 1)
          if step < 0 then error = Some(s"bad array near ${end + 1}")
          else
            closed = text.charAt(end + 1) == '}'
            i = if closed then end + 2 else step
      else
        val end = unquotedEnd(text, i)
        val raw = text.substring(i, end)
        if raw.equalsIgnoreCase("NULL") then out += ArrayMember.Null
        else out += ArrayMember.Text(raw)
        if end >= text.length then error = Some("unclosed array")
        else if text.charAt(end) == '}' then
          closed = true
          i = end + 1
        else if text.charAt(end) == ',' then i = end + 1
        else error = Some(s"bad array near $end")
      end if
    end while
    if error.nonEmpty then Left(error.get)
    else if !closed then Left("unclosed array")
    else Right(out.result())
  end parseMembers

  /** Index after a comma, or after a closing brace. Negative on a bad character. */
  private def separator(text: String, at: Int): Int =
    if at >= text.length then -1
    else if text.charAt(at) == ',' then at + 1
    else if text.charAt(at) == '}' then at + 1
    else -1

  private def unquotedEnd(text: String, at: Int): Int =
    var i = at
    while i < text.length && text.charAt(i) != ',' && text.charAt(i) != '}' do i += 1
    i

  private def matchingBrace(text: String, open: Int): Int =
    var i     = open
    var depth = 0
    var quote = false
    var done  = false
    var found = -1
    while i < text.length && !done do
      val c = text.charAt(i)
      if quote then
        if c == '\\' && i + 1 < text.length then i += 2
        else
          if c == '"' then quote = false
          i += 1
      else if c == '"' then
        quote = true
        i += 1
      else if c == '{' then
        depth += 1
        i += 1
      else if c == '}' then
        depth -= 1
        if depth == 0 then
          found = i
          done = true
        i += 1
      else i += 1
      end if
    end while
    found
  end matchingBrace

  private def quoted(text: String, at: Int): Either[String, (String, Int)] =
    val out                   = new StringBuilder
    var i                     = at
    var closed                = false
    var error: Option[String] = None
    while i < text.length && error.isEmpty && !closed do
      val c = text.charAt(i)
      if c == '\\' then
        if i + 1 >= text.length then error = Some("truncated array escape")
        else
          out.append(text.charAt(i + 1))
          i += 2
      else if c == '"' then
        closed = true
        i += 1
      else
        out.append(c)
        i += 1
      end if
    end while
    if error.nonEmpty then Left(error.get)
    else if !closed then Left("unclosed array quote")
    else Right((out.toString, i))
  end quoted

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
