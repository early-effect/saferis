package saferis

import zio.Chunk
import zio.Duration

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

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal
import scala.util.matching.Regex

/** Casts, bind values, and `DateStyle=ISO` text. The cast name is a function of `SqlType`, never user text. */
private[saferis] object PgWire:
  private val dateFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd", Locale.ROOT)
  private val timeFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm:ss", Locale.ROOT)
  private val timestampFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss", Locale.ROOT)

  /** Offset at the end of a `DateStyle=ISO` time or timestamptz, for example `+00` or `-05:30`. */
  private val zoneTail: Regex = """^(.+)([+-]\d{2}(?::\d{2}){0,2})$""".r

  private val rawParser: js.Function1[js.Any, js.Any] =
    (value: js.Any) => value

  /** Returns the raw text for every OID. Passed on the pool. Not `pg.types.setTypeParser`. */
  val getTypeParser: js.Function2[js.Any, js.Any, js.Function1[js.Any, js.Any]] =
    (_: js.Any, _: js.Any) => rawParser

  def poolConfig(config: PgConfig): js.Object =
    val types = js.Dynamic.literal("getTypeParser" -> getTypeParser)
    js.Dynamic.literal(
      "host"            -> config.host,
      "port"            -> config.port.toDouble,
      "database"        -> config.database,
      "user"            -> config.user,
      "password"        -> config.password,
      "max"             -> config.poolSize.toDouble,
      "ssl"             -> config.ssl,
      "options"         -> "-c DateStyle=ISO",
      "allowExitOnIdle" -> true,
      "types"           -> types,
    )
  end poolConfig

  def queryConfig(sql: String, values: js.Array[js.Any]): js.Object =
    js.Dynamic.literal(
      "text"    -> sql,
      "values"  -> values,
      "rowMode" -> "array",
    )

  def render(command: SqlCommand): String =
    command.render((index, tpe) => s"$$$index::${cast(tpe)}")

  /** Whole milliseconds, round up, minimum 1. Infinity is `Int.MaxValue` milliseconds. `0` is not a present cap. */
  def millis(d: Duration): Int =
    if d.compareTo(Duration.Infinity) >= 0 then Int.MaxValue
    else if d.isZero || d.isNegative then 1
    else
      val seconds = d.getSeconds
      val nano    = d.getNano
      if seconds >= (Int.MaxValue.toLong / 1000L) then Int.MaxValue
      else
        val base    = seconds * 1000L + (nano / 1000000L)
        val rounded = if nano % 1000000L != 0L then base + 1L else base
        if rounded < 1L then 1
        else if rounded > Int.MaxValue.toLong then Int.MaxValue
        else rounded.toInt

  def parameters(pieces: Chunk[SqlPiece]): js.Array[js.Any] =
    val values = js.Array[js.Any]()
    pieces.foreach:
      case SqlPiece.Text(_)      => ()
      case SqlPiece.Param(value) =>
        val _ = values.push(bind(value))
    values

  def ensureSingle(result: PgResult): Either[SaferisError, PgResult] =
    if js.Array.isArray(result.asInstanceOf[js.Any]) then
      Left(SaferisError.Unexpected("multiple statements are not one command"))
    else Right(result)

  def rowCount(result: PgResult): Either[SaferisError, Long] =
    ensureSingle(result).map: single =>
      val raw = single.asInstanceOf[js.Dynamic].rowCount
      if js.isUndefined(raw) || (raw eq null) then 0L
      else raw.asInstanceOf[Double].toLong

  def readRows(result: PgResult): Either[SaferisError, Chunk[SqlRow]] =
    ensureSingle(result).flatMap: single =>
      readFields(single)

  private def readFields(result: PgResult): Either[SaferisError, Chunk[SqlRow]] =
    val fields = result.fields
    if fields == null || js.isUndefined(fields) then Left(SaferisError.Unexpected("query result has no fields"))
    else
      val width  = fields.length
      val labels = Chunk.fromIterator(Iterator.tabulate(width)(i => fieldName(fields(i))))
      val oids   = Array.tabulate(width)(i => fields(i).dataTypeID.toInt)
      val grid   = result.rows
      if grid == null || js.isUndefined(grid) then Right(Chunk.empty)
      else readGrid(labels, oids, grid)
  end readFields

  private def readGrid(
      labels: Chunk[String],
      oids: Array[Int],
      grid: js.Array[js.Array[js.Any]],
  ): Either[SaferisError, Chunk[SqlRow]] =
    val rows                         = Chunk.newBuilder[SqlRow]
    var index                        = 0
    var failed: Option[SaferisError] = None
    while index < grid.length && failed.isEmpty do
      readRow(labels, oids, grid(index)) match
        case Left(err)  => failed = Some(err)
        case Right(row) => rows += row
      index += 1
    failed.fold[Either[SaferisError, Chunk[SqlRow]]](Right(rows.result()))(Left(_))
  end readGrid

  private def readRow(
      labels: Chunk[String],
      oids: Array[Int],
      row: js.Array[js.Any],
  ): Either[SaferisError, SqlRow] =
    if row.length != oids.length then
      Left(SaferisError.DecodingError("", "row", s"width ${row.length} does not match ${oids.length} fields"))
    else
      val cells = (0 until oids.length).foldLeft[Either[SaferisError, Chunk[SqlValue]]](Right(Chunk.empty)):
        case (Left(err), _)  => Left(err)
        case (Right(acc), i) =>
          readCell(oids(i), labels(i), row(i)).map(acc :+ _)
      cells.map(values => SqlRow(labels, values))

  private def fieldName(field: PgField): String =
    val name = field.name
    if name == null then "" else name

  /** SQL null is `Null` before any JSON parse. JSON null arrives as the text `null`. */
  private def readCell(oid: Int, label: String, raw: js.Any): Either[SaferisError, SqlValue] =
    if isJsNull(raw) then
      sqlType(oid) match
        case Some(tpe) => Right(SqlValue.Null(tpe))
        case None      => unrecognized(label, oid)
    // Scala.js will not match `js.Any` as `String`. A non-string is a cell failure.
    // Unknown OIDs still fail in `parseText` with the OID. This is not a type guess.
    else if js.typeOf(raw) != "string" then
      Left(SaferisError.DecodingError(label, oid.toString, s"expected raw text for oid $oid"))
    else parseText(oid, label, raw.asInstanceOf[String])

  private def parseText(oid: Int, label: String, text: String): Either[SaferisError, SqlValue] =
    def bad(detail: String): Left[SaferisError, SqlValue] =
      Left(SaferisError.DecodingError(label, typeLabel(oid), detail))
    oid match
      case 16 =>
        text match
          case "t" => Right(SqlValue.Bool(true))
          case "f" => Right(SqlValue.Bool(false))
          case _   => bad(s"expected t or f, found $text")
      case 21 =>
        text.toShortOption match
          case Some(v) => Right(SqlValue.SmallInt(v))
          case None    => bad(s"not an int2: $text")
      case 23 =>
        text.toIntOption match
          case Some(v) => Right(SqlValue.Integer(v))
          case None    => bad(s"not an int4: $text")
      case 20 =>
        text.toLongOption match
          case Some(v) => Right(SqlValue.BigInt(v))
          case None    => bad(s"not an int8: $text")
      case 700 =>
        text.toFloatOption match
          case Some(v) => Right(SqlValue.Real(v))
          case None    => bad(s"not a float4: $text")
      case 701 =>
        text.toDoubleOption match
          case Some(v) => Right(SqlValue.DoublePrecision(v))
          case None    => bad(s"not a float8: $text")
      case 1700 =>
        numeric(text) match
          case Some(v) => Right(SqlValue.Numeric(v))
          case None    => bad(s"not a numeric: $text")
      case 1043 | 18 | 19 | 1042 => Right(SqlValue.VarChar(text))
      case 25 | 705              => Right(SqlValue.Text(text))
      case 17                    =>
        decodeBytea(text) match
          case Right(bytes) => Right(SqlValue.Binary(bytes))
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
          case Right(v)     => Right(SqlValue.TimestampTz(v))
          case Left(detail) => bad(detail)
      case 114 | 3802 => Right(SqlValue.Json(text))
      case 2950       =>
        uuid(text) match
          case Some(v) => Right(SqlValue.Uuid(v))
          case None    => bad(s"not a uuid: $text")
      case _ => unrecognized(label, oid)
    end match
  end parseText

  private def unrecognized(label: String, oid: Int): Left[SaferisError, SqlValue] =
    Left(SaferisError.DecodingError(label, oid.toString, s"unrecognized oid $oid"))

  private def typeLabel(oid: Int): String =
    sqlType(oid).fold(oid.toString)(_.productPrefix)

  private def sqlType(oid: Int): Option[SqlType] = oid match
    case 16                    => Some(SqlType.Bool)
    case 21                    => Some(SqlType.SmallInt)
    case 23                    => Some(SqlType.Integer)
    case 20                    => Some(SqlType.BigInt)
    case 700                   => Some(SqlType.Real)
    case 701                   => Some(SqlType.DoublePrecision)
    case 1700                  => Some(SqlType.Numeric)
    case 1043 | 18 | 19 | 1042 => Some(SqlType.VarChar)
    case 25 | 705              => Some(SqlType.Text)
    case 17                    => Some(SqlType.Binary)
    case 1082                  => Some(SqlType.Date)
    case 1083 | 1266           => Some(SqlType.Time)
    case 1114                  => Some(SqlType.Timestamp)
    case 1184                  => Some(SqlType.TimestampTz)
    case 114 | 3802            => Some(SqlType.Json)
    case 2950                  => Some(SqlType.Uuid)
    case _                     => None

  private def cast(tpe: SqlType): String = tpe match
    case SqlType.Bool            => "boolean"
    case SqlType.SmallInt        => "int2"
    case SqlType.Integer         => "int4"
    case SqlType.BigInt          => "int8"
    case SqlType.Real            => "float4"
    case SqlType.DoublePrecision => "float8"
    case SqlType.Numeric         => "numeric"
    case SqlType.VarChar         => "varchar"
    case SqlType.Text            => "text"
    case SqlType.Binary          => "bytea"
    case SqlType.Date            => "date"
    case SqlType.Time            => "time"
    case SqlType.Timestamp       => "timestamp"
    case SqlType.TimestampTz     => "timestamptz"
    case SqlType.Json            => "jsonb"
    case SqlType.Uuid            => "uuid"

  private def bind(value: SqlValue): js.Any = value match
    case SqlValue.Null(_)            => jsNull
    case SqlValue.Bool(v)            => js.Any.fromBoolean(v)
    case SqlValue.SmallInt(v)        => js.Any.fromDouble(v.toDouble)
    case SqlValue.Integer(v)         => js.Any.fromDouble(v.toDouble)
    case SqlValue.BigInt(v)          => v.toString
    case SqlValue.Real(v)            => js.Any.fromDouble(v.toDouble)
    case SqlValue.DoublePrecision(v) => js.Any.fromDouble(v)
    case SqlValue.Numeric(v)         => v.underlying.toPlainString
    case SqlValue.VarChar(v)         => v
    case SqlValue.Text(v)            => v
    case SqlValue.Binary(v)          => bytea(v)
    case SqlValue.Date(v)            => v.toString
    case SqlValue.Time(v)            => formatTime(v)
    case SqlValue.Timestamp(v)       => formatTimestamp(v)
    case SqlValue.TimestampTz(v)     => formatTimestamptz(v)
    case SqlValue.Json(v)            => v
    case SqlValue.Uuid(v)            => v.toString

  /** JS null. Stays inside the bind. Callers see `SqlValue.Null`, not this. */
  private def jsNull: js.Any = null

  private def bytea(bytes: Chunk[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    var i   = 0
    while i < bytes.length do
      out(i) = (bytes(i) & 0xff).toShort
      i += 1
    out

  private def formatTime(value: LocalTime): String =
    val base   = f"${value.getHour}%02d:${value.getMinute}%02d:${value.getSecond}%02d"
    val micros = value.getNano / 1000
    if micros == 0 then base else f"$base.$micros%06d"

  private def formatTimestamp(value: LocalDateTime): String =
    val micros = value.getNano / 1000
    f"${value.getYear}%04d-${value.getMonthValue}%02d-${value.getDayOfMonth}%02d ${value.getHour}%02d:${value.getMinute}%02d:${value.getSecond}%02d.$micros%06d"

  private def formatTimestamptz(instant: Instant): String =
    s"${formatTimestamp(LocalDateTime.ofInstant(instant, ZoneOffset.UTC))}+00"

  private def isJsNull(raw: js.Any): Boolean =
    js.isUndefined(raw) || (raw eq null)

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

  /** `timetz` may append a numeric offset. The offset is not part of `LocalTime`. */
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
end PgWire
