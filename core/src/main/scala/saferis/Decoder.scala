package saferis

import zio.Chunk

import scala.util.NotGiven

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeParseException
import java.util.UUID

trait Decoder[A]:
  self =>
  def decode(value: SqlValue): Either[DecodeError, A]
  def transform[B](f: A => Either[DecodeError, B]): Decoder[B] =
    new Decoder[B]:
      def decode(value: SqlValue): Either[DecodeError, B] =
        self.decode(value).flatMap(f)

trait RowDecoder[A]:
  def decode(row: SqlRow): Either[DecodeError, A]

object Decoder:
  private val int2        = TypeName("int2")
  private val int4        = TypeName("int4")
  private val int8        = TypeName("int8")
  private val float4      = TypeName("float4")
  private val float8      = TypeName("float8")
  private val numeric     = TypeName("numeric")
  private val bigint      = TypeName("bigint")
  private val bool        = TypeName("bool")
  private val date        = TypeName("date")
  private val uuidName    = TypeName("uuid")
  private val varchar     = TypeName("varchar")
  private val bytea       = TypeName("bytea")
  private val jsonb       = TypeName("jsonb")
  private val timestamptz = TypeName("timestamptz")
  private val timestamp   = TypeName("timestamp")
  private val time        = TypeName("time")
  private val timetz      = TypeName("timetz")
  private val arrayName   = TypeName("array")

  given option[A](using decoder: Decoder[A]): Decoder[Option[A]] with
    def decode(value: SqlValue): Either[DecodeError, Option[A]] = value match
      case SqlValue.Null(_) => Right(None)
      case other            => decoder.decode(other).map(Some(_))

  given string: Decoder[String] with
    def decode(value: SqlValue): Either[DecodeError, String] = value match
      case SqlValue.Null(_) => Left(DecodeError.Null)
      case other            =>
        rendered(other) match
          case Some(text) => Right(text)
          case None       => Left(DecodeError.Mismatch(varchar, other))

  /** Any integer width, when the value fits. SQLite stores every integer as 64 bits, and `count(*)` is `int8` on
    * Postgres, so the column width alone does not decide the Scala type. Whole numerics, exact finite floats, and
    * integral text are the same integer spelled another way.
    */
  private def integral(expected: TypeName, value: SqlValue): Either[DecodeError, BigInt] = value match
    case SqlValue.Null(_)            => Left(DecodeError.Null)
    case SqlValue.SmallInt(v)        => Right(BigInt(v.toLong))
    case SqlValue.Integer(v)         => Right(BigInt(v))
    case SqlValue.BigInt(v)          => Right(BigInt(v))
    case SqlValue.Numeric(v)         => whole(v).toRight(DecodeError.Lossy(expected, value))
    case SqlValue.Real(v)            => integralFloat(expected, value, v.toDouble)
    case SqlValue.DoublePrecision(v) => integralFloat(expected, value, v)
    case SqlValue.VarChar(text)      => integralText(expected, text)
    case SqlValue.Text(text)         => integralText(expected, text)
    case other                       => Left(DecodeError.Mismatch(expected, other))

  /** Fractional text is not an integer spelling. A fractional `Numeric` is [[DecodeError.Lossy]]. */
  private def integralText(expected: TypeName, text: String): Either[DecodeError, BigInt] =
    parseDecimal(text).flatMap(whole) match
      case Some(n) => Right(n)
      case None    => Left(DecodeError.InvalidText(expected, text))

  private def integralFloat(expected: TypeName, value: SqlValue, number: Double): Either[DecodeError, BigInt] =
    if number.isFinite then whole(BigDecimal.exact(number)).toRight(DecodeError.Lossy(expected, value))
    else Left(DecodeError.Lossy(expected, value))

  private def fits(expected: TypeName, value: SqlValue, min: BigInt, max: BigInt): Either[DecodeError, Long] =
    integral(expected, value).flatMap: n =>
      if n >= min && n <= max then Right(n.toLong)
      else Left(DecodeError.OutOfRange(expected, value))

  given short: Decoder[Short] with
    def decode(value: SqlValue): Either[DecodeError, Short] =
      fits(int2, value, BigInt(Short.MinValue), BigInt(Short.MaxValue)).map(_.toShort)

  given int: Decoder[Int] with
    def decode(value: SqlValue): Either[DecodeError, Int] =
      fits(int4, value, BigInt(Int.MinValue), BigInt(Int.MaxValue)).map(_.toInt)

  given long: Decoder[Long] with
    def decode(value: SqlValue): Either[DecodeError, Long] =
      fits(int8, value, BigInt(Long.MinValue), BigInt(Long.MaxValue))

  given bigInt: Decoder[BigInt] with
    def decode(value: SqlValue): Either[DecodeError, BigInt] =
      integral(bigint, value)

  given boolean: Decoder[Boolean] with
    def decode(value: SqlValue): Either[DecodeError, Boolean] = value match
      case SqlValue.Null(_)          => Left(DecodeError.Null)
      case SqlValue.Bool(v)          => Right(v)
      case SqlValue.VarChar("true")  => Right(true)
      case SqlValue.VarChar("false") => Right(false)
      case SqlValue.Text("true")     => Right(true)
      case SqlValue.Text("false")    => Right(false)
      case SqlValue.VarChar(text)    => Left(DecodeError.InvalidText(bool, text))
      case SqlValue.Text(text)       => Left(DecodeError.InvalidText(bool, text))
      case other                     => Left(DecodeError.Mismatch(bool, other))
  end boolean

  /** A `float8` decodes when it is exactly a `Float`, as every value written from a `Float` is. SQLite stores every
    * `real` as 8 bytes, so this is how a `Float` column reads there. Anything else fails instead of rounding. A
    * `numeric` or numeric text becomes the nearest finite float. An integer is accepted only when it is that float.
    */
  given float: Decoder[Float] with
    def decode(value: SqlValue): Either[DecodeError, Float] = value match
      case SqlValue.Null(_)                                                  => Left(DecodeError.Null)
      case SqlValue.Real(v)                                                  => Right(v)
      case SqlValue.DoublePrecision(v) if v.isNaN || v.toFloat.toDouble == v => Right(v.toFloat)
      case SqlValue.DoublePrecision(_)                                       => Left(DecodeError.Lossy(float4, value))
      case SqlValue.SmallInt(v)                                              => exactFloat(value, BigInt(v.toLong))
      case SqlValue.Integer(v)                                               => exactFloat(value, BigInt(v))
      case SqlValue.BigInt(v)                                                => exactFloat(value, BigInt(v))
      case SqlValue.Numeric(v)                                               => nearestFloat(value, v)
      case SqlValue.VarChar(text)                                            => textFloat(value, text)
      case SqlValue.Text(text)                                               => textFloat(value, text)
      case other => Left(DecodeError.Mismatch(float4, other))
  end float

  /** Decimal to binary float is the nearest finite IEEE value. Overflow is loss. An exact decimal is a `BigDecimal`. An
    * integer is a `Double` only when `toDouble` is still that integer.
    */
  given double: Decoder[Double] with
    def decode(value: SqlValue): Either[DecodeError, Double] = value match
      case SqlValue.Null(_)            => Left(DecodeError.Null)
      case SqlValue.DoublePrecision(v) => Right(v)
      case SqlValue.Real(v)            => Right(v.toDouble)
      case SqlValue.SmallInt(v)        => exactDouble(value, BigInt(v.toLong))
      case SqlValue.Integer(v)         => exactDouble(value, BigInt(v))
      case SqlValue.BigInt(v)          => exactDouble(value, BigInt(v))
      case SqlValue.Numeric(v)         => nearestDouble(value, v)
      case SqlValue.VarChar(text)      => textDouble(value, text)
      case SqlValue.Text(text)         => textDouble(value, text)
      case other                       => Left(DecodeError.Mismatch(float8, other))
  end double

  given bigDecimal: Decoder[BigDecimal] with
    def decode(value: SqlValue): Either[DecodeError, BigDecimal] =
      decimal(value)

  given chunkByte: Decoder[Chunk[Byte]] with
    def decode(value: SqlValue): Either[DecodeError, Chunk[Byte]] = value match
      case SqlValue.Null(_)   => Left(DecodeError.Null)
      case SqlValue.Binary(v) => Right(v)
      case other              => Left(DecodeError.Mismatch(bytea, other))

  given instant: Decoder[Instant] with
    def decode(value: SqlValue): Either[DecodeError, Instant] = value match
      case SqlValue.Null(_)        => Left(DecodeError.Null)
      case SqlValue.TimestampTz(v) => Right(v)
      case other                   => Left(DecodeError.Mismatch(timestamptz, other))

  given localDateTime: Decoder[LocalDateTime] with
    def decode(value: SqlValue): Either[DecodeError, LocalDateTime] = value match
      case SqlValue.Null(_)      => Left(DecodeError.Null)
      case SqlValue.Timestamp(v) => Right(v)
      case other                 => Left(DecodeError.Mismatch(timestamp, other))

  given localDate: Decoder[LocalDate] with
    def decode(value: SqlValue): Either[DecodeError, LocalDate] = value match
      case SqlValue.Null(_)    => Left(DecodeError.Null)
      case SqlValue.Date(v)    => Right(v)
      case SqlValue.VarChar(t) => parseDate(t)
      case SqlValue.Text(t)    => parseDate(t)
      case other               => Left(DecodeError.Mismatch(date, other))

  given localTime: Decoder[LocalTime] with
    def decode(value: SqlValue): Either[DecodeError, LocalTime] = value match
      case SqlValue.Null(_) => Left(DecodeError.Null)
      case SqlValue.Time(v) => Right(v)
      case other            => Left(DecodeError.Mismatch(time, other))

  given offsetTime: Decoder[OffsetTime] with
    def decode(value: SqlValue): Either[DecodeError, OffsetTime] = value match
      case SqlValue.Null(_)   => Left(DecodeError.Null)
      case SqlValue.TimeTz(v) => Right(v)
      case other              => Left(DecodeError.Mismatch(timetz, other))

  given zonedDateTime: Decoder[ZonedDateTime] with
    def decode(value: SqlValue): Either[DecodeError, ZonedDateTime] = value match
      case SqlValue.Null(_)        => Left(DecodeError.Null)
      case SqlValue.TimestampTz(v) => Right(ZonedDateTime.ofInstant(v, ZoneOffset.UTC))
      case other                   => Left(DecodeError.Mismatch(timestamptz, other))

  given offsetDateTime: Decoder[OffsetDateTime] with
    def decode(value: SqlValue): Either[DecodeError, OffsetDateTime] = value match
      case SqlValue.Null(_)        => Left(DecodeError.Null)
      case SqlValue.TimestampTz(v) => Right(OffsetDateTime.ofInstant(v, ZoneOffset.UTC))
      case other                   => Left(DecodeError.Mismatch(timestamptz, other))

  given uuid: Decoder[UUID] with
    def decode(value: SqlValue): Either[DecodeError, UUID] = value match
      case SqlValue.Null(_)    => Left(DecodeError.Null)
      case SqlValue.Uuid(v)    => Right(v)
      case SqlValue.VarChar(t) => parseUuid(t)
      case SqlValue.Text(t)    => parseUuid(t)
      case other               => Left(DecodeError.Mismatch(uuidName, other))

  /** A Postgres array column. `Chunk[Byte]` stays `bytea` ([[chunkByte]]). */
  given array[A](using element: Decoder[A], notBytes: NotGiven[A =:= Byte]): Decoder[Chunk[A]] with
    def decode(value: SqlValue): Either[DecodeError, Chunk[A]] = value match
      case SqlValue.Null(_)          => Left(DecodeError.Null)
      case SqlValue.Array(_, values) =>
        values.foldLeft[Either[DecodeError, Chunk[A]]](Right(Chunk.empty)):
          case (Left(err), _)       => Left(err)
          case (Right(acc), member) => element.decode(member).map(acc :+ _)
      case other => Left(DecodeError.Mismatch(arrayName, other))

  def fromJsonCodec[T](using codec: zio.json.JsonCodec[T]): Decoder[T] =
    new Decoder[T]:
      def decode(value: SqlValue): Either[DecodeError, T] = value match
        case SqlValue.Null(_)    => Left(DecodeError.Null)
        case SqlValue.Json(json) => codec.decoder.decodeJson(json).left.map(DecodeError.Json(_))
        case other               => Left(DecodeError.Mismatch(jsonb, other))

  private def rendered(value: SqlValue): Option[String] = value match
    case SqlValue.VarChar(v)  => Some(v)
    case SqlValue.Text(v)     => Some(v)
    case SqlValue.Other(_, v) => Some(v)
    case SqlValue.SmallInt(v) => Some(v.toString)
    case SqlValue.Integer(v)  => Some(v.toString)
    case SqlValue.BigInt(v)   => Some(v.toString)
    case SqlValue.Numeric(v)  => Some(v.bigDecimal.toPlainString)
    case SqlValue.Bool(v)     => Some(if v then "true" else "false")
    case SqlValue.Date(v)     => Some(v.toString)
    case SqlValue.Uuid(v)     => Some(v.toString)
    case _                    => None

  private def parseDecimal(text: String): Option[BigDecimal] =
    try Some(BigDecimal(text))
    catch case _: NumberFormatException => None

  private def whole(value: BigDecimal): Option[BigInt] =
    try Some(BigInt(value.bigDecimal.toBigIntegerExact))
    catch case _: ArithmeticException => None

  private def decimal(value: SqlValue): Either[DecodeError, BigDecimal] = value match
    case SqlValue.Null(_)            => Left(DecodeError.Null)
    case SqlValue.Numeric(v)         => Right(v)
    case SqlValue.SmallInt(v)        => Right(BigDecimal(v.toLong))
    case SqlValue.Integer(v)         => Right(BigDecimal(v))
    case SqlValue.BigInt(v)          => Right(BigDecimal(v))
    case SqlValue.Real(v)            => finiteDecimal(value, v.toDouble)
    case SqlValue.DoublePrecision(v) => finiteDecimal(value, v)
    case SqlValue.VarChar(text)      => parseDecimal(text).toRight(DecodeError.InvalidText(numeric, text))
    case SqlValue.Text(text)         => parseDecimal(text).toRight(DecodeError.InvalidText(numeric, text))
    case other                       => Left(DecodeError.Mismatch(numeric, other))

  private def finiteDecimal(value: SqlValue, number: Double): Either[DecodeError, BigDecimal] =
    if number.isFinite then Right(BigDecimal.exact(number))
    else Left(DecodeError.Lossy(numeric, value))

  private def nearestDouble(value: SqlValue, number: BigDecimal): Either[DecodeError, Double] =
    val decoded = number.bigDecimal.doubleValue
    if decoded.isFinite then Right(decoded) else Left(DecodeError.Lossy(float8, value))

  private def nearestFloat(value: SqlValue, number: BigDecimal): Either[DecodeError, Float] =
    val decoded = number.bigDecimal.floatValue
    if decoded.isFinite then Right(decoded) else Left(DecodeError.Lossy(float4, value))

  private def exactDouble(value: SqlValue, number: BigInt): Either[DecodeError, Double] =
    val decoded = number.bigInteger.doubleValue
    if decoded.isFinite && BigDecimal.exact(decoded) == BigDecimal(number) then Right(decoded)
    else Left(DecodeError.Lossy(float8, value))

  private def exactFloat(value: SqlValue, number: BigInt): Either[DecodeError, Float] =
    val decoded = number.bigInteger.floatValue
    if decoded.isFinite && BigDecimal.exact(decoded.toDouble) == BigDecimal(number) then Right(decoded)
    else Left(DecodeError.Lossy(float4, value))

  private def textDouble(value: SqlValue, text: String): Either[DecodeError, Double] =
    parseDecimal(text) match
      case Some(number) => nearestDouble(value, number)
      case None         => Left(DecodeError.InvalidText(float8, text))

  private def textFloat(value: SqlValue, text: String): Either[DecodeError, Float] =
    parseDecimal(text) match
      case Some(number) => nearestFloat(value, number)
      case None         => Left(DecodeError.InvalidText(float4, text))

  private def parseDate(text: String): Either[DecodeError, LocalDate] =
    try Right(LocalDate.parse(text))
    catch case _: DateTimeParseException => Left(DecodeError.InvalidText(date, text))

  private def parseUuid(text: String): Either[DecodeError, UUID] =
    try Right(UUID.fromString(text))
    catch case _: IllegalArgumentException => Left(DecodeError.InvalidText(uuidName, text))

end Decoder

object RowDecoder:
  given rowFromCell[A](using cell: Decoder[A]): RowDecoder[A] with
    def decode(row: SqlRow): Either[DecodeError, A] =
      row.at(0).flatMap(cell.decode)

  private def cell[A](row: SqlRow, index: Int)(using decoder: Decoder[A]): Either[DecodeError, A] =
    row.at(index).flatMap(decoder.decode).left.map(err => DecodeError.At(index + 1, err))

  private def width(row: SqlRow, expected: Int): Either[DecodeError, Unit] =
    if row.width == expected then Right(())
    else Left(DecodeError.Width(expected, row.width))

  given tuple2[A, B](using decoderA: Decoder[A], decoderB: Decoder[B]): RowDecoder[(A, B)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B)] =
      width(row, 2).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
        yield (a, b)

  given tuple3[A, B, C](using decoderA: Decoder[A], decoderB: Decoder[B], decoderC: Decoder[C]): RowDecoder[(A, B, C)]
  with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C)] =
      width(row, 3).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
        yield (a, b, c)

  given tuple4[A, B, C, D](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
  ): RowDecoder[(A, B, C, D)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D)] =
      width(row, 4).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
        yield (a, b, c, d)
  end tuple4

  given tuple5[A, B, C, D, E](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
  ): RowDecoder[(A, B, C, D, E)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E)] =
      width(row, 5).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
        yield (a, b, c, d, e)
  end tuple5

  given tuple6[A, B, C, D, E, F](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
  ): RowDecoder[(A, B, C, D, E, F)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F)] =
      width(row, 6).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
        yield (a, b, c, d, e, f)
  end tuple6

  given tuple7[A, B, C, D, E, F, G](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
  ): RowDecoder[(A, B, C, D, E, F, G)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G)] =
      width(row, 7).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
        yield (a, b, c, d, e, f, g)
  end tuple7

  given tuple8[A, B, C, D, E, F, G, H](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
  ): RowDecoder[(A, B, C, D, E, F, G, H)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H)] =
      width(row, 8).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
        yield (a, b, c, d, e, f, g, h)
  end tuple8

  given tuple9[A, B, C, D, E, F, G, H, I](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I)] =
      width(row, 9).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
        yield (a, b, c, d, e, f, g, h, i)
  end tuple9

  given tuple10[A, B, C, D, E, F, G, H, I, J](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J)] =
      width(row, 10).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
        yield (a, b, c, d, e, f, g, h, i, j)
  end tuple10

  given tuple11[A, B, C, D, E, F, G, H, I, J, K](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K)] =
      width(row, 11).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
        yield (a, b, c, d, e, f, g, h, i, j, k)
  end tuple11

  given tuple12[A, B, C, D, E, F, G, H, I, J, K, L](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L)] =
      width(row, 12).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
        yield (a, b, c, d, e, f, g, h, i, j, k, l)
  end tuple12

  given tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M)] =
      width(row, 13).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m)
  end tuple13

  given tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
      width(row, 14).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  end tuple14

  given tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
      width(row, 15).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  end tuple15

  given tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
      width(row, 16).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  end tuple16

  given tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
      width(row, 17).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  end tuple17

  given tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
      decoderR: Decoder[R],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
      width(row, 18).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
          r <- cell[R](row, 17)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  end tuple18

  given tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
      decoderR: Decoder[R],
      decoderS: Decoder[S],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
      width(row, 19).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
          r <- cell[R](row, 17)
          s <- cell[S](row, 18)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  end tuple19

  given tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
      decoderR: Decoder[R],
      decoderS: Decoder[S],
      decoderT: Decoder[T],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
      width(row, 20).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
          r <- cell[R](row, 17)
          s <- cell[S](row, 18)
          t <- cell[T](row, 19)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  end tuple20

  given tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
      decoderR: Decoder[R],
      decoderS: Decoder[S],
      decoderT: Decoder[T],
      decoderU: Decoder[U],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
      width(row, 21).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
          r <- cell[R](row, 17)
          s <- cell[S](row, 18)
          t <- cell[T](row, 19)
          u <- cell[U](row, 20)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  end tuple21

  given tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](using
      decoderA: Decoder[A],
      decoderB: Decoder[B],
      decoderC: Decoder[C],
      decoderD: Decoder[D],
      decoderE: Decoder[E],
      decoderF: Decoder[F],
      decoderG: Decoder[G],
      decoderH: Decoder[H],
      decoderI: Decoder[I],
      decoderJ: Decoder[J],
      decoderK: Decoder[K],
      decoderL: Decoder[L],
      decoderM: Decoder[M],
      decoderN: Decoder[N],
      decoderO: Decoder[O],
      decoderP: Decoder[P],
      decoderQ: Decoder[Q],
      decoderR: Decoder[R],
      decoderS: Decoder[S],
      decoderT: Decoder[T],
      decoderU: Decoder[U],
      decoderV: Decoder[V],
  ): RowDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with
    def decode(row: SqlRow): Either[DecodeError, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
      width(row, 22).flatMap: _ =>
        for
          a <- cell[A](row, 0)
          b <- cell[B](row, 1)
          c <- cell[C](row, 2)
          d <- cell[D](row, 3)
          e <- cell[E](row, 4)
          f <- cell[F](row, 5)
          g <- cell[G](row, 6)
          h <- cell[H](row, 7)
          i <- cell[I](row, 8)
          j <- cell[J](row, 9)
          k <- cell[K](row, 10)
          l <- cell[L](row, 11)
          m <- cell[M](row, 12)
          n <- cell[N](row, 13)
          o <- cell[O](row, 14)
          p <- cell[P](row, 15)
          q <- cell[Q](row, 16)
          r <- cell[R](row, 17)
          s <- cell[S](row, 18)
          t <- cell[T](row, 19)
          u <- cell[U](row, 20)
          v <- cell[V](row, 21)
        yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  end tuple22
end RowDecoder
