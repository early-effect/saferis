package saferis

import zio.Chunk

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.util.UUID

final case class DecodeError(detail: String)

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
  private def reject(expected: String, value: SqlValue): Left[DecodeError, Nothing] =
    value match
      case SqlValue.Null(_) => Left(DecodeError("null value"))
      case other            => Left(DecodeError(s"expected $expected, found ${other.productPrefix}"))

  given option[A](using decoder: Decoder[A]): Decoder[Option[A]] with
    def decode(value: SqlValue): Either[DecodeError, Option[A]] = value match
      case SqlValue.Null(_) => Right(None)
      case other            => decoder.decode(other).map(Some(_))

  given string: Decoder[String] with
    def decode(value: SqlValue): Either[DecodeError, String] = value match
      case SqlValue.VarChar(v)  => Right(v)
      case SqlValue.Text(v)     => Right(v)
      case SqlValue.Other(_, v) => Right(v)
      case other                => reject("varchar", other)

  given short: Decoder[Short] with
    def decode(value: SqlValue): Either[DecodeError, Short] = value match
      case SqlValue.Int2(v) => Right(v)
      case other            => reject("int2", other)

  given int: Decoder[Int] with
    def decode(value: SqlValue): Either[DecodeError, Int] = value match
      case SqlValue.Int4(v) => Right(v)
      case other            => reject("int4", other)

  given long: Decoder[Long] with
    def decode(value: SqlValue): Either[DecodeError, Long] = value match
      case SqlValue.Int8(v) => Right(v)
      case other            => reject("int8", other)

  given boolean: Decoder[Boolean] with
    def decode(value: SqlValue): Either[DecodeError, Boolean] = value match
      case SqlValue.Bool(v) => Right(v)
      case other            => reject("bool", other)

  given float: Decoder[Float] with
    def decode(value: SqlValue): Either[DecodeError, Float] = value match
      case SqlValue.Float4(v) => Right(v)
      case other              => reject("float4", other)

  given double: Decoder[Double] with
    def decode(value: SqlValue): Either[DecodeError, Double] = value match
      case SqlValue.Float8(v) => Right(v)
      case other              => reject("float8", other)

  given bigDecimal: Decoder[BigDecimal] with
    def decode(value: SqlValue): Either[DecodeError, BigDecimal] = value match
      case SqlValue.Numeric(v) => Right(v)
      case other               => reject("numeric", other)

  given bigInt: Decoder[BigInt] with
    def decode(value: SqlValue): Either[DecodeError, BigInt] = value match
      case SqlValue.Numeric(v) => Right(v.toBigInt)
      case other               => reject("numeric", other)

  given chunkByte: Decoder[Chunk[Byte]] with
    def decode(value: SqlValue): Either[DecodeError, Chunk[Byte]] = value match
      case SqlValue.Bytea(v) => Right(v)
      case other             => reject("bytea", other)

  given instant: Decoder[Instant] with
    def decode(value: SqlValue): Either[DecodeError, Instant] = value match
      case SqlValue.Timestamptz(v) => Right(v)
      case other                   => reject("timestamptz", other)

  given localDateTime: Decoder[LocalDateTime] with
    def decode(value: SqlValue): Either[DecodeError, LocalDateTime] = value match
      case SqlValue.Timestamp(v) => Right(v)
      case other                 => reject("timestamp", other)

  given localDate: Decoder[LocalDate] with
    def decode(value: SqlValue): Either[DecodeError, LocalDate] = value match
      case SqlValue.Date(v) => Right(v)
      case other            => reject("date", other)

  given localTime: Decoder[LocalTime] with
    def decode(value: SqlValue): Either[DecodeError, LocalTime] = value match
      case SqlValue.Time(v) => Right(v)
      case other            => reject("time", other)

  given zonedDateTime: Decoder[ZonedDateTime] with
    def decode(value: SqlValue): Either[DecodeError, ZonedDateTime] = value match
      case SqlValue.Timestamptz(v) => Right(ZonedDateTime.ofInstant(v, ZoneOffset.UTC))
      case other                   => reject("timestamptz", other)

  given offsetDateTime: Decoder[OffsetDateTime] with
    def decode(value: SqlValue): Either[DecodeError, OffsetDateTime] = value match
      case SqlValue.Timestamptz(v) => Right(OffsetDateTime.ofInstant(v, ZoneOffset.UTC))
      case other                   => reject("timestamptz", other)

  given defaultUuidDecoder: Decoder[UUID] = postgres.uuidDecoder

  def array[A](using element: Decoder[A]): Decoder[Chunk[A]] = new Decoder[Chunk[A]]:
    def decode(value: SqlValue): Either[DecodeError, Chunk[A]] = value match
      case SqlValue.Array(_, values) =>
        values.foldLeft[Either[DecodeError, Chunk[A]]](Right(Chunk.empty)):
          case (Left(err), _)       => Left(err)
          case (Right(acc), member) => element.decode(member).map(acc :+ _)
      case other => reject("array", other)

  def fromJsonCodec[T](using codec: zio.json.JsonCodec[T]): Decoder[T] =
    new Decoder[T]:
      def decode(value: SqlValue): Either[DecodeError, T] = value match
        case SqlValue.Jsonb(json) =>
          codec.decoder.decodeJson(json).left.map(e => DecodeError(s"Failed to decode JSON: $e"))
        case SqlValue.Null(_) => Left(DecodeError("null value"))
        case other            => Left(DecodeError(s"expected jsonb, found ${other.productPrefix}"))

end Decoder

object RowDecoder:
  given rowFromCell[A](using cell: Decoder[A]): RowDecoder[A] with
    def decode(row: SqlRow): Either[DecodeError, A] =
      row.at(0).flatMap(cell.decode)

  private def cell[A](row: SqlRow, index: Int)(using decoder: Decoder[A]): Either[DecodeError, A] =
    row.at(index).flatMap(decoder.decode).left.map(err => DecodeError(s"column ${index + 1}: ${err.detail}"))

  private def width(row: SqlRow, expected: Int): Either[DecodeError, Unit] =
    if row.width == expected then Right(())
    else Left(DecodeError(s"Expected exactly $expected columns in result set, got ${row.width}"))

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
