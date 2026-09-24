package saferis

import zio.Chunk

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZonedDateTime
import java.util.UUID

trait Codec[A] extends Encoder[A], Decoder[A]:
  self =>
  val encoder: Encoder[A]
  val decoder: Decoder[A]
  def sqlType: SqlType                                = encoder.sqlType
  def encode(a: A): SqlValue                          = encoder.encode(a)
  def decode(value: SqlValue): Either[DecodeError, A] = decoder.decode(value)
  override def literal(a: A): String                  = encoder.literal(a)
  override def columnType(using Dialect): String      = encoder.columnType

  def transform[B](map: A => Either[DecodeError, B])(contramap: B => A): Codec[B] =
    new Codec[B]:
      val encoder: Encoder[B] = self.encoder.transform(contramap)
      val decoder: Decoder[B] = self.decoder.transform(map)
end Codec

object Codec:
  private def make[A](enc: Encoder[A], dec: Decoder[A]): Codec[A] = new Codec[A]:
    val encoder: Encoder[A] = enc
    val decoder: Decoder[A] = dec

  def apply[A](using encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = make(encoder, decoder)

  given string: Codec[String]                 = make(Encoder.string, Decoder.string)
  given short: Codec[Short]                   = make(Encoder.short, Decoder.short)
  given int: Codec[Int]                       = make(Encoder.int, Decoder.int)
  given long: Codec[Long]                     = make(Encoder.long, Decoder.long)
  given boolean: Codec[Boolean]               = make(Encoder.boolean, Decoder.boolean)
  given float: Codec[Float]                   = make(Encoder.float, Decoder.float)
  given double: Codec[Double]                 = make(Encoder.double, Decoder.double)
  given bigDecimal: Codec[BigDecimal]         = make(Encoder.bigDecimal, Decoder.bigDecimal)
  given bigInt: Codec[BigInt]                 = make(Encoder.bigInt, Decoder.bigInt)
  given chunkByte: Codec[Chunk[Byte]]         = make(Encoder.chunkByte, Decoder.chunkByte)
  given instant: Codec[Instant]               = make(Encoder.instant, Decoder.instant)
  given localDateTime: Codec[LocalDateTime]   = make(Encoder.localDateTime, Decoder.localDateTime)
  given localDate: Codec[LocalDate]           = make(Encoder.localDate, Decoder.localDate)
  given localTime: Codec[LocalTime]           = make(Encoder.localTime, Decoder.localTime)
  given zonedDateTime: Codec[ZonedDateTime]   = make(Encoder.zonedDateTime, Decoder.zonedDateTime)
  given offsetDateTime: Codec[OffsetDateTime] = make(Encoder.offsetDateTime, Decoder.offsetDateTime)

  given codec[A](using enc: Encoder[A], dec: Decoder[A]): Codec[A] with
    val encoder: Encoder[A] = enc
    val decoder: Decoder[A] = dec

  given option[A](using c: Codec[A]): Codec[Option[A]] with
    val encoder: Encoder[Option[A]] = Encoder.option[A](using c.encoder)
    val decoder: Decoder[Option[A]] = Decoder.option[A](using c.decoder)

  given defaultUuidCodec: Codec[UUID] =
    make(Encoder.defaultUuidEncoder, Decoder.defaultUuidDecoder)
end Codec
