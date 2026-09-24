package saferis

import zio.Chunk

import scala.compiletime.constValueTuple
import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.Mirror

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

  /** One column of `element` values. `Chunk[Byte]` stays `bytea` via [[Encoder.chunkByte]]. Summon this explicitly. */
  def array[A](using element: Codec[A]): Codec[Chunk[A]] =
    make(Encoder.array(using element.encoder), Decoder.array(using element.decoder))

  /** Postgres enum as [[SqlValue.Other]] text. The server type name is not matched: drivers disagree on name versus
    * OID.
    */
  def pgEnum[E](typeName: String)(encodeName: E => String, decodeName: String => Option[E]): Codec[E] =
    val server = ServerType.Named(typeName)
    new Codec[E]:
      val encoder: Encoder[E] = new Encoder[E]:
        def sqlType: SqlType       = SqlType.Other(server)
        def encode(a: E): SqlValue = SqlValue.Other(server, encodeName(a))
      val decoder: Decoder[E] = new Decoder[E]:
        def decode(value: SqlValue): Either[DecodeError, E] =
          val text = value match
            case SqlValue.Other(_, t) => Some(t)
            case SqlValue.Text(t)     => Some(t)
            case SqlValue.VarChar(t)  => Some(t)
            case _                    => None
          text.flatMap(decodeName).toRight(DecodeError(s"not a $typeName"))
    end new
  end pgEnum

  /** Parameterless Scala 3 enum. Case names are the Postgres labels. */
  inline def pgEnum[E](typeName: String)(using m: Mirror.SumOf[E]): Codec[E] =
    val labels = constValueTuple[m.MirroredElemLabels].productIterator.map(_.asInstanceOf[String]).toVector
    val values = enumValues[m.MirroredElemTypes].asInstanceOf[Vector[E]]
    pgEnum(typeName)(
      (e: E) => labels(m.ordinal(e)),
      (text: String) =>
        val index = labels.indexOf(text)
        if index < 0 then None else Some(values(index)),
    )

  private inline def enumValues[T <: Tuple]: Vector[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple     => Vector.empty
      case _: (head *: tail) =>
        Vector(summonInline[ValueOf[head]].value) ++ enumValues[tail]
end Codec
