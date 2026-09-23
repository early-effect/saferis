package saferis

import zio.Chunk

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZonedDateTime
import java.util.UUID

trait Encoder[A]:
  self =>
  def pgType: PgType
  def encode(a: A): SqlValue
  def columnType(using dialect: Dialect): String = dialect.columnType(pgType)
  def literal(a: A): String                      = SqlValue.literal(encode(a))

  def transform[B](f: B => A): Encoder[B] =
    new Encoder[B]:
      def pgType: PgType                             = self.pgType
      def encode(b: B): SqlValue                     = self.encode(f(b))
      override def literal(b: B): String             = self.literal(f(b))
      override def columnType(using Dialect): String = self.columnType
end Encoder

object Encoder:
  given option[A](using encoder: Encoder[A]): Encoder[Option[A]] with
    def pgType: PgType                 = encoder.pgType
    def encode(a: Option[A]): SqlValue = a match
      case None    => SqlValue.Null(encoder.pgType)
      case Some(v) => encoder.encode(v)

  given string: Encoder[String] with
    def pgType: PgType              = PgType.VarChar
    def encode(a: String): SqlValue = SqlValue.VarChar(a)

  given short: Encoder[Short] with
    def pgType: PgType             = PgType.Int2
    def encode(a: Short): SqlValue = SqlValue.Int2(a)

  given int: Encoder[Int] with
    def pgType: PgType           = PgType.Int4
    def encode(a: Int): SqlValue = SqlValue.Int4(a)

  given long: Encoder[Long] with
    def pgType: PgType            = PgType.Int8
    def encode(a: Long): SqlValue = SqlValue.Int8(a)

  given boolean: Encoder[Boolean] with
    def pgType: PgType               = PgType.Bool
    def encode(a: Boolean): SqlValue = SqlValue.Bool(a)

  given float: Encoder[Float] with
    def pgType: PgType             = PgType.Float4
    def encode(a: Float): SqlValue = SqlValue.Float4(a)

  given double: Encoder[Double] with
    def pgType: PgType              = PgType.Float8
    def encode(a: Double): SqlValue = SqlValue.Float8(a)

  given bigDecimal: Encoder[BigDecimal] with
    def pgType: PgType                  = PgType.Numeric
    def encode(a: BigDecimal): SqlValue = SqlValue.Numeric(a)

  given bigInt: Encoder[BigInt] with
    def pgType: PgType              = PgType.Numeric
    def encode(a: BigInt): SqlValue = SqlValue.Numeric(BigDecimal(a))

  given chunkByte: Encoder[Chunk[Byte]] with
    def pgType: PgType                   = PgType.Bytea
    def encode(a: Chunk[Byte]): SqlValue = SqlValue.Bytea(a)

  given instant: Encoder[Instant] with
    def pgType: PgType               = PgType.Timestamptz
    def encode(a: Instant): SqlValue = SqlValue.Timestamptz(a)

  given localDateTime: Encoder[LocalDateTime] with
    def pgType: PgType                     = PgType.Timestamp
    def encode(a: LocalDateTime): SqlValue = SqlValue.Timestamp(a)

  given localDate: Encoder[LocalDate] with
    def pgType: PgType                 = PgType.Date
    def encode(a: LocalDate): SqlValue = SqlValue.Date(a)

  given localTime: Encoder[LocalTime] with
    def pgType: PgType                 = PgType.Time
    def encode(a: LocalTime): SqlValue = SqlValue.Time(a)

  given zonedDateTime: Encoder[ZonedDateTime] with
    def pgType: PgType                     = PgType.Timestamptz
    def encode(a: ZonedDateTime): SqlValue = SqlValue.Timestamptz(a.toInstant)

  given offsetDateTime: Encoder[OffsetDateTime] with
    def pgType: PgType                      = PgType.Timestamptz
    def encode(a: OffsetDateTime): SqlValue = SqlValue.Timestamptz(a.toInstant)

  given defaultUuidEncoder: Encoder[UUID] = postgres.uuidEncoder

  def fromJsonCodec[T](using codec: zio.json.JsonCodec[T]): Encoder[T] = new Encoder[T]:
    def pgType: PgType         = PgType.Jsonb
    def encode(a: T): SqlValue = SqlValue.Jsonb(codec.encoder.encodeJson(a, None).toString)
end Encoder
