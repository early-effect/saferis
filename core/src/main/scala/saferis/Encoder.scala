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
  def sqlType: SqlType
  def encode(a: A): SqlValue
  def columnType(using dialect: Dialect): String = dialect.columnType(sqlType)
  def literal(a: A): String                      = SqlValue.literal(encode(a))

  def transform[B](f: B => A): Encoder[B] =
    new Encoder[B]:
      def sqlType: SqlType                           = self.sqlType
      def encode(b: B): SqlValue                     = self.encode(f(b))
      override def literal(b: B): String             = self.literal(f(b))
      override def columnType(using Dialect): String = self.columnType
end Encoder

object Encoder:
  given option[A](using encoder: Encoder[A]): Encoder[Option[A]] with
    def sqlType: SqlType               = encoder.sqlType
    def encode(a: Option[A]): SqlValue = a match
      case None    => SqlValue.Null(encoder.sqlType)
      case Some(v) => encoder.encode(v)

  given string: Encoder[String] with
    def sqlType: SqlType            = SqlType.VarChar
    def encode(a: String): SqlValue = SqlValue.VarChar(a)

  given short: Encoder[Short] with
    def sqlType: SqlType           = SqlType.SmallInt
    def encode(a: Short): SqlValue = SqlValue.SmallInt(a)

  given int: Encoder[Int] with
    def sqlType: SqlType         = SqlType.Integer
    def encode(a: Int): SqlValue = SqlValue.Integer(a)

  given long: Encoder[Long] with
    def sqlType: SqlType          = SqlType.BigInt
    def encode(a: Long): SqlValue = SqlValue.BigInt(a)

  given boolean: Encoder[Boolean] with
    def sqlType: SqlType             = SqlType.Bool
    def encode(a: Boolean): SqlValue = SqlValue.Bool(a)

  given float: Encoder[Float] with
    def sqlType: SqlType           = SqlType.Real
    def encode(a: Float): SqlValue = SqlValue.Real(a)

  given double: Encoder[Double] with
    def sqlType: SqlType            = SqlType.DoublePrecision
    def encode(a: Double): SqlValue = SqlValue.DoublePrecision(a)

  given bigDecimal: Encoder[BigDecimal] with
    def sqlType: SqlType                = SqlType.Numeric
    def encode(a: BigDecimal): SqlValue = SqlValue.Numeric(a)

  given bigInt: Encoder[BigInt] with
    def sqlType: SqlType            = SqlType.Numeric
    def encode(a: BigInt): SqlValue = SqlValue.Numeric(BigDecimal(a))

  given chunkByte: Encoder[Chunk[Byte]] with
    def sqlType: SqlType                 = SqlType.Binary
    def encode(a: Chunk[Byte]): SqlValue = SqlValue.Binary(a)

  given instant: Encoder[Instant] with
    def sqlType: SqlType             = SqlType.TimestampTz
    def encode(a: Instant): SqlValue = SqlValue.TimestampTz(a)

  given localDateTime: Encoder[LocalDateTime] with
    def sqlType: SqlType                   = SqlType.Timestamp
    def encode(a: LocalDateTime): SqlValue = SqlValue.Timestamp(a)

  given localDate: Encoder[LocalDate] with
    def sqlType: SqlType               = SqlType.Date
    def encode(a: LocalDate): SqlValue = SqlValue.Date(a)

  given localTime: Encoder[LocalTime] with
    def sqlType: SqlType               = SqlType.Time
    def encode(a: LocalTime): SqlValue = SqlValue.Time(a)

  given zonedDateTime: Encoder[ZonedDateTime] with
    def sqlType: SqlType                   = SqlType.TimestampTz
    def encode(a: ZonedDateTime): SqlValue = SqlValue.TimestampTz(a.toInstant)

  given offsetDateTime: Encoder[OffsetDateTime] with
    def sqlType: SqlType                    = SqlType.TimestampTz
    def encode(a: OffsetDateTime): SqlValue = SqlValue.TimestampTz(a.toInstant)

  given defaultUuidEncoder: Encoder[UUID] = postgres.uuidEncoder

  def fromJsonCodec[T](using codec: zio.json.JsonCodec[T]): Encoder[T] = new Encoder[T]:
    def sqlType: SqlType       = SqlType.Json
    def encode(a: T): SqlValue = SqlValue.Json(codec.encoder.encodeJson(a, None).toString)
end Encoder
