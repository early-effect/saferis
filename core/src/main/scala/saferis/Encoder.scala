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
    def sqlType: SqlType           = SqlType.Int2
    def encode(a: Short): SqlValue = SqlValue.Int2(a)

  given int: Encoder[Int] with
    def sqlType: SqlType         = SqlType.Int4
    def encode(a: Int): SqlValue = SqlValue.Int4(a)

  given long: Encoder[Long] with
    def sqlType: SqlType          = SqlType.Int8
    def encode(a: Long): SqlValue = SqlValue.Int8(a)

  given boolean: Encoder[Boolean] with
    def sqlType: SqlType             = SqlType.Bool
    def encode(a: Boolean): SqlValue = SqlValue.Bool(a)

  given float: Encoder[Float] with
    def sqlType: SqlType           = SqlType.Float4
    def encode(a: Float): SqlValue = SqlValue.Float4(a)

  given double: Encoder[Double] with
    def sqlType: SqlType            = SqlType.Float8
    def encode(a: Double): SqlValue = SqlValue.Float8(a)

  given bigDecimal: Encoder[BigDecimal] with
    def sqlType: SqlType                = SqlType.Numeric
    def encode(a: BigDecimal): SqlValue = SqlValue.Numeric(a)

  given bigInt: Encoder[BigInt] with
    def sqlType: SqlType            = SqlType.Numeric
    def encode(a: BigInt): SqlValue = SqlValue.Numeric(BigDecimal(a))

  given chunkByte: Encoder[Chunk[Byte]] with
    def sqlType: SqlType                 = SqlType.Bytea
    def encode(a: Chunk[Byte]): SqlValue = SqlValue.Bytea(a)

  given instant: Encoder[Instant] with
    def sqlType: SqlType             = SqlType.Timestamptz
    def encode(a: Instant): SqlValue = SqlValue.Timestamptz(a)

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
    def sqlType: SqlType                   = SqlType.Timestamptz
    def encode(a: ZonedDateTime): SqlValue = SqlValue.Timestamptz(a.toInstant)

  given offsetDateTime: Encoder[OffsetDateTime] with
    def sqlType: SqlType                    = SqlType.Timestamptz
    def encode(a: OffsetDateTime): SqlValue = SqlValue.Timestamptz(a.toInstant)

  given defaultUuidEncoder: Encoder[UUID] = postgres.uuidEncoder

  def array[A](using element: Encoder[A]): Encoder[Chunk[A]] = new Encoder[Chunk[A]]:
    def sqlType: SqlType                   = SqlType.Array(element.sqlType)
    def encode(values: Chunk[A]): SqlValue =
      val members = values.map(element.encode)
      SqlValue.array(element.sqlType, members).getOrElse(SqlValue.Array(element.sqlType, members))

  def fromJsonCodec[T](using codec: zio.json.JsonCodec[T]): Encoder[T] = new Encoder[T]:
    def sqlType: SqlType       = SqlType.Jsonb
    def encode(a: T): SqlValue = SqlValue.Jsonb(codec.encoder.encodeJson(a, None).toString)
end Encoder
