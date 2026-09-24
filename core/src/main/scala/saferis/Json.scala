package saferis

import zio.json.*

/** A wrapper type for storing values as JSON in the database (JSONB in PostgreSQL, JSON in MySQL, etc.)
  *
  * Use this to wrap any type that has a JsonCodec to store it as JSON.
  *
  * Example:
  * {{{
  *   case class Metadata(tags: List[String], version: Int) derives JsonCodec
  *
  *   case class Event(
  *     @key id: Int,
  *     name: String,
  *     metadata: Json[Metadata]  // Stored as JSONB in PostgreSQL
  *   ) derives Table
  * }}}
  */
opaque type Json[A] = A

object Json:
  def apply[A](value: A)(using @scala.annotation.unused codec: JsonCodec[A]): Json[A] = value

  extension [A](json: Json[A]) def value(using @scala.annotation.unused codec: JsonCodec[A]): A = json

  given encoder[A: JsonCodec]: Encoder[Json[A]] with
    def sqlType: SqlType             = SqlType.Json
    def encode(a: Json[A]): SqlValue =
      SqlValue.Json(summon[JsonCodec[A]].encoder.encodeJson(a, None).toString)

  given decoder[A: JsonCodec]: Decoder[Json[A]] with
    def decode(value: SqlValue): Either[DecodeError, Json[A]] = value match
      case SqlValue.Json(json) =>
        summon[JsonCodec[A]].decoder.decodeJson(json).left.map(e => DecodeError(s"Failed to decode JSON: $e"))
      case SqlValue.Null(_) => Left(DecodeError("null value"))
      case other            => Left(DecodeError(s"expected jsonb, found ${other.productPrefix}"))

  given codec[A](using JsonCodec[A]): Codec[Json[A]] = new Codec[Json[A]]:
    val encoder: Encoder[Json[A]] = Json.encoder[A]
    val decoder: Decoder[Json[A]] = Json.decoder[A]

  given optionEncoder[A: JsonCodec]: Encoder[Option[Json[A]]] = Encoder.option[Json[A]]
  given optionDecoder[A: JsonCodec]: Decoder[Option[Json[A]]] = Decoder.option[Json[A]]
  given optionCodec[A: JsonCodec]: Codec[Option[Json[A]]]     = Codec.option[Json[A]]
end Json
