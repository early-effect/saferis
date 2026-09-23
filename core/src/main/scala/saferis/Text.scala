package saferis

/** An opaque type for unbounded text columns (maps to TEXT in PostgreSQL, LONGTEXT in MySQL, etc.)
  *
  * Use this instead of String when you need TEXT column type rather than VARCHAR(255).
  *
  * Example:
  * {{{
  *   case class Article(
  *     @key id: Int,
  *     title: String,          // VARCHAR(255)
  *     content: Text           // TEXT
  *   ) derives Table
  * }}}
  */
opaque type Text = String

object Text:
  def apply(value: String): Text = value

  extension (t: Text) def value: String = t

  given encoder: Encoder[Text] with
    def pgType: PgType            = PgType.Text
    def encode(a: Text): SqlValue = SqlValue.Text(a)

  given decoder: Decoder[Text] with
    def decode(value: SqlValue): Either[DecodeError, Text] = value match
      case SqlValue.VarChar(v) => Right(v)
      case SqlValue.Text(v)    => Right(v)
      case SqlValue.Null(_)    => Left(DecodeError("null value"))
      case other               => Left(DecodeError(s"expected text, found ${other.productPrefix}"))

  given codec: Codec[Text] = new Codec[Text]:
    val encoder: Encoder[Text] = Text.encoder
    val decoder: Decoder[Text] = Text.decoder

  given optionEncoder: Encoder[Option[Text]] = Encoder.option[Text]
  given optionDecoder: Decoder[Option[Text]] = Decoder.option[Text]
  given optionCodec: Codec[Option[Text]]     = Codec.option[Text]
end Text
