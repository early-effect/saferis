package saferis.mysql

import saferis.*

import java.util.UUID

/** MySQL-specific codecs for types that require different handling than PostgreSQL.
  *
  * Import these with `import saferis.mysql.{given}` to override the default PostgreSQL codecs.
  */

/** UUID encoder for MySQL. The bound value is text. Column DDL is `char(36)`, not `longtext`. */
given uuidEncoder: Encoder[UUID] with
  def pgType: PgType                             = PgType.Text
  def encode(uuid: UUID): SqlValue               = SqlValue.Text(uuid.toString)
  override def columnType(using Dialect): String = "char(36)"

/** UUID decoder for MySQL. Reads the text form, not `SqlValue.Uuid`. */
given uuidDecoder: Decoder[UUID] with
  def decode(value: SqlValue): Either[DecodeError, UUID] =
    def parse(text: String): Either[DecodeError, UUID] =
      try Right(UUID.fromString(text))
      catch case _: IllegalArgumentException => Left(DecodeError(s"invalid uuid text: $text"))
    value match
      case SqlValue.Text(text)    => parse(text)
      case SqlValue.VarChar(text) => parse(text)
      case SqlValue.Null(_)       => Left(DecodeError("null value"))
      case other                  => Left(DecodeError(s"expected uuid text, found ${other.productPrefix}"))
end uuidDecoder
