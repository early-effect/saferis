package saferis.postgres

import saferis.*

import java.util.UUID

/** PostgreSQL-specific codecs for types that have native database support.
  *
  * These are automatically available when using `import saferis.*` since PostgreSQL is the default dialect.
  */

given uuidEncoder: Encoder[UUID] with
  def pgType: PgType               = PgType.Uuid
  def encode(uuid: UUID): SqlValue = SqlValue.Uuid(uuid)

/** UUID decoder for PostgreSQL. Reads `SqlValue.Uuid` only. */
given uuidDecoder: Decoder[UUID] with
  def decode(value: SqlValue): Either[DecodeError, UUID] = value match
    case SqlValue.Uuid(uuid) => Right(uuid)
    case SqlValue.Null(_)    => Left(DecodeError("null value"))
    case other               => Left(DecodeError(s"expected uuid, found ${other.productPrefix}"))
