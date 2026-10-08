package saferis.postgres

import saferis.*

import java.util.UUID

/** PostgreSQL UUID encoder. The bound value is `SqlValue.Uuid`. `Encoder.defaultUuidEncoder` points here. */
given uuidEncoder: Encoder[UUID] with
  def sqlType: SqlType             = SqlType.Uuid
  def encode(uuid: UUID): SqlValue = SqlValue.Uuid(uuid)
