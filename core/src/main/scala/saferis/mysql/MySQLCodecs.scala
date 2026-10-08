package saferis.mysql

import saferis.*

import java.util.UUID

/** MySQL-specific codecs for types that require different handling than PostgreSQL.
  *
  * Import these with `import saferis.mysql.{given}` to override the default PostgreSQL encoder. UUID decode is shared:
  * it reads `SqlValue.Uuid` and canonical UUID text.
  */

/** UUID encoder for MySQL. The bound value is text. Column DDL is `char(36)`, not `longtext`. */
given uuidEncoder: Encoder[UUID] with
  def sqlType: SqlType                               = SqlType.Text
  def encode(uuid: UUID): SqlValue                   = SqlValue.Text(uuid.toString)
  override def columnType(using Dialect): ColumnType = ColumnType("char(36)")
