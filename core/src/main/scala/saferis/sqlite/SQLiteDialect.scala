package saferis.sqlite

import saferis.*

/** SQLite dialect implementation providing SQLite-specific type mappings and SQL generation.
  *
  * SQLite characteristics:
  *   - Uses AUTOINCREMENT for auto-increment columns
  *   - Supports IF NOT EXISTS for tables but not for all operations
  *   - Uses double quotes for identifier escaping
  *   - Has a unique type affinity system (simplified here)
  */
object SQLiteDialect extends Dialect with ReturningSupport with CommonTableExpressionSupport with WindowFunctionSupport:

  val name: String = "SQLite"

  // === Type Mappings ===
  def columnType(tpe: PgType): String = tpe match
    case PgType.Bool                                    => "integer"
    case PgType.Int2 | PgType.Int4 | PgType.Int8        => "integer"
    case PgType.Float4 | PgType.Float8 | PgType.Numeric => "real"
    case PgType.Bytea                                   => "blob"
    case PgType.VarChar | PgType.Text | PgType.Date | PgType.Time | PgType.Timestamp | PgType.Timestamptz |
        PgType.Jsonb | PgType.Uuid =>
      "text"

  // === Auto-increment Syntax ===
  override def autoIncrementClause(isGenerated: Boolean, isKey: Boolean, hasDefault: Boolean): String =
    (isGenerated, isKey, hasDefault) match
      case (true, true, false)  => " primary key autoincrement"
      case (true, true, true)   => " autoincrement"
      case (false, true, false) => " primary key"
      case (true, false, false) => " autoincrement"
      case _                    => ""

  // === Table Operations ===
  override def addColumnSql(tableName: String, columnName: String, columnDefinition: String): String =
    s"alter table ${escapeIdentifier(tableName)} add column $columnDefinition"

  override def dropColumnSql(tableName: String, columnName: String): String =
    // SQLite has limited support for dropping columns - requires recreating table
    throw new UnsupportedOperationException(
      "SQLite does not support dropping columns directly. Use PRAGMA table_info and recreate table."
    )

  // === Index Operations ===
  // SQLite supports partial indexes (WHERE clause)
  override def createIndexSql(
      indexName: String,
      tableName: String,
      columnNames: Seq[String],
      ifNotExists: Boolean = true,
      where: Option[String] = None,
  ): String =
    val ifNotExistsClause = if ifNotExists then " if not exists" else ""
    val columns           = columnNames.map(escapeIdentifier).mkString(", ")
    val whereClause       = where.map(w => s" where $w").getOrElse("")
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    s"create index$ifNotExistsClause ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} ($columns)$whereClause"
  end createIndexSql

  override def dropIndexSql(indexName: String, ifExists: Boolean = false): String =
    val ifExistsClause = if ifExists then "if exists " else ""
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    s"drop index $ifExistsClause${escapeIdentifier(indexName)}"

  // === SQLite-specific Query Features ===
  override def identifierQuote: String = "\""

  // === SQLite-specific Table Operations ===
  override def truncateTableSql(tableName: String): String =
    // SQLite doesn't have TRUNCATE, use DELETE instead
    s"delete from ${escapeIdentifier(tableName)}"

  // ReturningSupport uses default implementations since SQLite supports RETURNING

end SQLiteDialect

// Export SQLiteDialect with its singleton type so all capability intersections are satisfied
given SQLiteDialect.type = SQLiteDialect
