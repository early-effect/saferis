package saferis

/** How a column participates in key generation. A compound key is never [[GeneratedKey.IdentityPrimaryKey]]: the
  * primary key is a table constraint, so the column is [[GeneratedKey.Identity]] or [[GeneratedKey.Plain]].
  */
enum GeneratedKey:
  case Plain
  case PrimaryKey
  case Identity
  case IdentityPrimaryKey

object GeneratedKey:
  def column(isGenerated: Boolean, isKey: Boolean, compound: Boolean): GeneratedKey =
    if compound then if isGenerated then GeneratedKey.Identity else GeneratedKey.Plain
    else if isGenerated && isKey then GeneratedKey.IdentityPrimaryKey
    else if isGenerated then GeneratedKey.Identity
    else if isKey then GeneratedKey.PrimaryKey
    else GeneratedKey.Plain

/** Trait representing a database dialect that provides database-specific type mappings and SQL generation.
  *
  * This allows the library to support multiple databases by providing different implementations for each database's
  * specific requirements including column types, auto-increment behavior, index creation, and other SQL syntax
  * variations.
  */
trait Dialect:

  /** DDL spelling of a `SqlType` for this dialect. */
  def columnType(tpe: SqlType): ColumnType

  /** Database name/identifier */
  def name: DialectName

  /** Default length for variable-length types like VARCHAR */
  val DefaultVarcharLength: Int = 255

  /** The column clause for this [[GeneratedKey]]. A dialect with no identity or primary-key syntax returns empty SQL.
    */
  def generatedKey(key: GeneratedKey): SqlText

  /** Returns the SQL clause for primary key constraint on a single column.
    *
    * @return
    *   SQL clause for primary key (e.g., "PRIMARY KEY")
    */
  def primaryKeyClause: SqlText = SqlText("primary key")

  /** Returns the SQL for a compound primary key constraint.
    *
    * @param columnNames
    *   The column names that make up the compound key
    * @return
    *   SQL constraint clause
    */
  def compoundPrimaryKeyClause(columnNames: Seq[ColumnName]): SqlText =
    SqlText(s"primary key (${columnNames.map(escapeIdentifier).mkString(", ")})")

  // === Index Creation ===

  /** Returns the SQL for creating a regular index.
    *
    * @param indexName
    *   Name of the index
    * @param tableName
    *   Name of the table
    * @param columnNames
    *   Column names to index
    * @param ifNotExists
    *   Whether to include IF NOT EXISTS clause
    * @return
    *   SQL statement for creating the index
    */
  def createIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None,
  ): SqlText =
    val ifNotExistsClause = if ifNotExists then " if not exists" else ""
    val whereClause       = where.map(w => s" where $w").getOrElse("")
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    SqlText(
      s"create index$ifNotExistsClause ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})$whereClause"
    )
  end createIndexSql

  /** Returns the SQL for creating a unique index.
    *
    * @param indexName
    *   Name of the index
    * @param tableName
    *   Name of the table
    * @param columnNames
    *   Column names to index
    * @param ifNotExists
    *   Whether to include IF NOT EXISTS clause
    * @return
    *   SQL statement for creating the unique index
    */
  def createUniqueIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None,
  ): SqlText =
    val ifNotExistsClause = if ifNotExists then " if not exists" else ""
    val whereClause       = where.map(w => s" where $w").getOrElse("")
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    SqlText(
      s"create unique index$ifNotExistsClause ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})$whereClause"
    )
  end createUniqueIndexSql

  /** Returns the SQL for dropping an index.
    *
    * @param indexName
    *   Name of the index to drop
    * @param ifExists
    *   Whether to include IF EXISTS clause
    * @return
    *   SQL statement for dropping the index
    */
  def dropIndexSql(indexName: IndexName, ifExists: Boolean = false): SqlText =
    val ifExistsClause = if ifExists then " if exists" else ""
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    SqlText(s"drop index$ifExistsClause ${escapeIdentifier(indexName)}")

  // === Table Operations ===

  /** Returns the SQL for creating a table with IF NOT EXISTS clause.
    *
    * @param ifNotExists
    *   Whether to include IF NOT EXISTS
    * @return
    *   SQL clause for table creation
    */
  def createTableClause(ifNotExists: Boolean): SqlText =
    val ifNotExistsClause = if ifNotExists then " if not exists" else ""
    SqlText(s"create table$ifNotExistsClause")

  /** Returns the SQL for dropping a table.
    *
    * @param tableName
    *   Name of the table to drop
    * @param ifExists
    *   Whether to include IF EXISTS clause
    * @return
    *   SQL statement for dropping the table
    */
  def dropTableSql(tableName: TableName, ifExists: Boolean): SqlText =
    val ifExistsClause = if ifExists then " if exists" else ""
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- identifiers are escaped via escapeIdentifier (identifiers cannot be bind parameters)
    SqlText(s"drop table$ifExistsClause ${escapeIdentifier(tableName)}")

  /** Returns the SQL for truncating a table.
    *
    * @param tableName
    *   Name of the table to truncate
    * @return
    *   SQL statement for truncating the table
    */
  def truncateTableSql(tableName: TableName): SqlText = SqlText(s"truncate table ${escapeIdentifier(tableName)}")

  // === Column Operations ===

  /** Returns the SQL for adding a column to a table.
    *
    * @param tableName
    *   Name of the table
    * @param columnName
    *   Name of the new column
    * @param columnType
    *   Type of the new column
    * @return
    *   SQL statement for adding the column
    */
  def addColumnSql(tableName: TableName, columnName: ColumnName, columnType: ColumnType): SqlText =
    SqlText(s"alter table ${escapeIdentifier(tableName)} add column ${escapeIdentifier(columnName)} $columnType")

  /** Returns the SQL for dropping a column from a table.
    *
    * @param tableName
    *   Name of the table
    * @param columnName
    *   Name of the column to drop
    * @return
    *   SQL statement for dropping the column
    */
  def dropColumnSql(tableName: TableName, columnName: ColumnName): SqlText =
    SqlText(s"alter table ${escapeIdentifier(tableName)} drop column ${escapeIdentifier(columnName)}")

  // === Query Features ===

  /** Quote character for identifiers (table names, column names, etc.).
    *
    * @return
    *   Quote character (e.g., '"' for PostgreSQL, '`' for MySQL)
    */
  def identifierQuote: String = "\""

  /** Escapes an identifier (table name, column name, etc.) using the database's quoting rules.
    *
    * @param identifier
    *   The identifier to escape
    * @return
    *   Escaped identifier
    */
  def escapeIdentifier[A](name: A)(using sqlName: SqlName[A]): SqlText =
    Dialect.quote(identifierQuote, sqlName.text(name), sqlName.qualify)

end Dialect

object Dialect:
  /** Wrap `name` in `identifierQuote`, doubling any quote already inside it.
    *
    * When `qualify` is set, a dot separates identifiers (`schema.table` becomes `"schema"."table"`).
    */
  def quote(identifierQuote: String, name: String, qualify: Boolean = false): SqlText =
    if qualify && name.contains('.') then
      SqlText(name.split("\\.", -1).map(part => quoteOne(identifierQuote, part)).mkString("."))
    else SqlText(quoteOne(identifierQuote, name))

  private def quoteOne(identifierQuote: String, name: String): String =
    val escaped = name.replace(identifierQuote, identifierQuote + identifierQuote)
    s"$identifierQuote$escaped$identifierQuote"

  /** Default PostgreSQL dialect - provided as a low priority given. This allows users to work with Postgres out of the
    * box with just `import saferis.*` Users can override this by providing their own given Dialect with higher
    * priority.
    *
    * Using the singleton type `PostgresDialect.type` ensures this given satisfies all capability intersection types
    * like `Dialect & UpsertSupport & ReturningSupport`.
    */
  given defaultDialect: postgres.PostgresDialect.type = postgres.PostgresDialect
end Dialect
