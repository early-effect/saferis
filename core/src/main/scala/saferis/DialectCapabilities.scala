package saferis

/** Trait for dialects that support the RETURNING clause in INSERT/UPDATE/DELETE operations */
trait ReturningSupport:
  self: Dialect =>

  /** Returns the SQL for an INSERT statement with RETURNING clause.
    *
    * @param tableName
    *   Name of the table
    * @param insertColumns
    *   Columns being inserted
    * @param returningColumns
    *   Columns to return
    * @return
    *   SQL fragment for INSERT ... RETURNING
    */
  def insertReturningSql(tableName: SqlText, insertColumns: SqlText, returningColumns: SqlText): SqlText =
    SqlText(s"insert into $tableName $insertColumns returning $returningColumns")

  /** Returns the SQL for an UPDATE statement with RETURNING clause.
    *
    * @param tableName
    *   Name of the table
    * @param setClause
    *   SET clause for the update
    * @param whereClause
    *   WHERE clause for the update
    * @param returningColumns
    *   Columns to return
    * @return
    *   SQL fragment for UPDATE ... RETURNING
    */
  def updateReturningSql(
      tableName: SqlText,
      setClause: SqlText,
      whereClause: SqlText,
      returningColumns: SqlText,
  ): SqlText =
    SqlText(s"update $tableName set $setClause where $whereClause returning $returningColumns")

  /** Returns the SQL for a DELETE statement with RETURNING clause.
    *
    * @param tableName
    *   Name of the table
    * @param whereClause
    *   WHERE clause for the delete
    * @param returningColumns
    *   Columns to return
    * @return
    *   SQL fragment for DELETE ... RETURNING
    */
  def deleteReturningSql(tableName: SqlText, whereClause: SqlText, returningColumns: SqlText): SqlText =
    SqlText(s"delete from $tableName where $whereClause returning $returningColumns")
end ReturningSupport

/** Trait for dialects that support IF NOT EXISTS in index creation */
trait IndexIfNotExistsSupport:
  self: Dialect =>

  /** Creates an index with IF NOT EXISTS support.
    *
    * @param indexName
    *   Name of the index
    * @param tableName
    *   Name of the table
    * @param columnNames
    *   Column names to index
    * @param unique
    *   Whether the index should be unique
    * @param where
    *   Optional WHERE clause for partial indexes
    * @return
    *   SQL statement for creating the index with IF NOT EXISTS
    */
  def createIndexIfNotExistsSql(
      indexName: SqlText,
      tableName: SqlText,
      columnNames: Seq[SqlText],
      unique: Boolean = false,
      where: Option[SqlText] = None,
  ): SqlText =
    val uniqueClause = if unique then "unique " else ""
    val whereClause  = where.map(w => s" where $w").getOrElse("")
    // nosemgrep: scala-security.scala.lang.security.audit.tainted-sql-string -- callers supply already-safe identifiers: the public SpecializedDML.createIndexIfNotExists escapes user input at the trust boundary, and internal callers pass compile-time schema-derived labels
    SqlText(
      s"create ${uniqueClause}index if not exists $indexName on $tableName (${columnNames.mkString(", ")})$whereClause"
    )
  end createIndexIfNotExistsSql
end IndexIfNotExistsSupport

/** Trait for dialects that support advanced ALTER TABLE operations */
trait AdvancedAlterTableSupport:
  self: Dialect =>

  /** Returns SQL for renaming a column.
    *
    * @param tableName
    *   Name of the table
    * @param oldColumnName
    *   Current column name
    * @param newColumnName
    *   New column name
    * @return
    *   SQL statement for renaming the column
    */
  def renameColumnSql(tableName: SqlText, oldColumnName: SqlText, newColumnName: SqlText): SqlText =
    SqlText(s"alter table $tableName rename column $oldColumnName to $newColumnName")

  /** Returns SQL for modifying a column type.
    *
    * @param tableName
    *   Name of the table
    * @param columnName
    *   Name of the column
    * @param newColumnType
    *   New column type
    * @return
    *   SQL statement for modifying the column type
    */
  def modifyColumnTypeSql(tableName: SqlText, columnName: SqlText, newColumnType: ColumnType): SqlText =
    SqlText(s"alter table $tableName alter column $columnName type $newColumnType")
end AdvancedAlterTableSupport

/** Trait for dialects that support UPSERT operations */
trait UpsertSupport:
  self: Dialect =>

  /** Returns SQL for an UPSERT operation.
    *
    * @param tableName
    *   Name of the table
    * @param insertColumns
    *   Columns for insertion
    * @param conflictColumns
    *   Columns that define the conflict
    * @param updateColumns
    *   Columns to update on conflict
    * @return
    *   SQL statement for UPSERT
    */
  def upsertSql(
      tableName: SqlText,
      insertColumns: SqlText,
      conflictColumns: Seq[SqlText],
      updateColumns: SqlText,
  ): SqlText

  /** Returns SQL for an UPSERT operation with a WHERE clause on the conflict update.
    *
    * Used for conditional upserts like: `ON CONFLICT (id) DO UPDATE SET ... WHERE expires_at < now()`
    *
    * @param tableName
    *   Name of the table
    * @param insertColumns
    *   Columns for insertion
    * @param conflictColumns
    *   Columns that define the conflict
    * @param updateColumns
    *   Columns to update on conflict
    * @param conflictWhere
    *   Optional WHERE clause for the DO UPDATE (conditions for when to actually update)
    * @return
    *   SQL statement for conditional UPSERT
    */
  def upsertWithWhereSql(
      tableName: SqlText,
      insertColumns: SqlText,
      conflictColumns: Seq[SqlText],
      updateColumns: SqlText,
      conflictWhere: Option[SqlText],
  ): SqlText =
    val baseUpsert  = upsertSql(tableName, insertColumns, conflictColumns, updateColumns)
    val whereClause = conflictWhere.map(w => s" where $w").getOrElse("")
    SqlText(baseUpsert + whereClause)
  end upsertWithWhereSql

  /** Returns SQL for an UPSERT with DO NOTHING (insert only if no conflict).
    *
    * @param tableName
    *   Name of the table
    * @param insertColumns
    *   Columns for insertion
    * @param conflictColumns
    *   Columns that define the conflict
    * @return
    *   SQL statement for INSERT ... ON CONFLICT DO NOTHING
    */
  def upsertDoNothingSql(
      tableName: SqlText,
      insertColumns: SqlText,
      conflictColumns: Seq[SqlText],
  ): SqlText

end UpsertSupport

/** Trait for dialects that support JSON operations */
trait JsonSupport:
  self: Dialect =>

  /** Returns the SQL type for JSON columns */
  def jsonType: ColumnType

  /** Returns SQL for extracting a JSON field.
    *
    * @param column
    *   SQL reference to the JSON column
    * @param fieldPath
    *   Path to the field (e.g., "user.name")
    * @return
    *   SQL expression for field extraction
    */
  def jsonExtractSql(column: SqlText, fieldPath: String): SqlText

  /** Returns SQL for checking if a JSON column contains a value. PostgreSQL: `column @> '{"key": "value"}'` MySQL:
    * `JSON_CONTAINS(column, '{"key": "value"}')`
    *
    * @param column
    *   SQL reference to the JSON column
    * @param jsonValue
    *   JSON value to check for (as a string literal)
    * @return
    *   SQL expression for JSON containment
    */
  def jsonContainsSql(column: SqlText, jsonValue: JsonText): SqlText

  /** Returns SQL for checking if a JSON column has a key. PostgreSQL: `column ? 'key'` MySQL:
    * `JSON_CONTAINS_PATH(column, 'one', '$.key')`
    *
    * @param column
    *   SQL reference to the JSON column
    * @param key
    *   Key to check for
    * @return
    *   SQL expression for key existence check
    */
  def jsonHasKeySql(column: SqlText, key: String): SqlText

  /** Returns SQL for checking if a JSON column has any of the specified keys. PostgreSQL:
    * `column ?| array['key1', 'key2']` MySQL: `JSON_CONTAINS_PATH(column, 'one', '$.key1', '$.key2')`
    *
    * @param column
    *   SQL reference to the JSON column
    * @param keys
    *   Keys to check for (any match)
    * @return
    *   SQL expression for any key existence check
    */
  def jsonHasAnyKeySql(column: SqlText, keys: Seq[String]): SqlText

  /** Returns SQL for checking if a JSON column has all of the specified keys. PostgreSQL:
    * `column ?& array['key1', 'key2']` MySQL: `JSON_CONTAINS_PATH(column, 'all', '$.key1', '$.key2')`
    *
    * @param column
    *   SQL reference to the JSON column
    * @param keys
    *   Keys to check for (all must exist)
    * @return
    *   SQL expression for all keys existence check
    */
  def jsonHasAllKeysSql(column: SqlText, keys: Seq[String]): SqlText
end JsonSupport

/** Trait for dialects that support array operations */
trait ArraySupport:
  self: Dialect =>

  /** Returns the SQL type for array columns.
    *
    * @param elementType
    *   The type of array elements
    * @return
    *   SQL type for arrays
    */
  def arrayType(elementType: ColumnType): ColumnType

  /** Returns SQL for checking if an array contains a value.
    *
    * @param column
    *   SQL reference to the array column
    * @param value
    *   Value to check for
    * @return
    *   SQL expression for array containment
    */
  def arrayContainsSql(column: SqlText, value: SqlText): SqlText
end ArraySupport

/** Trait for dialects that support window functions */
trait WindowFunctionSupport:
  self: Dialect =>

  /** Returns SQL for ROW_NUMBER() window function.
    *
    * @param partitionBy
    *   Columns to partition by
    * @param orderBy
    *   Columns to order by
    * @return
    *   SQL expression for ROW_NUMBER()
    */
  def rowNumberSql(partitionBy: Seq[SqlText], orderBy: Seq[SqlText]): SqlText =
    val partitionClause = if partitionBy.nonEmpty then s" partition by ${partitionBy.mkString(", ")}" else ""
    val orderClause     = if orderBy.nonEmpty then s" order by ${orderBy.mkString(", ")}" else ""
    SqlText(s"row_number() over($partitionClause$orderClause)")
end WindowFunctionSupport

/** Trait for dialects that support common table expressions (CTEs) */
trait CommonTableExpressionSupport:
  self: Dialect =>

  /** Returns SQL for a WITH clause.
    *
    * @param cteName
    *   Name of the CTE
    * @param cteQuery
    *   Query for the CTE
    * @param recursive
    *   Whether the CTE is recursive
    * @return
    *   SQL fragment for WITH clause
    */
  def withClauseSql(cteName: SqlText, cteQuery: SqlText, recursive: Boolean = false): SqlText =
    val recursiveClause = if recursive then "recursive " else ""
    SqlText(s"with $recursiveClause$cteName as ($cteQuery)")
end CommonTableExpressionSupport

/** A dialect that can read its own catalogs through `SqlSession`.
  *
  * Spark does not implement this. `Schema.verify` on a dialect without it fails with `SaferisError.Unsupported`.
  */
trait SchemaIntrospectionSupport:
  self: Dialect =>

  import zio.*

  /** Introspect a table's schema from the database. */
  def introspectTable(tableName: TableName)(using
      Trace
  ): ZIO[SqlSession, SaferisError, Option[DatabaseTable]]
end SchemaIntrospectionSupport
