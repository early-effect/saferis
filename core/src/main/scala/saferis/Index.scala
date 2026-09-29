package saferis

/** Represents a complete index specification ready for DDL generation.
  *
  * @tparam A
  *   The table type this index belongs to
  * @param columns
  *   Column field names for the index (will be converted to labels at SQL generation time)
  * @param name
  *   Optional custom index name (auto-generated if None)
  * @param unique
  *   Whether this is a unique index
  * @param where
  *   Optional WHERE clause for partial indexes (pre-rendered SQL)
  */
final case class IndexSpec[A](
    columns: Seq[FieldName],
    name: Option[IndexName] = None,
    unique: Boolean = false,
    where: Option[SqlText] = None,
):
  /** Generate CREATE INDEX SQL for this spec
    *
    * @param tableName
    *   The table name
    * @param fieldToLabel
    *   Function to convert field names to column labels (respects @label annotations)
    */
  def toCreateSql(tableName: TableName, fieldToLabel: String => ColumnName)(using dialect: Dialect): SqlText =
    val columnLabels = columns.map(fieldToLabel)
    val indexName    = name.getOrElse(IndexName.default(tableName, columnLabels))
    if unique then dialect.createUniqueIndexSql(indexName, tableName, columnLabels, false, where)
    else dialect.createIndexSql(indexName, tableName, columnLabels, false, where)
end IndexSpec
