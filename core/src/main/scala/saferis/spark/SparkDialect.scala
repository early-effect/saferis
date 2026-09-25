package saferis.spark

import saferis.*

given Dialect = SparkDialect

/** Spark/Databricks/Hive dialect implementation
  *
  * Key characteristics:
  *   - Uses backticks (`) for identifier quoting (table names, column names)
  *   - Uses single quotes (') for string literals (handled by default Encoder)
  *   - Limited support for constraints (no auto-increment, foreign keys)
  *   - Supports JSON, arrays, maps, and complex types
  *   - Uses Hive-compatible DDL syntax
  */
object SparkDialect
    extends Dialect
    with JsonSupport
    with ArraySupport
    with WindowFunctionSupport
    with CommonTableExpressionSupport:

  val name: DialectName = DialectName("Spark SQL")

  def columnType(tpe: SqlType): ColumnType = ColumnType:
    tpe match
      case SqlType.Bool        => "boolean"
      case SqlType.Int2        => "smallint"
      case SqlType.Int4        => "int"
      case SqlType.Int8        => "bigint"
      case SqlType.Float4      => "float"
      case SqlType.Float8      => "double"
      case SqlType.Numeric     => "decimal(10,0)"
      case SqlType.VarChar     => "string"
      case SqlType.Text        => "string"
      case SqlType.Bytea       => "binary"
      case SqlType.Date        => "date"
      case SqlType.Time        => "timestamp"
      case SqlType.Timestamp   => "timestamp"
      case SqlType.Timestamptz => "timestamp"
      case SqlType.Jsonb       => "string"
      case SqlType.Uuid        => "string"
      case SqlType.Array(_)    => "array<string>"
      case SqlType.Other(_)    => "string"

  // === Spark SQL Auto-increment and Primary Key Support ===
  // Spark SQL does not support auto-increment or primary key constraints in standard DDL
  // Some distributions (like Databricks) may support GENERATED ALWAYS AS IDENTITY
  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): SqlText = ???

  // === Spark SQL uses backticks for identifier escaping ===
  // This is critical: backticks are ONLY for identifiers (tables, columns, aliases)
  // String literals must use single quotes (handled by Encoder)
  override def identifierQuote: String = "`"

  // === Override DDL operations for Spark SQL syntax ===

  // Spark SQL doesn't support indexes at all
  override def createIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None,
  ): SqlText = throw new UnsupportedOperationException(
    "Spark SQL does not support CREATE INDEX. Consider using partitioning, bucketing, or Z-ordering instead."
  )

  override def createUniqueIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None,
  ): SqlText =
    throw new UnsupportedOperationException(
      "Spark SQL does not support CREATE UNIQUE INDEX. Uniqueness must be enforced at the application level."
    )

  override def dropIndexSql(indexName: IndexName, ifExists: Boolean = false): SqlText =
    throw new UnsupportedOperationException("Spark SQL does not support DROP INDEX")

  // === JsonSupport implementation ===
  // Spark SQL has JSON functions like get_json_object, from_json, to_json
  def jsonType: ColumnType = ColumnType("string") // JSON is stored as STRING type

  def jsonExtractSql(column: SqlText, fieldPath: String): SqlText =
    val escaped = fieldPath.replace("'", "''")
    SqlText(s"get_json_object($column, '$$.$escaped')")

  // Spark doesn't have a native @> operator, but we can use get_json_object to compare
  def jsonContainsSql(column: SqlText, jsonValue: JsonText): SqlText =
    throw new UnsupportedOperationException(
      "Spark SQL does not support JSON containment. Use get_json_object for field extraction instead."
    )

  // Check if a key exists by checking if get_json_object returns non-null
  def jsonHasKeySql(column: SqlText, key: String): SqlText =
    val escaped = key.replace("'", "''")
    SqlText(s"get_json_object($column, '$$.$escaped') is not null")

  // Check if any of the keys exist
  def jsonHasAnyKeySql(column: SqlText, keys: Seq[String]): SqlText =
    val checks = keys.map(k => s"get_json_object($column, '$$.${k.replace("'", "''")}') is not null")
    SqlText(checks.mkString("(", " or ", ")"))

  // Check if all keys exist
  def jsonHasAllKeysSql(column: SqlText, keys: Seq[String]): SqlText =
    val checks = keys.map(k => s"get_json_object($column, '$$.${k.replace("'", "''")}') is not null")
    SqlText(checks.mkString("(", " and ", ")"))

  // === ArraySupport implementation ===
  def arrayType(elementType: ColumnType): ColumnType = ColumnType(s"array<$elementType>")

  def arrayContainsSql(column: SqlText, value: SqlText): SqlText =
    SqlText(s"array_contains($column, $value)")

end SparkDialect
