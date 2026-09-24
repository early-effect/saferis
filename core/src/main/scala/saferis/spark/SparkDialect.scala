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

  val name: String = "Spark SQL"

  def columnType(tpe: SqlType): String = tpe match
    case SqlType.Bool            => "boolean"
    case SqlType.SmallInt        => "smallint"
    case SqlType.Integer         => "int"
    case SqlType.BigInt          => "bigint"
    case SqlType.Real            => "float"
    case SqlType.DoublePrecision => "double"
    case SqlType.Numeric         => "decimal(10,0)"
    case SqlType.VarChar         => "string"
    case SqlType.Text            => "string"
    case SqlType.Binary          => "binary"
    case SqlType.Date            => "date"
    case SqlType.Time            => "timestamp"
    case SqlType.Timestamp       => "timestamp"
    case SqlType.TimestampTz     => "timestamp"
    case SqlType.Json            => "string"
    case SqlType.Uuid            => "string"

  // === Spark SQL Auto-increment and Primary Key Support ===
  // Spark SQL does not support auto-increment or primary key constraints in standard DDL
  // Some distributions (like Databricks) may support GENERATED ALWAYS AS IDENTITY
  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): String = ???

  // === Spark SQL uses backticks for identifier escaping ===
  // This is critical: backticks are ONLY for identifiers (tables, columns, aliases)
  // String literals must use single quotes (handled by Encoder)
  override def identifierQuote: String = "`"

  // === Override DDL operations for Spark SQL syntax ===

  override def createTableClause(ifNotExists: Boolean): String =
    if ifNotExists then "create table if not exists"
    else "create table"

  override def dropTableSql(tableName: String, ifExists: Boolean): String =
    if ifExists then s"drop table if exists $tableName"
    else s"drop table $tableName"

  override def truncateTableSql(tableName: String): String =
    s"truncate table $tableName"

  // Spark SQL doesn't support indexes at all
  override def createIndexSql(
      indexName: String,
      tableName: String,
      columnNames: Seq[String],
      ifNotExists: Boolean = true,
      where: Option[String] = None,
  ): String = throw new UnsupportedOperationException(
    "Spark SQL does not support CREATE INDEX. Consider using partitioning, bucketing, or Z-ordering instead."
  )

  override def createUniqueIndexSql(
      indexName: String,
      tableName: String,
      columnNames: Seq[String],
      ifNotExists: Boolean = true,
      where: Option[String] = None,
  ): String =
    throw new UnsupportedOperationException(
      "Spark SQL does not support CREATE UNIQUE INDEX. Uniqueness must be enforced at the application level."
    )

  override def dropIndexSql(indexName: String, ifExists: Boolean = false): String =
    throw new UnsupportedOperationException("Spark SQL does not support DROP INDEX")

  // === JsonSupport implementation ===
  // Spark SQL has JSON functions like get_json_object, from_json, to_json
  def jsonType: String = "string" // JSON is stored as STRING type

  def jsonExtractSql(columnName: String, fieldPath: String): String =
    val escaped = fieldPath.replace("'", "''")
    s"get_json_object($columnName, '$$.$escaped')"

  // Spark doesn't have a native @> operator, but we can use get_json_object to compare
  def jsonContainsSql(columnName: String, jsonValue: String): String =
    throw new UnsupportedOperationException(
      "Spark SQL does not support JSON containment. Use get_json_object for field extraction instead."
    )

  // Check if a key exists by checking if get_json_object returns non-null
  def jsonHasKeySql(columnName: String, key: String): String =
    val escaped = key.replace("'", "''")
    s"get_json_object($columnName, '$$.$escaped') is not null"

  // Check if any of the keys exist
  def jsonHasAnyKeySql(columnName: String, keys: Seq[String]): String =
    val checks = keys.map(k => s"get_json_object($columnName, '$$.${k.replace("'", "''")}') is not null")
    checks.mkString("(", " or ", ")")

  // Check if all keys exist
  def jsonHasAllKeysSql(columnName: String, keys: Seq[String]): String =
    val checks = keys.map(k => s"get_json_object($columnName, '$$.${k.replace("'", "''")}') is not null")
    checks.mkString("(", " and ", ")")

  // === ArraySupport implementation ===
  def arrayType(elementType: String): String = s"array<$elementType>"

  def arrayContainsSql(columnName: String, value: String): String =
    s"array_contains($columnName, $value)"

end SparkDialect
