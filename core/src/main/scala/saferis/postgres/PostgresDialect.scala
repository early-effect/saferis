package saferis.postgres

import saferis.*
import zio.Trace
import zio.ZIO

// Export PostgresDialect with its singleton type so all capability intersections are satisfied
given PostgresDialect.type = PostgresDialect

/** PostgreSQL dialect implementation providing PostgreSQL-specific type mappings, SQL generation, and catalog reads. */
object PostgresDialect
    extends Dialect
    with ReturningSupport
    with IndexIfNotExistsSupport
    with AdvancedAlterTableSupport
    with UpsertSupport
    with JsonSupport
    with ArraySupport
    with WindowFunctionSupport
    with CommonTableExpressionSupport
    with SchemaIntrospectionSupport:

  val name: DialectName = DialectName("PostgreSQL")

  def introspectTable(tableName: TableName)(using Trace): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    PostgresCatalog.introspect(tableName)

  def columnType(tpe: SqlType): ColumnType = ColumnType:
    tpe match
      case SqlType.Bool           => "boolean"
      case SqlType.Int2           => "smallint"
      case SqlType.Int4           => "integer"
      case SqlType.Int8           => "bigint"
      case SqlType.Float4         => "real"
      case SqlType.Float8         => "double precision"
      case SqlType.Numeric        => "numeric"
      case SqlType.VarChar        => s"varchar($DefaultVarcharLength)"
      case SqlType.Text           => "text"
      case SqlType.Bytea          => "bytea"
      case SqlType.Date           => "date"
      case SqlType.Time           => "time"
      case SqlType.Timestamp      => "timestamp"
      case SqlType.Timestamptz    => "timestamptz"
      case SqlType.Jsonb          => "jsonb"
      case SqlType.Uuid           => "uuid"
      case SqlType.Array(element) => s"${columnType(element)}[]"
      case SqlType.Other(_)       => "text"

  // === PostgreSQL-specific Auto-increment and Primary Key Support ===

  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): SqlText = SqlText:
    if isGenerated && isPrimaryKey && !hasCompoundKey then " generated always as identity primary key"
    else if isGenerated then " generated always as identity"
    else if isPrimaryKey && !hasCompoundKey then " primary key"
    else ""

  // === PostgreSQL-specific Query Features ===
  // PostgreSQL uses double quotes for identifier escaping
  override def identifierQuote: String = "\""

  // === UpsertSupport implementation ===
  def upsertSql(
      tableName: SqlText,
      insertColumns: SqlText,
      conflictColumns: Seq[SqlText],
      updateColumns: SqlText,
  ): SqlText =
    SqlText(
      s"insert into $tableName $insertColumns on conflict (${conflictColumns.mkString(", ")}) do update set $updateColumns"
    )

  def upsertDoNothingSql(tableName: SqlText, insertColumns: SqlText, conflictColumns: Seq[SqlText]): SqlText =
    SqlText(s"insert into $tableName $insertColumns on conflict (${conflictColumns.mkString(", ")}) do nothing")

  // === JsonSupport implementation ===
  def jsonType: ColumnType = ColumnType("jsonb")

  def jsonExtractSql(column: SqlText, fieldPath: String): SqlText =
    val escaped = fieldPath.replace("'", "''")
    SqlText(s"$column->>'$escaped'")

  def jsonContainsSql(column: SqlText, jsonValue: JsonText): SqlText =
    val escaped = jsonValue.replace("'", "''")
    SqlText(s"$column @> '$escaped'")

  // Function equivalents of ? / ?| / ?&. The operators stay out of generated SQL.
  def jsonHasKeySql(column: SqlText, key: String): SqlText =
    val escaped = key.replace("'", "''")
    SqlText(s"jsonb_exists($column, '$escaped')")

  def jsonHasAnyKeySql(column: SqlText, keys: Seq[String]): SqlText =
    val keysArray = keys.map(k => s"'${k.replace("'", "''")}'").mkString(", ")
    SqlText(s"jsonb_exists_any($column, array[$keysArray])")

  def jsonHasAllKeysSql(column: SqlText, keys: Seq[String]): SqlText =
    val keysArray = keys.map(k => s"'${k.replace("'", "''")}'").mkString(", ")
    SqlText(s"jsonb_exists_all($column, array[$keysArray])")

  // === ArraySupport implementation ===
  def arrayType(elementType: ColumnType): ColumnType = ColumnType(s"$elementType[]")

  def arrayContainsSql(column: SqlText, value: SqlText): SqlText =
    SqlText(s"$value = ANY($column)")

end PostgresDialect
