package saferis.postgres

import saferis.*

// Export PostgresDialect with its singleton type so all capability intersections are satisfied
given PostgresDialect.type = PostgresDialect

/** PostgreSQL dialect implementation providing PostgreSQL-specific type mappings and SQL generation */
object PostgresDialect
    extends Dialect
    with ReturningSupport
    with IndexIfNotExistsSupport
    with AdvancedAlterTableSupport
    with UpsertSupport
    with JsonSupport
    with ArraySupport
    with WindowFunctionSupport
    with CommonTableExpressionSupport:

  val name: String = "PostgreSQL"

  def columnType(tpe: PgType): String = tpe match
    case PgType.Bool        => "boolean"
    case PgType.Int2        => "smallint"
    case PgType.Int4        => "integer"
    case PgType.Int8        => "bigint"
    case PgType.Float4      => "real"
    case PgType.Float8      => "double precision"
    case PgType.Numeric     => "numeric"
    case PgType.VarChar     => s"varchar($DefaultVarcharLength)"
    case PgType.Text        => "text"
    case PgType.Bytea       => "bytea"
    case PgType.Date        => "date"
    case PgType.Time        => "time"
    case PgType.Timestamp   => "timestamp"
    case PgType.Timestamptz => "timestamptz"
    case PgType.Jsonb       => "jsonb"
    case PgType.Uuid        => "uuid"

  // === PostgreSQL-specific Auto-increment and Primary Key Support ===

  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): String =
    if isGenerated && isPrimaryKey && !hasCompoundKey then " generated always as identity primary key"
    else if isGenerated then " generated always as identity"
    else if isPrimaryKey && !hasCompoundKey then " primary key"
    else ""

  // === PostgreSQL-specific Query Features ===
  // PostgreSQL uses double quotes for identifier escaping
  override def identifierQuote: String = "\""

  // === UpsertSupport implementation ===
  def upsertSql(tableName: String, insertColumns: String, conflictColumns: Seq[String], updateColumns: String): String =
    s"insert into $tableName $insertColumns on conflict (${conflictColumns.mkString(", ")}) do update set $updateColumns"

  def upsertDoNothingSql(tableName: String, insertColumns: String, conflictColumns: Seq[String]): String =
    s"insert into $tableName $insertColumns on conflict (${conflictColumns.mkString(", ")}) do nothing"

  // === JsonSupport implementation ===
  def jsonType: String = "jsonb"

  def jsonExtractSql(columnName: String, fieldPath: String): String =
    val escaped = fieldPath.replace("'", "''")
    s"$columnName->>'$escaped'"

  def jsonContainsSql(columnName: String, jsonValue: String): String =
    val escaped = jsonValue.replace("'", "''")
    s"$columnName @> '$escaped'"

  // Function equivalents of ? / ?| / ?&. The operators stay out of generated SQL.
  def jsonHasKeySql(columnName: String, key: String): String =
    val escaped = key.replace("'", "''")
    s"jsonb_exists($columnName, '$escaped')"

  def jsonHasAnyKeySql(columnName: String, keys: Seq[String]): String =
    val keysArray = keys.map(k => s"'${k.replace("'", "''")}'").mkString(", ")
    s"jsonb_exists_any($columnName, array[$keysArray])"

  def jsonHasAllKeysSql(columnName: String, keys: Seq[String]): String =
    val keysArray = keys.map(k => s"'${k.replace("'", "''")}'").mkString(", ")
    s"jsonb_exists_all($columnName, array[$keysArray])"

  // === ArraySupport implementation ===
  def arrayType(elementType: String): String = s"$elementType[]"

  def arrayContainsSql(columnName: String, value: String): String =
    s"$value = ANY($columnName)"

end PostgresDialect
