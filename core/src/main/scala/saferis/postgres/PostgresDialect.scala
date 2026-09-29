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
      case SqlType.Bool            => "boolean"
      case SqlType.SmallInt        => "smallint"
      case SqlType.Integer         => "integer"
      case SqlType.BigInt          => "bigint"
      case SqlType.Real            => "real"
      case SqlType.DoublePrecision => "double precision"
      case SqlType.Numeric         => "numeric"
      case SqlType.VarChar         => s"varchar($DefaultVarcharLength)"
      case SqlType.Text            => "text"
      case SqlType.Binary          => "bytea"
      case SqlType.Date            => "date"
      case SqlType.Time            => "time"
      case SqlType.TimeTz          => "timetz"
      case SqlType.Timestamp       => "timestamp"
      case SqlType.TimestampTz     => "timestamptz"
      case SqlType.Json            => "jsonb"
      case SqlType.Uuid            => "uuid"
      case SqlType.Array(element)  => s"${columnType(element)}[]"
      case SqlType.Other(_)        => "text"

  def generatedKey(key: GeneratedKey): SqlText = SqlText:
    key match
      case GeneratedKey.IdentityPrimaryKey => " generated always as identity primary key"
      case GeneratedKey.Identity           => " generated always as identity"
      case GeneratedKey.PrimaryKey         => " primary key"
      case GeneratedKey.Plain              => ""

  // === PostgreSQL-specific Query Features ===
  // PostgreSQL uses double quotes for identifier escaping
  override def identifierQuote: String = "\""

  // === UpsertSupport implementation ===
  def upsertSql(
      tableName: TableName,
      insertColumns: SqlText,
      conflictColumns: Seq[ColumnName],
      updateColumns: SqlText,
  ): SqlText =
    val conflicts = conflictColumns.map(escapeIdentifier).mkString(", ")
    SqlText(
      s"insert into ${escapeIdentifier(tableName)} $insertColumns on conflict ($conflicts) do update set $updateColumns"
    )
  end upsertSql

  def upsertDoNothingSql(tableName: TableName, insertColumns: SqlText, conflictColumns: Seq[ColumnName]): SqlText =
    val conflicts = conflictColumns.map(escapeIdentifier).mkString(", ")
    SqlText(s"insert into ${escapeIdentifier(tableName)} $insertColumns on conflict ($conflicts) do nothing")

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
