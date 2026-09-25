package saferis.mysql

import saferis.*

given Dialect = MySQLDialect

/** MySQL dialect implementation providing MySQL-specific type mappings and SQL generation.
  *
  * This demonstrates how different databases have different syntax and features:
  *   - MySQL uses AUTO_INCREMENT instead of GENERATED ALWAYS AS IDENTITY
  *   - MySQL doesn't support IF NOT EXISTS for indexes until version 5.7
  *   - MySQL uses backticks for identifier escaping
  *   - MySQL has different type names for some SQL types
  */
object MySQLDialect
    extends Dialect
    with JsonSupport
    with WindowFunctionSupport
    with CommonTableExpressionSupport
    with SchemaIntrospectionSupport:

  val name: DialectName = DialectName("MySQL")

  def introspectTable(tableName: TableName)(using zio.Trace): zio.ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    MySQLCatalog.introspect(tableName)

  /** `decimal` alone is `decimal(10,0)` in MySQL and drops the fraction, so `Numeric` asks for the widest scale.
    * `datetime(6)` is a local timestamp and `timestamp(6)` an instant: MySQL converts only `timestamp` through the
    * session time zone, and the two spellings let a driver tell them apart when reading. `(6)` keeps microseconds.
    */
  def columnType(tpe: SqlType): ColumnType = ColumnType:
    tpe match
      case SqlType.Bool        => "boolean"
      case SqlType.Int2        => "smallint"
      case SqlType.Int4        => "int"
      case SqlType.Int8        => "bigint"
      case SqlType.Float4      => "float"
      case SqlType.Float8      => "double"
      case SqlType.Numeric     => "decimal(65, 30)"
      case SqlType.VarChar     => s"varchar($DefaultVarcharLength)"
      case SqlType.Text        => "longtext"
      case SqlType.Bytea       => "blob"
      case SqlType.Date        => "date"
      case SqlType.Time        => "time(6)"
      case SqlType.Timestamp   => "datetime(6)"
      case SqlType.Timestamptz => "timestamp(6)"
      case SqlType.Jsonb       => "json"
      case SqlType.Uuid        => "char(36)"
      case SqlType.Array(_)    => "json"
      case SqlType.Other(_)    => "text"

  // === MySQL-specific Auto-increment and Primary Key Support ===

  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): SqlText = SqlText:
    if isGenerated && isPrimaryKey && !hasCompoundKey then " auto_increment primary key"
    else if isGenerated then " auto_increment"
    else if isPrimaryKey && !hasCompoundKey then " primary key"
    else ""

  // === MySQL-specific Index Creation ===
  // MySQL doesn't support IF NOT EXISTS for indexes in older versions
  // MySQL also doesn't support partial indexes (WHERE clause), so we ignore it
  override def createIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None, // Ignored - MySQL doesn't support partial indexes
  ): SqlText = SqlText:
    s"create index ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})"

  override def createUniqueIndexSql(
      indexName: IndexName,
      tableName: TableName,
      columnNames: Seq[ColumnName],
      ifNotExists: Boolean = true,
      where: Option[SqlText] = None, // Ignored - MySQL doesn't support partial indexes
  ): SqlText = SqlText:
    s"create unique index ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})"

  // === MySQL-specific Query Features ===
  // MySQL uses backticks for identifier escaping
  override def identifierQuote: String = "`"

  // === JsonSupport implementation ===
  def jsonType: ColumnType = ColumnType("json")

  def jsonExtractSql(column: SqlText, fieldPath: String): SqlText =
    val escaped = fieldPath.replace("'", "''")
    SqlText(s"JSON_EXTRACT($column, '$$.$escaped')")

  def jsonContainsSql(column: SqlText, jsonValue: JsonText): SqlText =
    val escaped = jsonValue.replace("'", "''")
    SqlText(s"JSON_CONTAINS($column, '$escaped')")

  def jsonHasKeySql(column: SqlText, key: String): SqlText =
    val escaped = key.replace("'", "''")
    SqlText(s"JSON_CONTAINS_PATH($column, 'one', '$$.$escaped')")

  def jsonHasAnyKeySql(column: SqlText, keys: Seq[String]): SqlText =
    val paths = keys.map(k => s"'$$.${k.replace("'", "''")}'").mkString(", ")
    SqlText(s"JSON_CONTAINS_PATH($column, 'one', $paths)")

  def jsonHasAllKeysSql(column: SqlText, keys: Seq[String]): SqlText =
    val paths = keys.map(k => s"'$$.${k.replace("'", "''")}'").mkString(", ")
    SqlText(s"JSON_CONTAINS_PATH($column, 'all', $paths)")

end MySQLDialect
