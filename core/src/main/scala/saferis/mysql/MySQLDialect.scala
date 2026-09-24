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

  val name: String = "MySQL"

  def introspectTable(tableName: String)(using zio.Trace): zio.ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    MySQLCatalog.introspect(tableName)

  /** `decimal` alone is `decimal(10,0)` in MySQL and drops the fraction, so `Numeric` asks for the widest scale.
    * `datetime(6)` is a local timestamp and `timestamp(6)` an instant: MySQL converts only `timestamp` through the
    * session time zone, and the two spellings let a driver tell them apart when reading. `(6)` keeps microseconds.
    */
  def columnType(tpe: SqlType): String = tpe match
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

  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): String =
    if isGenerated && isPrimaryKey && !hasCompoundKey then " auto_increment primary key"
    else if isGenerated then " auto_increment"
    else if isPrimaryKey && !hasCompoundKey then " primary key"
    else ""

  // === MySQL-specific Index Creation ===
  // MySQL doesn't support IF NOT EXISTS for indexes in older versions
  // MySQL also doesn't support partial indexes (WHERE clause), so we ignore it
  override def createIndexSql(
      indexName: String,
      tableName: String,
      columnNames: Seq[String],
      ifNotExists: Boolean = true,
      where: Option[String] = None, // Ignored - MySQL doesn't support partial indexes
  ): String =
    s"create index ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})"

  override def createUniqueIndexSql(
      indexName: String,
      tableName: String,
      columnNames: Seq[String],
      ifNotExists: Boolean = true,
      where: Option[String] = None, // Ignored - MySQL doesn't support partial indexes
  ): String =
    s"create unique index ${escapeIdentifier(indexName)} on ${escapeIdentifier(tableName)} (${columnNames.map(escapeIdentifier).mkString(", ")})"

  // === MySQL-specific Query Features ===
  // MySQL uses backticks for identifier escaping
  override def identifierQuote: String = "`"

  // === MySQL-specific Table Operations ===
  override def truncateTableSql(tableName: String): String = s"truncate table ${escapeIdentifier(tableName)}"

  override def dropIndexSql(indexName: String, ifExists: Boolean = false): String =
    // MySQL uses different syntax for dropping indexes
    if ifExists then s"drop index if exists ${escapeIdentifier(indexName)}"
    else s"drop index ${escapeIdentifier(indexName)}"

  // === JsonSupport implementation ===
  def jsonType: String = "json"

  def jsonExtractSql(columnName: String, fieldPath: String): String =
    val escaped = fieldPath.replace("'", "''")
    s"JSON_EXTRACT($columnName, '$$.$escaped')"

  def jsonContainsSql(columnName: String, jsonValue: String): String =
    val escaped = jsonValue.replace("'", "''")
    s"JSON_CONTAINS($columnName, '$escaped')"

  def jsonHasKeySql(columnName: String, key: String): String =
    val escaped = key.replace("'", "''")
    s"JSON_CONTAINS_PATH($columnName, 'one', '$$.$escaped')"

  def jsonHasAnyKeySql(columnName: String, keys: Seq[String]): String =
    val paths = keys.map(k => s"'$$.${k.replace("'", "''")}'").mkString(", ")
    s"JSON_CONTAINS_PATH($columnName, 'one', $paths)"

  def jsonHasAllKeysSql(columnName: String, keys: Seq[String]): String =
    val paths = keys.map(k => s"'$$.${k.replace("'", "''")}'").mkString(", ")
    s"JSON_CONTAINS_PATH($columnName, 'all', $paths)"

end MySQLDialect
