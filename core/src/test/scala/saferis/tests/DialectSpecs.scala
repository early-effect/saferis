package saferis.tests

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.postgres.given
import saferis.sqlite.SQLiteDialect
import zio.test.*

object DialectSpecs extends ZIOSpecDefault:

  val spec = suite("Dialect Support")(
    test("PostgreSQL spells each GeneratedKey") {
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "PostgreSQL") &&
      assertTrue(
        dialect.generatedKey(GeneratedKey.IdentityPrimaryKey) == " generated always as identity primary key"
      ) &&
      assertTrue(dialect.generatedKey(GeneratedKey.Identity) == " generated always as identity") &&
      assertTrue(dialect.generatedKey(GeneratedKey.PrimaryKey) == " primary key") &&
      assertTrue(dialect.generatedKey(GeneratedKey.Plain) == "")
    },
    test("MySQL spells each GeneratedKey") {
      assertTrue(MySQLDialect.name == "MySQL") &&
      assertTrue(MySQLDialect.generatedKey(GeneratedKey.IdentityPrimaryKey) == " auto_increment primary key") &&
      assertTrue(MySQLDialect.generatedKey(GeneratedKey.Identity) == " auto_increment") &&
      assertTrue(MySQLDialect.generatedKey(GeneratedKey.PrimaryKey) == " primary key") &&
      assertTrue(MySQLDialect.generatedKey(GeneratedKey.Plain) == "")
    },
    test("SQLite spells each GeneratedKey, and AUTOINCREMENT is the identity primary key") {
      assertTrue(SQLiteDialect.name == "SQLite") &&
      assertTrue(SQLiteDialect.generatedKey(GeneratedKey.IdentityPrimaryKey) == " primary key autoincrement") &&
      assertTrue(SQLiteDialect.generatedKey(GeneratedKey.Identity) == " autoincrement") &&
      assertTrue(SQLiteDialect.generatedKey(GeneratedKey.PrimaryKey) == " primary key") &&
      assertTrue(SQLiteDialect.generatedKey(GeneratedKey.Plain) == "")
    },
    test("Dialects have different column type mappings") {
      val pgDialect = summon[Dialect]
      assertTrue(pgDialect.columnType(SqlType.VarChar) == "varchar(255)") &&
      assertTrue(MySQLDialect.columnType(SqlType.VarChar) == "varchar(255)") &&
      assertTrue(SQLiteDialect.columnType(SqlType.VarChar) == "varchar(255)") &&
      assertTrue(pgDialect.columnType(SqlType.Integer) == "integer") &&
      assertTrue(MySQLDialect.columnType(SqlType.Integer) == "int") &&
      assertTrue(SQLiteDialect.columnType(SqlType.Integer) == "integer")
    },
    test("Dialects have different identifier quoting") {
      val pgDialect = summon[Dialect]
      assertTrue(pgDialect.identifierQuote == "\"") &&
      assertTrue(MySQLDialect.identifierQuote == "`") &&
      assertTrue(SQLiteDialect.identifierQuote == "\"")
    },
    test("Dialects generate correct index SQL with escaped identifiers") {
      val pgDialect = summon[Dialect]
      val indexSql  =
        pgDialect.createIndexSql(IndexName("idx_test"), TableName("test_table"), Seq(ColumnName("name")), true)
      assertTrue(indexSql == "create index if not exists \"idx_test\" on \"test_table\" (\"name\")")

      val mysqlIndexSql =
        MySQLDialect.createIndexSql(IndexName("idx_test"), TableName("test_table"), Seq(ColumnName("name")), true)
      assertTrue(mysqlIndexSql == "create index `idx_test` on `test_table` (`name`)")

      val sqliteIndexSql =
        SQLiteDialect.createIndexSql(IndexName("idx_test"), TableName("test_table"), Seq(ColumnName("name")), true)
      assertTrue(sqliteIndexSql == "create index if not exists \"idx_test\" on \"test_table\" (\"name\")")
    },
    test("PostgreSQL escapeIdentifier handles SQL injection attempts") {
      val pgDialect = summon[Dialect]
      // Test normal identifier
      assertTrue(pgDialect.escapeIdentifier(ColumnName("my_column")) == "\"my_column\"") &&
      // Test identifier with embedded quotes - should double the quotes
      assertTrue(pgDialect.escapeIdentifier(ColumnName("my\"column")) == "\"my\"\"column\"") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(pgDialect.escapeIdentifier(ColumnName("\"; DROP TABLE users--")) == "\"\"\"; DROP TABLE users--\"") &&
      // Test identifier with multiple quotes
      assertTrue(pgDialect.escapeIdentifier(ColumnName("a\"b\"c")) == "\"a\"\"b\"\"c\"") &&
      // Test empty identifier
      assertTrue(pgDialect.escapeIdentifier(ColumnName("")) == "\"\"") &&
      // Test identifier with spaces
      assertTrue(pgDialect.escapeIdentifier(ColumnName("my column")) == "\"my column\"")
    },
    test("MySQL escapeIdentifier handles SQL injection attempts") {
      // Test normal identifier
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("my_column")) == "`my_column`") &&
      // Test identifier with embedded backticks - should double the backticks
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("my`column")) == "`my``column`") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("`; DROP TABLE users--")) == "```; DROP TABLE users--`") &&
      // Test identifier with multiple backticks
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("a`b`c")) == "`a``b``c`") &&
      // Test empty identifier
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("")) == "``") &&
      // Test identifier with spaces
      assertTrue(MySQLDialect.escapeIdentifier(ColumnName("my column")) == "`my column`")
    },
    test("SQLite escapeIdentifier handles SQL injection attempts") {
      // Test normal identifier
      assertTrue(SQLiteDialect.escapeIdentifier(ColumnName("my_column")) == "\"my_column\"") &&
      // Test identifier with embedded quotes - should double the quotes
      assertTrue(SQLiteDialect.escapeIdentifier(ColumnName("my\"column")) == "\"my\"\"column\"") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(
        SQLiteDialect.escapeIdentifier(ColumnName("\"; DROP TABLE users--")) == "\"\"\"; DROP TABLE users--\""
      ) &&
      // Test identifier with multiple quotes
      assertTrue(SQLiteDialect.escapeIdentifier(ColumnName("a\"b\"c")) == "\"a\"\"b\"\"c\"") &&
      // Test empty identifier
      assertTrue(SQLiteDialect.escapeIdentifier(ColumnName("")) == "\"\"") &&
      // Test identifier with spaces
      assertTrue(SQLiteDialect.escapeIdentifier(ColumnName("my column")) == "\"my column\"")
    },
  )
end DialectSpecs
