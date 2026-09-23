package saferis.tests

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.postgres.given
import saferis.sqlite.SQLiteDialect
import zio.test.*

object DialectSpecs extends ZIOSpecDefault:

  val spec = suite("Dialect Support")(
    test("PostgreSQL dialect provides correct auto-increment clause") {
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "PostgreSQL") &&
      assertTrue(dialect.autoIncrementClause(true, true, false) == " generated always as identity primary key") &&
      assertTrue(dialect.autoIncrementClause(true, true, true) == " generated always as identity") &&
      assertTrue(dialect.autoIncrementClause(false, true, false) == " primary key") &&
      assertTrue(dialect.autoIncrementClause(false, false, false) == "")
    },
    test("MySQL dialect provides correct auto-increment clause") {
      assertTrue(MySQLDialect.name == "MySQL") &&
      assertTrue(MySQLDialect.autoIncrementClause(true, true, false) == " auto_increment primary key") &&
      assertTrue(MySQLDialect.autoIncrementClause(true, true, true) == " auto_increment") &&
      assertTrue(MySQLDialect.autoIncrementClause(false, true, false) == " primary key") &&
      assertTrue(MySQLDialect.autoIncrementClause(false, false, false) == "")
    },
    test("SQLite dialect provides correct auto-increment clause") {
      assertTrue(SQLiteDialect.name == "SQLite") &&
      assertTrue(SQLiteDialect.autoIncrementClause(true, true, false) == " primary key autoincrement") &&
      assertTrue(SQLiteDialect.autoIncrementClause(false, true, false) == " primary key") &&
      assertTrue(SQLiteDialect.autoIncrementClause(true, false, false) == " autoincrement") &&
      assertTrue(SQLiteDialect.autoIncrementClause(false, false, false) == "")
    },
    test("Dialects have different column type mappings") {
      val pgDialect = summon[Dialect]
      assertTrue(pgDialect.columnType(PgType.VarChar) == "varchar(255)") &&
      assertTrue(MySQLDialect.columnType(PgType.VarChar) == "varchar(255)") &&
      assertTrue(SQLiteDialect.columnType(PgType.VarChar) == "text") &&
      assertTrue(pgDialect.columnType(PgType.Int4) == "integer") &&
      assertTrue(MySQLDialect.columnType(PgType.Int4) == "int") &&
      assertTrue(SQLiteDialect.columnType(PgType.Int4) == "integer")
    },
    test("Dialects have different identifier quoting") {
      val pgDialect = summon[Dialect]
      assertTrue(pgDialect.identifierQuote == "\"") &&
      assertTrue(MySQLDialect.identifierQuote == "`") &&
      assertTrue(SQLiteDialect.identifierQuote == "\"")
    },
    test("Dialects generate correct index SQL with escaped identifiers") {
      val pgDialect = summon[Dialect]
      val indexSql  = pgDialect.createIndexSql("idx_test", "test_table", Seq("name"), true)
      assertTrue(indexSql == "create index if not exists \"idx_test\" on \"test_table\" (\"name\")")

      val mysqlIndexSql = MySQLDialect.createIndexSql("idx_test", "test_table", Seq("name"), true)
      assertTrue(mysqlIndexSql == "create index `idx_test` on `test_table` (`name`)")

      val sqliteIndexSql = SQLiteDialect.createIndexSql("idx_test", "test_table", Seq("name"), true)
      assertTrue(sqliteIndexSql == "create index if not exists \"idx_test\" on \"test_table\" (\"name\")")
    },
    test("PostgreSQL escapeIdentifier handles SQL injection attempts") {
      val pgDialect = summon[Dialect]
      // Test normal identifier
      assertTrue(pgDialect.escapeIdentifier("my_column") == "\"my_column\"") &&
      // Test identifier with embedded quotes - should double the quotes
      assertTrue(pgDialect.escapeIdentifier("my\"column") == "\"my\"\"column\"") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(pgDialect.escapeIdentifier("\"; DROP TABLE users--") == "\"\"\"; DROP TABLE users--\"") &&
      // Test identifier with multiple quotes
      assertTrue(pgDialect.escapeIdentifier("a\"b\"c") == "\"a\"\"b\"\"c\"") &&
      // Test empty identifier
      assertTrue(pgDialect.escapeIdentifier("") == "\"\"") &&
      // Test identifier with spaces
      assertTrue(pgDialect.escapeIdentifier("my column") == "\"my column\"")
    },
    test("MySQL escapeIdentifier handles SQL injection attempts") {
      // Test normal identifier
      assertTrue(MySQLDialect.escapeIdentifier("my_column") == "`my_column`") &&
      // Test identifier with embedded backticks - should double the backticks
      assertTrue(MySQLDialect.escapeIdentifier("my`column") == "`my``column`") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(MySQLDialect.escapeIdentifier("`; DROP TABLE users--") == "```; DROP TABLE users--`") &&
      // Test identifier with multiple backticks
      assertTrue(MySQLDialect.escapeIdentifier("a`b`c") == "`a``b``c`") &&
      // Test empty identifier
      assertTrue(MySQLDialect.escapeIdentifier("") == "``") &&
      // Test identifier with spaces
      assertTrue(MySQLDialect.escapeIdentifier("my column") == "`my column`")
    },
    test("SQLite escapeIdentifier handles SQL injection attempts") {
      // Test normal identifier
      assertTrue(SQLiteDialect.escapeIdentifier("my_column") == "\"my_column\"") &&
      // Test identifier with embedded quotes - should double the quotes
      assertTrue(SQLiteDialect.escapeIdentifier("my\"column") == "\"my\"\"column\"") &&
      // Test SQL injection attempt with DROP TABLE
      assertTrue(SQLiteDialect.escapeIdentifier("\"; DROP TABLE users--") == "\"\"\"; DROP TABLE users--\"") &&
      // Test identifier with multiple quotes
      assertTrue(SQLiteDialect.escapeIdentifier("a\"b\"c") == "\"a\"\"b\"\"c\"") &&
      // Test empty identifier
      assertTrue(SQLiteDialect.escapeIdentifier("") == "\"\"") &&
      // Test identifier with spaces
      assertTrue(SQLiteDialect.escapeIdentifier("my column") == "\"my column\"")
    },
  )
end DialectSpecs
