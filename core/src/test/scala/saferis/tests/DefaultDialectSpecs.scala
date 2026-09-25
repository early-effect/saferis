package saferis.tests

import saferis.*
import zio.test.*

object DefaultDialectSpecs extends ZIOSpecDefault:

  val spec = suite("Default Dialect Support")(
    test("PostgreSQL dialect is available by default without explicit import") {
      // This should work without importing saferis.postgres.{given}
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "PostgreSQL")
    },
    test("Default dialect provides PostgreSQL-specific features") {
      val dialect = summon[Dialect]
      assertTrue(dialect.autoIncrementClause(true, true, false) == " generated always as identity primary key") &&
      assertTrue(dialect.identifierQuote == "\"") &&
      assertTrue(dialect.columnType(SqlType.VarChar) == "varchar(255)")
    },
    test("Can create tables with just import saferis.*") {
      // This tests that all the necessary implicits are available
      @tableName("test_default_dialect")
      final case class TestTable(@key id: Int, name: String) derives Table

      val table = Table[TestTable]
      assertTrue(toSql(table) == "test_default_dialect")
    },
    test("Can generate compound key index SQL with default dialect") {
      @tableName("test_create_sql")
      final case class TestTable(@key id: Int, @key tenantId: Int, name: String) derives Table

      import saferis.ddl.*
      val sql = createIndexesSql[TestTable]()

      // Should contain PostgreSQL-specific compound key index syntax
      assertTrue(sql.contains("create index if not exists")) &&
      assertTrue(sql.contains("compound_key")) &&
      assertTrue(sql.contains("test_create_sql"))
    },
  )
end DefaultDialectSpecs
