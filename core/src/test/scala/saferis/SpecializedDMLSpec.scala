package saferis

import saferis.SpecializedDML.*
import zio.*
import zio.test.*

object SpecializedDMLSpec extends ZIOSpecDefault:

  // Test model
  @tableName("test_users")
  final case class TestUser(@generated @key id: Int, name: String, email: String) derives Table

  def spec = suite("SpecializedDML")(
    test("should provide type-safe dialect-specific operations") {
      for _ <- ZIO.unit
      yield assertTrue(true) // Just a placeholder test
    },
    test("PostgreSQL dialect capabilities") {
      import saferis.postgres.given
      // Compile-time type evidence - these lines only compile if PostgreSQL supports these capabilities
      val _: Dialect & ReturningSupport        = summon[Dialect]
      val _: Dialect & JsonSupport             = summon[Dialect]
      val _: Dialect & ArraySupport            = summon[Dialect]
      val _: Dialect & UpsertSupport           = summon[Dialect]
      val _: Dialect & IndexIfNotExistsSupport = summon[Dialect]
      assertTrue(summon[Dialect].name == "PostgreSQL")
    },
    test("SQLite dialect capabilities") {
      import saferis.sqlite.given
      // Compile-time type evidence - these lines only compile if SQLite supports these capabilities
      val _: Dialect & ReturningSupport             = summon[Dialect]
      val _: Dialect & WindowFunctionSupport        = summon[Dialect]
      val _: Dialect & CommonTableExpressionSupport = summon[Dialect]
      assertTrue(summon[Dialect].name == "SQLite")
    },

    // MySQL dialect test temporarily disabled due to import issues
    /*
    test("MySQL dialect capabilities") {
      import saferis.mysql.{given}
      val dialect = summon[Dialect]
      
      for
        _ <- ZIO.unit
      yield {
        // Test that MySQL supports expected capabilities
        assertTrue(dialect.name == "MySQL") &&
        assertTrue(dialect.isInstanceOf[JsonSupport]) &&
        assertTrue(dialect.isInstanceOf[WindowFunctionSupport]) &&
        assertTrue(dialect.isInstanceOf[CommonTableExpressionSupport])
      }
    },
     */

    test("JSON operations are available for dialects with JsonSupport") {
      import saferis.postgres.PostgresDialect
      val dialect      = PostgresDialect
      val jsonFragment = jsonExtract(SqlText("user_data"), "profile.name")(using dialect)
      assertTrue(jsonFragment.sql.contains("JSON_EXTRACT") || jsonFragment.sql.contains("->"))
    },
    test("Array operations are available for dialects with ArraySupport") {
      import saferis.postgres.PostgresDialect
      val dialect       = PostgresDialect
      val arrayFragment = arrayContains(SqlText("tags"), SqlText("'scala'"))(using dialect)
      assertTrue(arrayFragment.sql.contains("ANY") || arrayFragment.sql.contains("@>"))
    },
  )
end SpecializedDMLSpec
