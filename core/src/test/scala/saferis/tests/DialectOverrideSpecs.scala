package saferis.tests

import saferis.*
import zio.test.*

object DialectOverrideSpecs extends ZIOSpecDefault:

  val spec = suite("Dialect Override Support")(
    test("Default PostgreSQL dialect is used without explicit import") {
      // No explicit dialect import - should use default PostgreSQL
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "PostgreSQL")
    },
    test("Can override default dialect with explicit MySQL import") {
      // Explicit import should override the default
      import saferis.mysql.given
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "MySQL")
    },
    test("Can use custom codec with MySQL dialect") {
      // Override with MySQL dialect
      import saferis.mysql.given

      // Define a custom type and encoder for testing
      final case class CustomId(value: String)

      given customIdEncoder: Encoder[CustomId] with
        def sqlType: SqlType                                        = SqlType.VarChar
        def encode(id: CustomId): SqlValue                          = SqlValue.VarChar(id.value)
        override def columnType(using dialect: Dialect): ColumnType = ColumnType("varchar(50)")

      // The MySQL dialect should be in scope
      val dialect     = summon[Dialect]
      val dialectName = dialect.name

      // The custom encoder should work with the MySQL dialect
      val encoder = summon[Encoder[CustomId]]
      val colType = encoder.columnType

      // Test that we can create a literal
      val testId  = CustomId("test-123")
      val literal = encoder.literal(testId)

      assertTrue(dialectName == "MySQL") &&
      assertTrue(colType == "varchar(50)") &&
      assertTrue(literal == "'test-123'")
    },
    test("Switching dialects changes column types for standard types") {
      // Test with default PostgreSQL
      val pgDialect = summon[Dialect]
      val pgIntType = pgDialect.columnType(SqlType.Int4)

      // Test with MySQL in a nested scope
      val mysqlIntType =
        import saferis.mysql.given
        val dialect = summon[Dialect]
        dialect.columnType(SqlType.Int4)

      assertTrue(pgIntType == "integer") &&
      assertTrue(mysqlIntType == "int")
    },
  )
end DialectOverrideSpecs
