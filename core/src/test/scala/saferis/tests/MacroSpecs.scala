package saferis.tests

import saferis.*
import zio.test.*

object MacroSpecs extends ZIOSpecDefault:

  @tableName("test_users")
  final case class TestUser(@key id: Int, name: String, age: Int) derives Table

  // Table with a field named "column" to test naming collision avoidance
  @tableName("with_column_field")
  final case class WithColumnField(@key id: Int, column: String, data: Int) derives Table

  // Table with fields that could collide with Instance methods
  @tableName("reserved_names")
  final case class ReservedNames(
      @key id: Int,
      sql: String,       // Collides with Placeholder.sql
      withAlias: String, // Collides with Instance.withAlias
      deAliased: String, // Collides with Instance.deAliased
  ) derives Table

  val spec = suite("MacroSpecs")(
    suiteAll("Macro extractColumn"):
      test("extractColumn returns typed Column[String] for string field"):
        val instance                = Table[TestUser]
        val nameCol: Column[String] = instance.column(_.name)
        assertTrue(
          nameCol.label == "name",
          nameCol.name == "name",
        )

      test("extractColumn returns typed Column[Int] for int field"):
        val instance            = Table[TestUser]
        val ageCol: Column[Int] = instance.column(_.age)
        assertTrue(
          ageCol.label == "age",
          ageCol.name == "age",
        )

      test("extractColumn returns typed Column[Int] for key field"):
        val instance           = Table[TestUser]
        val idCol: Column[Int] = instance.column(_.id)
        assertTrue(
          idCol.label == "id",
          idCol.name == "id",
          idCol.isKey,
        )

      test("extractColumn preserves column metadata"):
        val instance                = Table[TestUser]
        val idCol: Column[Int]      = instance.column(_.id)
        val nameCol: Column[String] = instance.column(_.name)
        assertTrue(
          idCol.isKey,
          !nameCol.isKey,
          !idCol.isNullable,
          !nameCol.isNullable,
        )

      test("table with field named 'column' works correctly"):
        val instance = Table[WithColumnField]
        // Can access the "column" field via the column() method
        val columnCol: Column[String] = instance.column(_.column)
        assertTrue(
          columnCol.label == "column",
          columnCol.name == "column",
        )

      test("table with field named 'column' - selectDynamic still works"):
        val instance = Table[WithColumnField]
        // From within saferis.*, the column method shadows selectDynamic("column")
        // But we can still access via fieldNamesToColumns
        val columnFromMap = instance.fieldNamesToColumns("column")
        assertTrue(
          columnFromMap.label == "column",
          columnFromMap.name == "column",
        )
    ,
    suiteAll("Reserved field names"):
      test("table with field named 'sql' - selectDynamic now works (no collision)"):
        val instance = Table[ReservedNames]
        // After removing Placeholder inheritance, instance.sql returns Column[String]
        // via selectDynamic - no more collision!
        val sqlCol: Column[String] = instance.sql
        assertTrue(
          sqlCol.label == "sql",
          sqlCol.name == "sql",
        )

      test("table with field named 'sql' - can also access via fieldNamesToColumns"):
        val instance = Table[ReservedNames]
        // Both methods work now - selectDynamic and direct map access
        val sqlCol = instance.fieldNamesToColumns("sql")
        assertTrue(
          sqlCol.label == "sql",
          sqlCol.name == "sql",
        )

      test("table with reserved names - can use columns in SQL interpolator"):
        val instance = Table[ReservedNames]
        // The sql interpolator works correctly with any field name
        val sqlCol   = instance.sql // Now works via selectDynamic!
        val fragment = sql"SELECT $sqlCol FROM $instance"
        assertTrue(
          fragment.sql.contains("sql"),
          fragment.sql.contains("reserved_names"),
        )

      test("table with reserved names - Query builder works"):
        // Query builder uses fieldNamesToColumns internally, so it works
        val query    = Query[ReservedNames].where(_.sql).eq("test")
        val fragment = query.build
        assertTrue(
          fragment.sql.contains("sql")
        )

      test("toSql extractor function works"):
        val instance = Table[ReservedNames]
        assertTrue(
          toSql(instance) == "\"reserved_names\""
        )

      test("aliased via 'as' extension method"):
        val instance = Table[ReservedNames]
        val aliased  = instance as "rn"
        assertTrue(
          toSql(aliased) == "\"reserved_names\" as \"rn\""
        ),
  )
end MacroSpecs
