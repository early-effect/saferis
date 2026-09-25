package saferis.tests

import saferis.*
import saferis.postgres.PostgresDialect
import zio.test.*

object JsonDialectSpecs extends ZIOSpecDefault:
  private val data = SqlText("data")

  val spec = suite("JSON dialect escaping")(
    // === Dialect SQL Generation with Escaping ===
    suite("PostgreSQL dialect escaping")(
      test("jsonExtractSql escapes single quotes in field path"):
        val sql = PostgresDialect.jsonExtractSql(data, "user's_field")
        assertTrue(sql == "data->>'user''s_field'")
      ,
      test("jsonHasKeySql escapes single quotes in key"):
        val sql = PostgresDialect.jsonHasKeySql(data, "user's_key")
        assertTrue(sql == "jsonb_exists(data, 'user''s_key')")
      ,
      test("jsonContainsSql escapes single quotes in value"):
        val sql = PostgresDialect.jsonContainsSql(data, JsonText("""{"name":"O'Brien"}"""))
        assertTrue(sql == """data @> '{"name":"O''Brien"}'""")
      ,
      test("jsonHasAnyKeySql escapes single quotes in keys"):
        val sql = PostgresDialect.jsonHasAnyKeySql(data, Seq("key1", "user's_key", "key2"))
        assertTrue(sql == "jsonb_exists_any(data, array['key1', 'user''s_key', 'key2'])")
      ,
      test("jsonHasAllKeysSql escapes single quotes in keys"):
        val sql = PostgresDialect.jsonHasAllKeysSql(data, Seq("user's_key", "another's_key"))
        assertTrue(sql == "jsonb_exists_all(data, array['user''s_key', 'another''s_key'])"),
    ),
    suite("MySQL dialect escaping")(
      test("jsonExtractSql escapes single quotes in field path"):
        val sql = saferis.mysql.MySQLDialect.jsonExtractSql(data, "user's_field")
        assertTrue(sql == "JSON_EXTRACT(data, '$.user''s_field')")
      ,
      test("jsonHasKeySql escapes single quotes in key"):
        val sql = saferis.mysql.MySQLDialect.jsonHasKeySql(data, "user's_key")
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'one', '$.user''s_key')")
      ,
      test("jsonContainsSql escapes single quotes in value"):
        val sql = saferis.mysql.MySQLDialect.jsonContainsSql(data, JsonText("""{"name":"O'Brien"}"""))
        assertTrue(sql == """JSON_CONTAINS(data, '{"name":"O''Brien"}')""")
      ,
      test("jsonHasAnyKeySql escapes single quotes in keys"):
        val sql = saferis.mysql.MySQLDialect.jsonHasAnyKeySql(data, Seq("key1", "user's_key"))
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'one', '$.key1', '$.user''s_key')")
      ,
      test("jsonHasAllKeysSql escapes single quotes in keys"):
        val sql = saferis.mysql.MySQLDialect.jsonHasAllKeysSql(data, Seq("user's_key", "another's_key"))
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'all', '$.user''s_key', '$.another''s_key')"),
    ),
    suite("Spark dialect escaping")(
      test("jsonExtractSql escapes single quotes in field path"):
        val sql = saferis.spark.SparkDialect.jsonExtractSql(data, "user's_field")
        assertTrue(sql == "get_json_object(data, '$.user''s_field')")
      ,
      test("jsonHasKeySql escapes single quotes in key"):
        val sql = saferis.spark.SparkDialect.jsonHasKeySql(data, "user's_key")
        assertTrue(sql == "get_json_object(data, '$.user''s_key') is not null")
      ,
      test("jsonHasAnyKeySql escapes single quotes in keys"):
        val sql = saferis.spark.SparkDialect.jsonHasAnyKeySql(data, Seq("key1", "user's_key"))
        assertTrue(
          sql == "(get_json_object(data, '$.key1') is not null or get_json_object(data, '$.user''s_key') is not null)"
        )
      ,
      test("jsonHasAllKeysSql escapes single quotes in keys"):
        val sql = saferis.spark.SparkDialect.jsonHasAllKeysSql(data, Seq("user's_key", "another's_key"))
        assertTrue(
          sql == "(get_json_object(data, '$.user''s_key') is not null and get_json_object(data, '$.another''s_key') is not null)"
        ),
    ),
  )
end JsonDialectSpecs
