package saferis.postgres

import zio.Config.Secret
import zio.test.*

object PgConnectionConfigSpecs extends ZIOSpecDefault:
  private def config(parameters: Map[String, String]): PgConnectionConfig =
    PgConnectionConfig("localhost", 5432, "db", "user", Secret("pw"), parameters = parameters)

  private val required = Map("DateStyle" -> "ISO")

  def spec = suite("startup options")(
    test("with no parameters, only the required ones are sent"):
      assertTrue(config(Map.empty).startupOptions(required) == "-c DateStyle=ISO")
    ,
    test("whitespace in a value is escaped, so the server keeps it in one argument"):
      val options = config(Map("search_path" -> "app, public")).startupOptions(required)
      assertTrue(options == "-c search_path=app,\\ public -c DateStyle=ISO")
    ,
    test("a backslash in a value is escaped"):
      val options = config(Map("application_name" -> "a\\b")).startupOptions(Map.empty)
      assertTrue(options == "-c application_name=a\\\\b")
    ,
    test("a required parameter replaces a user entry of the same name, in any case"):
      val options = config(Map("datestyle" -> "SQL", "search_path" -> "app")).startupOptions(required)
      assertTrue(options == "-c search_path=app -c DateStyle=ISO")
    ,
    test("escaping keeps every character"):
      check(Gen.string): text =>
        val escaped   = PgConnectionConfig.escapeOption(text)
        val unescaped = escaped.replaceAll("(?s)\\\\(.)", "$1")
        assertTrue(unescaped == text),
  )
end PgConnectionConfigSpecs
