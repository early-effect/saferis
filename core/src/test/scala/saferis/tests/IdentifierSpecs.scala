package saferis.tests

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.postgres.PostgresDialect
import saferis.spark.SparkDialect
import saferis.sqlite.SQLiteDialect

import zio.test.*

object IdentifierSpecs extends ZIOSpecDefault:

  def spec = suite("identifier quoting")(
    law(PostgresDialect),
    law(MySQLDialect),
    law(SQLiteDialect),
    law(SparkDialect),
    test("toCommand quotes with the dialect at the call site, not the companion default"):
      given Dialect = MySQLDialect
      sql"select ${identifier(ColumnName("order"))}".toCommand.map: command =>
        assertTrue(command.inspection == "select `order`"),
  )

  /** `escapeIdentifier`, an `Ident` piece, and unquoting agree for every string, including the quote character. */
  private def law(dialect: Dialect) =
    test(s"${dialect.name} quotes a name and reads it back"):
      val quote = dialect.identifierQuote
      val names =
        Gen.string.flatMap: body =>
          Gen.elements(body, body + quote, s" $body", "order", "select", quote, "")
      check(names): raw =>
        given Dialect = dialect
        val expected  = Dialect.quote(quote, raw)
        val quoted    = dialect.escapeIdentifier(ColumnName(raw))
        val viaIdent  = SqlFragment.ident(ColumnName(raw)).sql
        assertTrue(quoted == expected, viaIdent == expected, unquote(quoted, quote) == raw)

  private def unquote(quoted: String, quote: String): String =
    quoted.stripPrefix(quote).stripSuffix(quote).replace(quote + quote, quote)
end IdentifierSpecs
