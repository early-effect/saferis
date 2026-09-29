package saferis.tests

import saferis.*

import zio.test.*

/** A reserved word and a name that contains the dialect's quote character survive create, insert, and select. */
object IdentifierConformance:
  def conformance =
    test("a reserved word and an embedded quote round-trip"):
      SqlSessionConformance.withDialect:
        val quote  = summon[Dialect].identifierQuote
        val table  = TableName("ident_roundtrip")
        val order  = ColumnName("order")
        val tricky = ColumnName(s"a${quote}b")
        for
          _ <- sql"drop table if exists ${identifier(table)}".dml
          _ <- sql"create table ${identifier(table)} (${identifier(order)} integer, ${identifier(tricky)} integer)".dml
          _ <- sql"insert into ${identifier(table)} (${identifier(order)}, ${identifier(tricky)}) values (1, 2)".dml
          got <- sql"select ${identifier(tricky)} from ${identifier(table)} where ${identifier(order)} = ${1}"
            .queryValue[Int]
        yield assertTrue(got.contains(2))
end IdentifierConformance
