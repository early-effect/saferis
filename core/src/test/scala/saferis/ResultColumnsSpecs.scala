package saferis

import zio.Chunk
import zio.test.*

object ResultColumnsSpecs extends ZIOSpecDefault:
  private def name(raw: String): ColumnName = ColumnName(raw)

  def spec = suite("ResultColumns.indexes")(
    test("keeps wanted labels in result order"):
      val labels = Chunk("a", "b", "c").map(name)
      val got    = ResultColumns.indexes(labels, ResultColumns.Labels(Chunk(name("c"), name("a"))))
      assertTrue(got == Chunk(0, 2))
    ,
    test("keeps the first of two identical labels"):
      val labels = Chunk("a", "b", "a").map(name)
      val got    = ResultColumns.indexes(labels, ResultColumns.Labels(Chunk(name("a"), name("b"))))
      assertTrue(got == Chunk(0, 1))
    ,
    test("matches labels without regard to case"):
      val labels = Chunk("Id", "Name").map(name)
      val got    = ResultColumns.indexes(labels, ResultColumns.Labels(Chunk(name("NAME"), name("id"))))
      assertTrue(got == Chunk(0, 1))
    ,
    test("omits a wanted label that the result does not have"):
      val labels = Chunk("a").map(name)
      val got    = ResultColumns.indexes(labels, ResultColumns.Labels(Chunk(name("a"), name("missing"))))
      assertTrue(got == Chunk(0))
    ,
    test("All keeps every index"):
      val labels = Chunk("a", "b").map(name)
      val got    = ResultColumns.indexes(labels, ResultColumns.All)
      assertTrue(got == Chunk(0, 1))
    ,
    test("SqlRow.get uses the same case-insensitive first hit"):
      val row = SqlRow(
        Chunk(name("Id"), name("Name")),
        Chunk(SqlValue.Integer(1), SqlValue.VarChar("ada")),
      )
      assertTrue(
        row.get(name("id")) == Right(SqlValue.Integer(1)),
        row.get(name("NAME")) == Right(SqlValue.VarChar("ada")),
      ),
  )
end ResultColumnsSpecs
