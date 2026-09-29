package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object QueryExecution extends SaferisDocSpecSuite:

  @tableName("query_execution_value_items")
  case class ValueItem(@generated @key id: Int, name: String, price: Double) derives Table

  val items = Table[ValueItem]

  @tableName("query_execution_exec_items")
  case class ExecItem(@generated @key id: Int, name: String, quantity: Int) derives Table

  def doc = page("Query Execution Methods")(
    md"""`SqlFragment` provides several methods for executing queries:

| Method | Returns | Description |
|--------|---------|-------------|
| `.query[T]` | `Chunk[T]` | Execute query, return all matching rows |
| `.queryOne[T]` | `Option[T]` | Execute query, return first row if exists |
| `.queryStream[T]` | `ZStream[..., T]` | Execute query, lazily stream rows (see [Streaming with ZStream](streaming.html)) |
| `.queryValue[T]` | `Option[T]` | Execute query, return single value from first column |
| `.execute` / `.dml` | `Long` | Execute DML statement, return affected row count |""",
    section("queryValue for Single Values")(
      md"""Use `.queryValue[T]` for queries that return a single value (aggregates, counts, etc.):""",
      exampleZIO {
        (for
          _        <- ddl.createTable[ValueItem](ifNotExists = true)
          _        <- dml.insert(ValueItem(-1, "Widget", 10.0))
          _        <- dml.insert(ValueItem(-1, "Gadget", 25.0))
          _        <- dml.insert(ValueItem(-1, "Gizmo", 15.0))
          count    <- sql"SELECT COUNT(*) FROM $items".queryValue[Long]
          maxPrice <- sql"SELECT MAX(${items.price}) FROM $items".queryValue[Double]
          avgPrice <- sql"SELECT AVG(${items.price}) FROM $items".queryValue[Double]
        yield (count, maxPrice, avgPrice)).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right((count, maxPrice, avgPrice)) =>
          assertTrue(count.contains(3) && maxPrice.contains(25.0) && avgPrice.exists(a => a > 16.0 && a < 17.0))
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("execute for Mutation Builders")(
      md"""The mutation builders (Insert, Update, Delete) support `.build.execute` to run the statement:""",
      exampleZIO {
        (for
          _ <- ddl.createTable[ExecItem](ifNotExists = true)
          // Insert using builder and execute
          insertCount <- Insert[ExecItem]
            .value(_.name, "Widget")
            .value(_.quantity, 10)
            .build
            .execute
          // Update using builder and execute
          updateCount <- Update[ExecItem]
            .set(_.quantity, 20)
            .where(_.name)
            .eq("Widget")
            .build
            .execute
          // Verify
          result <- sql"SELECT * FROM ${Table[ExecItem]}".query[ExecItem]
        yield (insertCount, updateCount, result)).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right((insertCount, updateCount, result)) =>
          assertTrue(
            insertCount == 1,
            updateCount == 1,
            result.exists(r => r.name == "Widget" && r.quantity == 20),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
  )
end QueryExecution
