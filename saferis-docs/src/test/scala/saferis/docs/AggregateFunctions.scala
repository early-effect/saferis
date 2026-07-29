package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import saferis.postgres.given
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object AggregateFunctions extends SaferisDocSpecSuite:

  @tableName("aggregate_event_rows")
  case class EventRow(
      @generated @key id: Int,
      instanceId: String,
      sequenceNr: Long,
      amount: BigDecimal,
  ) derives Table

  def doc = page("Aggregate Functions")(
    md"""Saferis provides type-safe aggregate functions with the `selectAggregate` method.""",
    section("Basic Aggregates")(
      exampleValue {
        // MAX aggregate
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(_.sequenceNr)(_.max)
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("max") && sql.contains("sequenceNr"))),
      exampleValue {
        // MIN aggregate
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(_.amount)(_.min)
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("min") && sql.contains("amount"))),
      exampleValue {
        // SUM aggregate
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(_.amount)(_.sum)
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("sum") && sql.contains("amount"))),
      exampleValue {
        // COUNT on a column
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(_.sequenceNr)(_.count)
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("count") && sql.contains("sequenceNr"))),
      exampleValue {
        // COUNT(*) - count all rows
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(countAll)
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("count(*)"))),
    ),
    section("COALESCE for Default Values")(
      md"""Handle NULL results from aggregates with `coalesce`:""",
      exampleValue {
        // MAX with COALESCE - returns 0 if no rows match
        Query[EventRow]
          .where(_.instanceId)
          .eq("instance-1")
          .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("coalesce") && sql.contains("max"))),
      exampleValue {
        // SUM with COALESCE
        Query[EventRow]
          .where(_.instanceId)
          .eq("nonexistent")
          .selectAggregate(_.amount)(_.sum.coalesce(BigDecimal(0)))
          .build
          .sql
      }.assert(sql => assertTrue(sql.contains("coalesce") && sql.contains("sum"))),
    ),
    section("Executing Aggregate Queries")(
      md"""Use `queryValue[T]` to get the aggregate result:""",
      exampleZIO {
        xa.run(
          for
            _      <- ddl.createTable[EventRow](ifNotExists = true)
            _      <- dml.insert(EventRow(-1, "test", 1L, BigDecimal(100)))
            _      <- dml.insert(EventRow(-1, "test", 5L, BigDecimal(200)))
            _      <- dml.insert(EventRow(-1, "test", 3L, BigDecimal(150)))
            maxSeq <- Query[EventRow]
              .where(_.instanceId)
              .eq("test")
              .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
              .queryValue[Long]
            total <- Query[EventRow]
              .where(_.instanceId)
              .eq("test")
              .selectAggregate(_.amount)(_.sum)
              .queryValue[BigDecimal]
            count <- Query[EventRow]
              .where(_.instanceId)
              .eq("test")
              .selectAggregate(countAll)
              .queryValue[Long]
          yield (maxSeq, total, count)
        ).either
      }.assert {
        case Right((maxSeq, total, count)) =>
          assertTrue(
            maxSeq.contains(5L),
            total.contains(BigDecimal(450)),
            count.contains(3L),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("Available Aggregate Functions")(
      md"""| Function | Description |
|----------|-------------|
| `_.max` | Maximum value |
| `_.min` | Minimum value |
| `_.sum` | Sum of values |
| `_.count` | Count of non-null values |
| `_.avg` | Average value |
| `countAll` | Count all rows (`COUNT(*)`) |
| `.coalesce(default)` | Return default if NULL |"""
    ),
  )
end AggregateFunctions
