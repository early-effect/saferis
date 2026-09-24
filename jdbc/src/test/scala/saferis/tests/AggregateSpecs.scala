package saferis.tests

import saferis.*
import saferis.tests.DataSourceProvider
import zio.*
import zio.json.*
import zio.test.*

object AggregateSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Test table for aggregates
  @tableName("aggregate_test")
  final case class EventRow(
      @generated @key id: Int,
      @label("instance_id") instanceId: String,
      @label("sequence_nr") sequenceNr: Long,
      amount: BigDecimal,
  ) derives Table

  // Generic case class with @label annotations and polymorphic given
  // This tests the specific case where Table instance comes from a companion object
  final case class EventPayload(name: String, value: Int) derives JsonCodec

  @tableName("generic_aggregate_test")
  final case class GenericEventRow[E](
      @generated @key id: Int,
      @label("instance_id") instanceId: String,
      @label("sequence_nr") sequenceNr: Long,
      payload: Json[E],
  )
  object GenericEventRow:
    given [E: JsonCodec]: Table[GenericEventRow[E]] = Table.derived

  // ============================================================================
  // Integration Tests (with PostgreSQL)
  // ============================================================================

  val integrationTests = suite("Integration")(
    test("max returns correct value"):
      for
        _      <- (ddl.createTable[EventRow]())
        _      <- (dml.insert(EventRow(-1, "test-1", 1L, BigDecimal(100))))
        _      <- (dml.insert(EventRow(-1, "test-1", 5L, BigDecimal(200))))
        _      <- (dml.insert(EventRow(-1, "test-1", 3L, BigDecimal(150))))
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(_.sequenceNr)(_.max)
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(5L))
    ,
    test("min returns correct value"):
      for
        _      <- (ddl.createTable[EventRow]())
        _      <- (dml.insert(EventRow(-1, "test-1", 1L, BigDecimal(100))))
        _      <- (dml.insert(EventRow(-1, "test-1", 5L, BigDecimal(200))))
        _      <- (dml.insert(EventRow(-1, "test-1", 3L, BigDecimal(150))))
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(_.sequenceNr)(_.min)
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(1L))
    ,
    test("sum returns correct value"):
      for
        _      <- (ddl.createTable[EventRow]())
        _      <- (dml.insert(EventRow(-1, "test-1", 1L, BigDecimal(100))))
        _      <- (dml.insert(EventRow(-1, "test-1", 2L, BigDecimal(200))))
        _      <- (dml.insert(EventRow(-1, "test-1", 3L, BigDecimal(150))))
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(_.amount)(_.sum)
            .queryValue[BigDecimal]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(BigDecimal(450)))
    ,
    test("count returns correct value"):
      for
        _      <- (ddl.createTable[EventRow]())
        _      <- (dml.insert(EventRow(-1, "test-1", 1L, BigDecimal(100))))
        _      <- (dml.insert(EventRow(-1, "test-1", 2L, BigDecimal(200))))
        _      <- (dml.insert(EventRow(-1, "test-2", 1L, BigDecimal(50))))
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(countAll)
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(2L))
    ,
    test("max with coalesce returns default for empty result"):
      for
        _ <- (ddl.createTable[EventRow]())
        // No rows for "nonexistent"
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("nonexistent")
            .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(0L))
    ,
    test("max with coalesce returns actual value when rows exist"):
      for
        _      <- (ddl.createTable[EventRow]())
        _      <- (dml.insert(EventRow(-1, "test-1", 42L, BigDecimal(100))))
        result <- (
          Query[EventRow]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[EventRow]())
      yield assertTrue(result.contains(42L)),
  ).provideShared(xaLayer) @@ TestAspect.sequential

  // ============================================================================
  // Generic Type with @label Tests (Polymorphic Given)
  // ============================================================================

  val genericLabelTests = suite("Generic type with @label and polymorphic given")(
    test("selectAggregate with generic type executes correctly"):
      for
        _      <- (ddl.dropTable[GenericEventRow[EventPayload]](ifExists = true))
        _      <- (ddl.createTable[GenericEventRow[EventPayload]]())
        _      <- (dml.insert(GenericEventRow(-1, "test-1", 1L, Json(EventPayload("a", 10)))))
        _      <- (dml.insert(GenericEventRow(-1, "test-1", 5L, Json(EventPayload("b", 20)))))
        _      <- (dml.insert(GenericEventRow(-1, "test-1", 3L, Json(EventPayload("c", 30)))))
        result <- (
          Query[GenericEventRow[EventPayload]]
            .where(_.instanceId)
            .eq("test-1")
            .selectAggregate(_.sequenceNr)(_.max)
            .queryValue[Long]
        )
        _ <- (ddl.dropTable[GenericEventRow[EventPayload]]())
      yield assertTrue(result.contains(5L))
  ).provideShared(xaLayer) @@ TestAspect.sequential

  override def spec = suite("AggregateSpecs")(
    integrationTests,
    genericLabelTests,
  )
end AggregateSpecs
