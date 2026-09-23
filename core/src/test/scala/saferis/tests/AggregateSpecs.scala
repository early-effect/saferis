package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
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
  // SQL Generation Tests (Unit Tests)
  // ============================================================================

  val sqlGenerationTests = suite("SQL Generation")(
    suite("Basic Aggregates")(
      test("max generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.sequenceNr)(_.max)
          .build
        // Verify @label is respected - should use "sequence_nr" not "sequenceNr"
        assertTrue(
          frag.sql.contains("select max(sequence_nr) from aggregate_test"),
          frag.sql.contains("where"),
          frag.sql.contains("instance_id"),
          !frag.sql.contains("sequenceNr"),
          !frag.sql.contains("instanceId"),
        )
      ,
      test("min generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.sequenceNr)(_.min)
          .build
        assertTrue(frag.sql.contains("select min(sequence_nr) from aggregate_test"))
      ,
      test("sum generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.amount)(_.sum)
          .build
        assertTrue(frag.sql.contains("select sum(amount) from aggregate_test"))
      ,
      test("count generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.sequenceNr)(_.count)
          .build
        assertTrue(frag.sql.contains("select count(sequence_nr) from aggregate_test"))
      ,
      test("countAll generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(countAll)
          .build
        assertTrue(frag.sql.contains("select count(*) from aggregate_test")),
    ),
    suite("COALESCE")(
      test("max with coalesce generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
          .build
        assertTrue(frag.sql.contains("select coalesce(max(sequence_nr), $1) from aggregate_test")) &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2) // instanceId + default value
      ,
      test("sum with coalesce generates correct SQL"):
        val frag = Query[EventRow]
          .where(_.instanceId)
          .eq("test-1")
          .selectAggregate(_.amount)(_.sum.coalesce(BigDecimal(0)))
          .build
        assertTrue(frag.sql.contains("select coalesce(sum(amount), $1) from aggregate_test")),
    ),
  )

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
    test("DEBUG: Table.derived columns have correct labels"):
      val genericTable    = summon[Table[GenericEventRow[EventPayload]]]
      val nonGenericTable = summon[Table[EventRow]]

      println("=== GENERIC TABLE ===")
      println(s"Table name: ${genericTable.name}")
      genericTable.columns.foreach { c =>
        println(s"  Column: name='${c.name}', label='${c.label}'")
      }

      println("=== NON-GENERIC TABLE ===")
      println(s"Table name: ${nonGenericTable.name}")
      nonGenericTable.columns.foreach { c =>
        println(s"  Column: name='${c.name}', label='${c.label}'")
      }

      assertTrue(
        genericTable.columns.exists(c => c.name == "instanceId" && c.label == "instance_id"),
        genericTable.columns.exists(c => c.name == "sequenceNr" && c.label == "sequence_nr"),
      )
    ,
    test("selectAggregate uses @label for column name in SQL"):
      val frag = Query[GenericEventRow[EventPayload]]
        .where(_.instanceId)
        .eq("test-1")
        .selectAggregate(_.sequenceNr)(_.max)
        .build
      // Should use "sequence_nr" (from @label), not "sequencenr" (lowercased field name)
      assertTrue(
        frag.sql.contains("max(sequence_nr)"),
        !frag.sql.contains("sequencenr"),
        !frag.sql.contains("sequenceNr"),
      )
    ,
    test("where clause uses @label for column name in SQL"):
      val frag = Query[GenericEventRow[EventPayload]]
        .where(_.instanceId)
        .eq("test-1")
        .build
      // Should use "instance_id" (from @label), not "instanceid" (lowercased field name)
      assertTrue(
        frag.sql.contains("instance_id"),
        !frag.sql.contains("instanceid"),
        !frag.sql.contains("instanceId"),
      )
    ,
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
      yield assertTrue(result.contains(5L)),
  ).provideShared(xaLayer) @@ TestAspect.sequential

  override def spec = suite("AggregateSpecs")(
    sqlGenerationTests,
    integrationTests,
    genericLabelTests,
  )
end AggregateSpecs
