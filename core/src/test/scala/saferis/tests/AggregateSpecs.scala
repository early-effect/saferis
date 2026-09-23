package saferis.tests

import saferis.*
import zio.json.*
import zio.test.*

object AggregateSpecs extends ZIOSpecDefault:
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
      ),
  )

  override def spec = suite("AggregateSpecs")(
    sqlGenerationTests,
    genericLabelTests,
  )
end AggregateSpecs
