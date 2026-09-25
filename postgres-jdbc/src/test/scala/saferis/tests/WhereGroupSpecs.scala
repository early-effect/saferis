package saferis.tests

import saferis.*
import saferis.tests.DataSourceProvider
import zio.*
import zio.test.*

import java.time.Instant

object WhereGroupSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Test table with non-Option types for comparison tests
  @tableName("where_group_test")
  final case class TestRow(
      @generated @key id: Int,
      status: String,
      @label("claimed_by") claimedBy: String,
      @label("claimed_until") claimedUntil: Instant,
      deadline: Instant,
      priority: Int,
      active: Boolean,
  ) derives Table

  // Table with Option fields for is null tests
  @tableName("where_group_nullable")
  final case class NullableRow(
      @generated @key id: Int,
      status: Option[String],
      @label("claimed_by") claimedBy: Option[String],
      @label("claimed_until") claimedUntil: Option[Instant],
      deadline: Instant,
      active: Boolean,
  ) derives Table

  // ============================================================================
  // Integration Tests (with PostgreSQL)
  // ============================================================================

  val integrationTests = suite("Integration")(
    test("Query with andWhere executes correctly"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[TestRow]())
        // Insert test data
        _ <- (
          dml.insert(TestRow(-1, "active", "node-1", now.plusSeconds(60), now, 1, true))
        )
        _ <- (dml.insert(TestRow(-1, "pending", "", now, now.minusSeconds(60), 2, true)))
        _ <- (
          dml.insert(TestRow(-1, "expired", "node-2", now.minusSeconds(60), now, 3, false))
        )
        // Query using andWhere: active AND (status = 'pending' OR status = 'expired')
        results <- (
          Query[TestRow]
            .where(_.active)
            .eq(true)
            .andWhere(w => w(_.status).eq("pending").or(_.status).eq("expired"))
            .query[TestRow]
        )
        _ <- (ddl.dropTable[TestRow]())
      yield assertTrue(results.size == 1) && // Only the pending row (active=true)
        assertTrue(results.head.status == "pending")
      end for
    ,
    test("Query with mixed type OR condition"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[TestRow]())
        // Insert test data
        _ <- (dml.insert(TestRow(-1, "pending", "", now.plusSeconds(60), now, 1, true)))
        _ <- (dml.insert(TestRow(-1, "active", "node-1", now.minusSeconds(60), now, 2, true))) // expired claim
        _ <- (dml.insert(TestRow(-1, "active", "node-2", now.plusSeconds(60), now, 3, true)))  // valid claim
        // Query: active AND (claimedBy = '' OR claimedUntil < now) - find claimable rows
        results <- (
          Query[TestRow]
            .where(_.active)
            .eq(true)
            .andWhere(w => w(_.claimedBy).eq("").or(_.claimedUntil).lt(now))
            .query[TestRow]
        )
        _ <- (ddl.dropTable[TestRow]())
      yield assertTrue(results.size == 2) // unclaimed + expired claim
      end for
    ,
    test("Update with andWhere updates correct rows"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[TestRow]())
        // Insert test data
        _ <- (
          dml.insert(TestRow(-1, "pending", "", now, now.plusSeconds(60), 1, true))
        )
        _ <- (
          dml.insert(TestRow(-1, "pending", "other-node", now.plusSeconds(300), now.plusSeconds(60), 2, true))
        )
        // Update only rows with empty claimedBy OR low priority
        updated <- (
          Update[TestRow]
            .set(_.claimedBy, "my-node")
            .set(_.priority, 10)
            .where(_.status)
            .eq("pending")
            .andWhere(w => w(_.claimedBy).eq("").or(_.priority).lt(2))
            .build
            .update
        )
        remaining <- (Query[TestRow].where(_.claimedBy).eq("").query[TestRow])
        _         <- (ddl.dropTable[TestRow]())
      yield assertTrue(updated == 1) && // Only one row updated (empty claimedBy)
        assertTrue(remaining.isEmpty)   // The claimable row was claimed
      end for
    ,
    test("Query with is null in andWhere"):
      val now = Instant.now()
      val res = for
        _ <- (ddl.createTable[NullableRow]())
        // Insert test data
        _ <- (dml.insert(NullableRow(-1, Some("active"), Some("node-1"), Some(now.plusSeconds(60)), now, true)))
        _ <- (dml.insert(NullableRow(-1, None, None, None, now, true)))
        _ <- (dml.insert(NullableRow(-1, Some("pending"), None, Some(now.minusSeconds(60)), now, true)))
        // Query: active AND (status is null OR claimedBy is null)
        results <- (
          Query[NullableRow]
            .where(_.active)
            .eq(true)
            .andWhere(w => w(_.status).isNull.or(_.claimedBy).isNull)
            .query[NullableRow]
        )
        _ <- (ddl.dropTable[NullableRow]())
      yield assertTrue(results.size == 2)
      res,
    // status=None row and claimedBy=None rows
  ).provideShared(xaLayer) @@ TestAspect.sequential

  override def spec = suite("WhereGroupSpecs")(
    integrationTests
  )
end WhereGroupSpecs
