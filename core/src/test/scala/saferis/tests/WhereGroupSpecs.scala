package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
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
  // SQL Generation Tests (Unit Tests)
  // ============================================================================

  val sqlGenerationTests = suite("SQL Generation")(
    suite("WhereGroup")(
      test("single condition generates correct SQL"):
        val sql = Query[TestRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.status).eq("pending"))
          .build
          .sql
        assertTrue(sql.contains("active = $1")) &&
        assertTrue(sql.contains("and")) &&
        assertTrue(sql.contains("status = $2"))
      ,
      test("OR condition with same type generates parenthesized SQL"):
        val frag = Query[TestRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.status).eq("pending").or(_.status).eq("active"))
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("status = $2"))
      ,
      test("OR with different columns different types generates correct SQL"):
        val now  = Instant.now()
        val frag = Query[TestRow]
          .where(_.deadline)
          .lte(now)
          .andWhere(w => w(_.status).eq("pending").or(_.claimedUntil).lt(now))
          .build
        assertTrue(frag.sql.contains("deadline <= $1")) &&
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains("status = $2")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("claimed_until < $3")) &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3) // deadline + status + claimedUntil
      ,
      test("multiple OR conditions chain correctly"):
        val frag = Query[TestRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.status).eq("pending").or(_.status).eq("active").or(_.status).eq("retry"))
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 4) && // active + 3 status values
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 4)
      ,
      test("AND within group generates correct SQL"):
        val frag = Query[TestRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.status).eq("claimed").and(_.claimedBy).eq("node-1"))
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains(" and ")) &&
        assertTrue(frag.sql.contains("status = $2")) &&
        assertTrue(frag.sql.contains("claimed_by = $3"))
      ,
      test("is null in group generates correct SQL"):
        val frag = Query[NullableRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.status).isNull.or(_.claimedBy).isNotNull)
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains("status is null")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("claimed_by is not null"))
      ,
      test("numeric OR condition"):
        val frag = Query[TestRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.priority).lt(5).or(_.priority).gt(10))
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains("priority < $2")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("priority > $3"))
      ,
      test("mixed types in OR chain - string and instant"):
        val now  = Instant.now()
        val frag = Query[NullableRow]
          .where(_.active)
          .eq(true)
          .andWhere(w => w(_.claimedBy).isNull.or(_.claimedUntil).lt(Some(now)))
          .build
        assertTrue(frag.sql.contains("(")) &&
        assertTrue(frag.sql.contains("claimed_by is null")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("claimed_until < $2")),
    ),
    suite("Update andWhere")(
      test("Update with andWhere generates correct SQL"):
        val now  = Instant.now()
        val frag = Update[TestRow]
          .set(_.claimedBy, "node-1")
          .set(_.claimedUntil, now)
          .where(_.id)
          .eq(123)
          .andWhere(w => w(_.status).eq("pending").or(_.claimedUntil).lt(now))
          .build
        assertTrue(frag.sql.contains("update where_group_test set")) &&
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.sql.contains("id = $3")) &&
        assertTrue(frag.sql.contains("status = $4 or")) &&
        assertTrue(frag.sql.contains("claimed_until < $5"))
    ),
    suite("Delete andWhere")(
      test("Delete with andWhere generates correct SQL"):
        val frag = Delete[TestRow]
          .where(_.active)
          .eq(false)
          .andWhere(w => w(_.status).eq("deleted").or(_.claimedBy).eq(""))
          .build
        assertTrue(frag.sql.contains("delete from where_group_test")) &&
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.sql.contains("active = $1")) &&
        assertTrue(frag.sql.contains("status = $2 or")) &&
        assertTrue(frag.sql.contains("claimed_by = $3"))
    ),
  )

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
    sqlGenerationTests,
    integrationTests,
  )
end WhereGroupSpecs
