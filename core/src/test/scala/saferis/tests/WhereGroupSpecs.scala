package saferis.tests

import saferis.*
import zio.test.*

import java.time.Instant

object WhereGroupSpecs extends ZIOSpecDefault:
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

  override def spec = suite("WhereGroupSpecs")(
    sqlGenerationTests
  )
end WhereGroupSpecs
