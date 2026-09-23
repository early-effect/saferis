package saferis.tests

import saferis.*
import zio.test.*

import java.time.Instant

object UpsertSpecs extends ZIOSpecDefault:
  // Test table for upsert - simulates a lock/lease table
  @tableName("upsert_locks")
  final case class LockRow(
      @key @label("instance_id") instanceId: String,
      @label("node_id") nodeId: String,
      @label("acquired_at") acquiredAt: Instant,
      @label("expires_at") expiresAt: Instant,
  ) derives Table

  // ============================================================================
  // SQL Generation Tests (Unit Tests)
  // ============================================================================

  val sqlGenerationTests = suite("SQL Generation")(
    suite("Basic Upsert")(
      test("doUpdateAll generates correct SQL"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val frag   = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .doUpdateAll
          .build
        assertTrue(frag.sql.contains("insert into upsert_locks")) &&
        assertTrue(frag.sql.contains("on conflict (instance_id)")) &&
        assertTrue(frag.sql.contains("do update set")) &&
        assertTrue(frag.sql.contains("node_id = $5"))
      ,
      test("doNothing generates correct SQL"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val frag   = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .doNothing
          .build
        assertTrue(frag.sql.contains("insert into upsert_locks")) &&
        assertTrue(frag.sql.contains("on conflict (instance_id)")) &&
        assertTrue(frag.sql.contains("do nothing"))
      ,
      test("compound conflict columns"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val frag   = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .and(_.nodeId)
          .doUpdateAll
          .build
        assertTrue(frag.sql.contains("on conflict (instance_id, node_id)")),
    ),
    suite("Conditional Upsert")(
      test("WHERE clause on conflict"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val frag   = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .build
        assertTrue(frag.sql.contains("on conflict (instance_id)")) &&
        assertTrue(frag.sql.contains("do update set")) &&
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.sql.contains("expires_at < $8"))
      ,
      test("OR condition with eqExcluded"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val frag   = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .or(_.nodeId)
          .eqExcluded
          .build
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.sql.contains("expires_at < $8")) &&
        assertTrue(frag.sql.contains(" or ")) &&
        assertTrue(frag.sql.contains("node_id = excluded.node_id"))
      ,
      test("RETURNING clause"):
        val now    = Instant.now()
        val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
        val query  = Upsert[LockRow]
          .values(entity)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .or(_.nodeId)
          .eqExcluded
          .returning
        assertTrue(query.build.sql.contains("returning *")),
    ),
  )

  override def spec = suite("UpsertSpecs")(
    sqlGenerationTests
  )
end UpsertSpecs
