package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*

import java.time.Instant

object UpsertSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

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

  // ============================================================================
  // Integration Tests (with PostgreSQL)
  // ============================================================================

  val integrationTests = suite("Integration")(
    test("basic upsert inserts new row"):
      val now    = Instant.now()
      val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
      for
        _ <- (ddl.createTable[LockRow]())
        // First insert
        _ <- (Upsert[LockRow].values(entity).onConflict(_.instanceId).doUpdateAll.build.dml)
        // Verify
        results <- (Query[LockRow].where(_.instanceId).eq("instance-1").query[LockRow])
        _       <- (ddl.dropTable[LockRow]())
      yield assertTrue(results.size == 1) &&
        assertTrue(results.head.nodeId == "node-1")
      end for
    ,
    test("basic upsert updates existing row"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[LockRow]())
        // First insert
        _ <- (dml.insert(LockRow("instance-1", "node-1", now, now.plusSeconds(60))))
        // Upsert with different nodeId
        _ <- (
          Upsert[LockRow]
            .values(LockRow("instance-1", "node-2", now.plusSeconds(10), now.plusSeconds(120)))
            .onConflict(_.instanceId)
            .doUpdateAll
            .build
            .dml
        )
        // Verify - should be updated
        results <- (Query[LockRow].where(_.instanceId).eq("instance-1").query[LockRow])
        _       <- (ddl.dropTable[LockRow]())
      yield assertTrue(results.size == 1) &&
        assertTrue(results.head.nodeId == "node-2")
      end for
    ,
    test("conditional upsert only updates when condition met"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[LockRow]())
        // Insert with future expiry (not expired)
        _ <- (dml.insert(LockRow("instance-1", "node-1", now, now.plusSeconds(600))))
        // Try to upsert with condition that expiry < now (should NOT update because not expired)
        count <- (
          Upsert[LockRow]
            .values(LockRow("instance-1", "node-2", now.plusSeconds(10), now.plusSeconds(120)))
            .onConflict(_.instanceId)
            .doUpdateAll
            .where(_.expiresAt)
            .lt(now)
            .build
            .dml
        )
        // Verify - should still have original values
        results <- (Query[LockRow].where(_.instanceId).eq("instance-1").query[LockRow])
        _       <- (ddl.dropTable[LockRow]())
      yield assertTrue(count == 0) && // No rows affected because condition not met
        assertTrue(results.head.nodeId == "node-1")
      end for
    ,
    test("conditional upsert with OR nodeId = EXCLUDED.nodeId"):
      val now = Instant.now()
      for
        _ <- (ddl.createTable[LockRow]())
        // Insert with future expiry (not expired) but same node
        _ <- (dml.insert(LockRow("instance-1", "node-1", now, now.plusSeconds(600))))
        // Upsert with same node - should update because nodeId matches EXCLUDED
        count <- (
          Upsert[LockRow]
            .values(LockRow("instance-1", "node-1", now.plusSeconds(10), now.plusSeconds(120)))
            .onConflict(_.instanceId)
            .doUpdateAll
            .where(_.expiresAt)
            .lt(now)
            .or(_.nodeId)
            .eqExcluded
            .build
            .dml
        )
        // Verify - should be updated because nodeId = EXCLUDED.nodeId
        results <- (Query[LockRow].where(_.instanceId).eq("instance-1").query[LockRow])
        _       <- (ddl.dropTable[LockRow]())
      yield assertTrue(count == 1) && // Updated because same node
        assertTrue(results.head.expiresAt.isAfter(now.plusSeconds(100)))
      end for
    ,
    test("upsert with returning"):
      val now    = Instant.now()
      val entity = LockRow("instance-1", "node-1", now, now.plusSeconds(60))
      for
        _      <- (ddl.createTable[LockRow]())
        result <- (
          Upsert[LockRow]
            .values(entity)
            .onConflict(_.instanceId)
            .doUpdateAll
            .returning
            .queryOne
        )
        _ <- (ddl.dropTable[LockRow]())
      yield assertTrue(result.isDefined) &&
        assertTrue(result.get.instanceId == "instance-1")
      end for
    ,
    test("doNothing ignores conflict"):
      val now = Instant.now()
      val res =
        for
          _ <- (ddl.createTable[LockRow]())
          // First insert
          _ <- (dml.insert(LockRow("instance-1", "node-1", now, now.plusSeconds(60))))
          // Try to insert with DO NOTHING - should be ignored
          count <- (
            Upsert[LockRow]
              .values(LockRow("instance-1", "node-2", now.plusSeconds(10), now.plusSeconds(120)))
              .onConflict(_.instanceId)
              .doNothing
              .build
              .dml
          )
          // Verify - should still have original values
          results <- (Query[LockRow].where(_.instanceId).eq("instance-1").query[LockRow])
          _       <- (ddl.dropTable[LockRow]())
        yield assertTrue(count == 0) && // No rows affected (conflict ignored)
          assertTrue(results.head.nodeId == "node-1")
      end res
      res,
  ).provideShared(xaLayer) @@ TestAspect.sequential

  override def spec = suite("UpsertSpecs")(
    sqlGenerationTests,
    integrationTests,
  )
end UpsertSpecs
