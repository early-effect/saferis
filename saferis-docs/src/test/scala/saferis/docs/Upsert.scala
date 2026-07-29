package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object UpsertDocs extends SaferisDocSpecSuite:

  @tableName("upsert_locks")
  case class UpsertLock(
    @key instanceId: String,
    nodeId: String,
    acquiredAt: java.time.Instant,
    expiresAt: java.time.Instant,
  ) derives Table

  @tableName("upsert_items")
  case class UpsertItem(
    @key tenantId: String,
    @key sku: String,
    name: String,
    quantity: Int,
  ) derives Table

  @tableName("upsert_atomic_locks")
  case class AtomicLock(
    @key instanceId: String,
    nodeId: String,
    acquiredAt: java.time.Instant,
    expiresAt: java.time.Instant,
  ) derives Table

  def doc = page("Conditional Upsert DSL")(
    md"""Saferis provides a type-safe UPSERT (INSERT ... ON CONFLICT) builder for PostgreSQL.""",
    section("Basic Upsert")(
      exampleValue {
        // Basic upsert - update all non-key columns on conflict
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doUpdateAll
          .build
          .sql
      }.assert(sql =>
        assertTrue(sql.toLowerCase.contains("insert") && sql.toLowerCase.contains("on conflict") && sql.toLowerCase.contains("do update")),
      ),
    ),
    section("Conditional Upsert with WHERE")(
      md"""Add conditions to control when the update happens:""",
      exampleValue {
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        // Only update if the existing row has expired
        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
      md"""This generates: `INSERT INTO ... ON CONFLICT (instance_id) DO UPDATE SET ... WHERE upsert_locks.expires_at < ?`""",
    ),
    section("Reference EXCLUDED Pseudo-Table")(
      md"""Use `.eqExcluded` to compare with the value being inserted:""",
      exampleValue {
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        // Update only if we own the lock (same nodeId) OR it has expired
        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .or(_.nodeId)
          .eqExcluded
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("excluded"))),
      md"""The `.eqExcluded` generates `table.column = excluded.column`, referencing the value from the INSERT.""",
    ),
    section("Upsert with DO NOTHING")(
      md"""Skip the update entirely on conflict:""",
      exampleValue {
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        // Insert only if no conflict
        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doNothing
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("do nothing"))),
    ),
    section("Upsert with RETURNING")(
      md"""Get the resulting row back:""",
      exampleValue {
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        // Upsert with RETURNING - returns ReturningQuery which wraps SqlFragment
        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doUpdateAll
          .returning
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
      exampleValue {
        val now  = java.time.Instant.now()
        val lock = UpsertLock("instance-1", "node-1", now, now.plusSeconds(60))

        // Type-safe returning with WHERE clause
        saferis.Upsert[UpsertLock]
          .values(lock)
          .onConflict(_.instanceId)
          .doUpdateAll
          .where(_.expiresAt)
          .lt(now)
          .returning
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("returning") && sql.toLowerCase.contains("where"))),
    ),
    section("Compound Conflict Columns")(
      md"""Specify multiple columns for the conflict target:""",
      exampleValue {
        // Conflict on compound key
        val item = UpsertItem("tenant-1", "SKU-001", "Widget", 10)

        saferis.Upsert[UpsertItem]
          .values(item)
          .onConflict(_.tenantId)
          .and(_.sku)
          .doUpdateAll
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("on conflict"))),
    ),
    section("Full Atomic Lock Acquisition Example")(
      md"""Here's a complete example of atomic lock acquisition, run against the database:""",
      exampleZIO {
        xa.run(for
          _    <- ddl.createTable[AtomicLock](ifNotExists = true)
          now  = java.time.Instant.now()
          lock = AtomicLock("lock-1", "node-A", now, now.plusSeconds(60))

          // First acquisition - should succeed
          result1 <- saferis.Upsert[AtomicLock]
            .values(lock)
            .onConflict(_.instanceId)
            .doUpdateAll
            .where(_.expiresAt)
            .lt(now) // Only if expired
            .or(_.nodeId)
            .eqExcluded // Or we own it
            .returning
            .queryOne

          // Second acquisition by same node - should succeed (we own it)
          result2 <- saferis.Upsert[AtomicLock]
            .values(lock.copy(expiresAt = now.plusSeconds(120)))
            .onConflict(_.instanceId)
            .doUpdateAll
            .where(_.expiresAt)
            .lt(now)
            .or(_.nodeId)
            .eqExcluded
            .returning
            .queryOne

        yield (result1, result2)).either
      }.assert {
        case Right((result1, result2)) =>
          assertTrue(result1.exists(_.nodeId == "node-A") && result2.exists(_.nodeId == "node-A"))
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("Capability Requirements")(
      md"""The Upsert DSL requires `UpsertSupport`:
- PostgreSQL: Full support
- MySQL: Not supported (use `ON DUPLICATE KEY UPDATE` syntax via raw SQL)
- SQLite: Not supported

For `returningAs`, also requires `ReturningSupport`:
- PostgreSQL: Full support
- SQLite: Supported
- MySQL: Not supported

See [Type-Safe Capabilities](capabilities.html) for how these constraints are enforced at compile time.""",
    ),
  )
end UpsertDocs
