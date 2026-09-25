package saferis.docs

import specular.*

object Databases extends SaferisDocSpecSuite:

  def doc = page("Databases")(
    md"""Add `saferis` and the one adapter module for your database. Each adapter brings `saferis-jdbc` (or the Node driver) and its own driver, and its layer turns your pool (`javax.sql.DataSource`) into a `SqlSession`. Everything above the session, `sql"..."`, `Table`, the query builder, and `transact`, is the same on every database.

| Database | Module | Layer | Dialect |
|----------|--------|-------|---------|
| PostgreSQL | `saferis-postgres-jdbc` | `PostgresJdbc.layer()` | default |
| PostgreSQL on Node | `saferis-postgres-node` | `NodeSession.layer` | default |
| MySQL | `saferis-mysql-jdbc` | `MySqlJdbc.layer()` | `import saferis.mysql.given` |
| SQLite | `saferis-sqlite-jdbc` | `SqliteJdbc.layer()` | `import saferis.sqlite.given` |
| H2 | `saferis-h2-jdbc` | `H2Jdbc.layer()` | `import saferis.h2.given` |

Each adapter maps its driver's errors to the same SQLSTATEs, so `SaferisError.UniqueViolation`, `Deadlock`, and the rest mean the same thing everywhere. Every shipped adapter runs one conformance suite, provided as a layer, and so does a database you bring (see [Getting Started](getting-started.html)).

This page says what each adapter does that the others do not.""",
    section("PostgreSQL")(
      md"""- **Streams** read through a server-side cursor. Over JDBC a pool stream opens a read transaction with a fetch size of 256, because pgjdbc only honors a fetch size with autocommit off, so a long stream holds that transaction open. On Node the stream pulls 256 rows per `pg-cursor` read.
- **Arrays, enums, `jsonb`, `RETURNING`, and upsert** are available. The query builder's `.inList` binds one array parameter, `= ANY($$1)`.
- **`Schema.verify`** reads `information_schema` and `pg_catalog`.
- **Node** always sets `DateStyle=ISO`, which its decoding depends on; a `DateStyle` in `PgConnectionConfig.parameters` is replaced. The pool turns on TCP keepalive, so a peer that vanished fails a `COMMIT` instead of hanging it. npm `pg@8.16.3` and `pg-cursor@2.22.0` are required at run time."""
    ),
    section("MySQL")(
      md"""- **The session time zone is UTC.** Every checkout runs `SET time_zone = '+00:00'` before your `configure`, so a `timestamp` column stores and returns UTC and an `Instant` round-trips whatever zone the JVM or server is in. That also means `NOW()` and `CURRENT_TIMESTAMP` in your own SQL return UTC. A `configure` that sets another time zone runs after it and wins, at the cost of that round trip.
- **Streams read row by row** (Connector/J's streaming mode, fetch size `Integer.MIN_VALUE`). While a stream is open its connection cannot run another statement, so inside `transact` finish the stream before the next statement.
- **DDL**: `Numeric` is `decimal(65, 30)`, `Timestamp` is `datetime(6)`, `Timestamptz` is `timestamp(6)`, and `Time` is `time(6)`. `import saferis.mysql.given` makes a `UUID` a `char(36)`.
- **Tables created by Saferis 0.19** used `decimal` (which is `decimal(10,0)`) and `timestamp` for `LocalDateTime` too. A `LocalDateTime` field on such a `timestamp` column now reads as an instant and fails to decode; change the column to `datetime(6)` or the field to `Instant`.
- **Errors**: vendor codes map to the shared SQLSTATEs. 1062 is a unique violation named by its key, 1451 and 1452 a foreign-key violation named by its constraint, 1048 not null, 3819 check, 1213 deadlock, and 3024 a canceled statement.
- **No array parameters**: the query builder binds `IN (...)`, and binding an array fails with `SaferisError.Unsupported`. MySQL has no `RETURNING` or upsert capability.
- **`Schema.verify`** reads `information_schema`, with names folded to lower case as MySQL compares them."""
    ),
    section("SQLite")(
      md"""- **Every checkout** turns on `foreign_keys`, `journal_mode = WAL`, and a 5 second `busy_timeout`, before your `configure`. WAL persists on the database file and creates `-wal` and `-shm` files next to it, and it does not work on network file systems. With WAL a reader never blocks a writer, and a writer that meets another waits instead of failing; one that still cannot get the lock fails with a retryable `SerializationFailure`.
- **Checkouts are not serialized.** A single-permit session would deadlock a stream that writes each row through another connection. SQLite's own locking and the busy timeout serialize writers instead.
- **Types**: columns read by the type they were declared with (see [Dialect System](dialect-system.html)). Dates, times, and timestamps are ISO-8601 text, every integer is 64 bits, and a decimal uses SQLite's `numeric` affinity, which stores it as a number with SQLite's precision. A `json` column has numeric affinity too, so a document that is a bare number, such as `1e5`, is stored as a number and reads back as SQLite prints it.
- **Tables created by Saferis 0.19** declared booleans as `integer`, every float and decimal as `real`, and dates and timestamps as `text`. On those columns a boolean or date/time field fails to decode; recreate those tables from the current DDL. Floats and doubles still read correctly, because a `real` column reads as the 8-byte double SQLite stores.
- **Errors**: SQLite result codes map to the shared SQLSTATEs: unique and primary key, foreign key, not null, check, busy and locked (retryable), interrupt, and syntax, missing table, and missing column.
- **`Schema.verify`** reads the pragma table functions. SQLite does not keep the names of unique or foreign-key constraints, so those match by columns and `strictNameMatching` cannot match their names.
- **No array parameters**: binding one fails with `SaferisError.Unsupported`."""
    ),
    section("H2")(
      md"""H2 is here for fast in-memory tests of code that runs on another database in production. `H2Jdbc.memory("name")` is a `DataSource` for a named in-memory database that lives until the JVM exits, with `DATABASE_TO_LOWER` set so unquoted names fold to lower case as they do on PostgreSQL. `H2Dialect` renders H2's own SQL and claims no optional capability, so the query builder binds `IN (...)`, and there is no `RETURNING`, upsert, or `Schema.verify`. JSON binds as UTF-8 bytes, which H2 reads as JSON text."""
    ),
  )
end Databases
