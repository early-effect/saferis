package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Dml extends SaferisDocSpecSuite:

  @tableName("dml_tasks")
  case class Task(@generated @key id: Int, title: String, done: Boolean) derives Table

  val tasks = Table[Task]

  @tableName("dml_items")
  case class Item(@generated @key id: Int, name: String, quantity: Int) derives Table

  val items = Table[Item]

  @tableName("dml_logs")
  case class LogEntry(@generated @key id: Int, level: String, message: String) derives Table

  val logs = Table[LogEntry]

  @tableName("dml_builder_users")
  case class BuilderUser(@generated @key id: Int, name: String, email: String, age: Int) derives Table

  @tableName("dml_claim_tasks")
  case class ClaimTask(
    @generated @key id: Int,
    deadline: java.time.Instant,
    claimedBy: Option[String],
    claimedUntil: Option[java.time.Instant],
  ) derives Table

  @tableName("dml_lock_rows")
  case class LockRow(
    @key instanceId: String,
    nodeId: String,
    expiresAt: java.time.Instant,
  ) derives Table

  def doc = page("Data Manipulation Layer (DML)")(
    md"""The DML layer provides CRUD operations for your tables.""",
    section("Basic CRUD Operations")(
      exampleValue {
        // SELECT query
        sql"SELECT * FROM $tasks WHERE ${tasks.done} = ${false}".show
      }.assert(s => assertTrue(s.contains("dml_tasks") && s.contains("done"))),
      exampleValue {
        // SELECT with multiple conditions
        sql"SELECT * FROM $tasks WHERE ${tasks.title} LIKE ${"Learn%"} AND ${tasks.done} = ${false}".show
      }.assert(s => assertTrue(s.contains("LIKE") && s.contains("done"))),
    ),
    section("Running DML Operations")(
      exampleZIO {
        // Full workflow with actual database
        xa.run(for
          _    <- ddl.createTable[Task](ifNotExists = true)
          _    <- dml.insert(Task(-1, "Task 1", false))
          _    <- dml.insert(Task(-1, "Task 2", false))
          _    <- dml.insert(Task(-1, "Task 3", true))
          all  <- sql"SELECT * FROM $tasks".query[Task]
          done <- sql"SELECT * FROM $tasks WHERE ${tasks.done} = ${true}".query[Task]
        yield (all, done)).either
      }.assert {
        case Right((all, done)) => assertTrue(all.size >= 3 && done.exists(_.done))
        case Left(err)          => assertTrue(false).label(err.message)
      },
    ),
    section("Insert with RETURNING")(
      md"""For databases that support it (PostgreSQL, SQLite), get the inserted row back:""",
      exampleZIO {
        xa.run(for
          _      <- ddl.createTable[Task](ifNotExists = true)
          result <- dml.insertReturning(Task(-1, "New Task", false))
        yield result).either
      }.assert {
        case Right(result) => assertTrue(result.title == "New Task" && !result.done)
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Custom Queries")(
      md"""Use the `sql` interpolator for any query:""",
      exampleZIO {
        // Query with ordering
        xa.run(for
          _      <- ddl.createTable[Task](ifNotExists = true)
          _      <- dml.insert(Task(-1, "Alpha", false))
          _      <- dml.insert(Task(-1, "Beta", true))
          sorted <- sql"SELECT * FROM $tasks ORDER BY ${tasks.title}".query[Task]
        yield sorted).either
      }.assert {
        case Right(sorted) => assertTrue(sorted.nonEmpty)
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Update Operations")(
      md"""Update records by primary key or with custom conditions:""",
      exampleZIO {
        xa.run(for
          _        <- ddl.createTable[Item](ifNotExists = true)
          inserted <- dml.insertReturning(Item(-1, "Widget", 10))

          // Update by primary key
          _ <- dml.update(inserted.copy(quantity = 15))

          // Update with RETURNING (get the updated row back)
          updated <- dml.updateReturning(inserted.copy(name = "Super Widget", quantity = 20))

          // Verify the update
          result <- sql"SELECT * FROM $items WHERE ${items.id} = ${inserted.id}".queryOne[Item]
        yield (updated, result)).either
      }.assert {
        case Right((updated, result)) =>
          assertTrue(updated.name == "Super Widget" && updated.quantity == 20 && result.exists(_.quantity == 20))
        case Left(err) => assertTrue(false).label(err.message)
      },
      md"""Update multiple rows with a WHERE clause:""",
      exampleZIO {
        xa.run(for
          _ <- ddl.createTable[Item](ifNotExists = true)
          _ <- dml.insert(Item(-1, "Gadget A", 5))
          _ <- dml.insert(Item(-1, "Gadget B", 3))

          // Update all items with quantity < 10
          rowsUpdated <- dml.updateWhere(
            Item(-1, "Low Stock Item", 0), // Values to set (id ignored)
            sql"${items.quantity} < 10",
          )

          all <- sql"SELECT * FROM $items".query[Item]
        yield (rowsUpdated, all)).either
      }.assert {
        case Right((rowsUpdated, all)) => assertTrue(rowsUpdated >= 2 && all.exists(_.name == "Low Stock Item"))
        case Left(err)                 => assertTrue(false).label(err.message)
      },
    ),
    section("Delete Operations")(
      md"""Delete records by primary key or with custom conditions:""",
      exampleZIO {
        xa.run(for
          _      <- ddl.createTable[LogEntry](ifNotExists = true)
          entry1 <- dml.insertReturning(LogEntry(-1, "INFO", "Application started"))
          entry2 <- dml.insertReturning(LogEntry(-1, "DEBUG", "Processing request"))
          _      <- dml.insertReturning(LogEntry(-1, "ERROR", "Something failed"))

          // Delete by primary key
          _ <- dml.delete(entry2)

          // Delete with RETURNING (get the deleted row back)
          deleted <- dml.deleteReturning(entry1)

          remaining <- sql"SELECT * FROM $logs".query[LogEntry]
        yield (deleted, remaining)).either
      }.assert {
        case Right((deleted, remaining)) =>
          assertTrue(deleted.message == "Application started" && !remaining.exists(_.id == deleted.id))
        case Left(err) => assertTrue(false).label(err.message)
      },
      md"""Delete multiple rows with a WHERE clause:""",
      exampleZIO {
        xa.run(for
          _ <- ddl.createTable[LogEntry](ifNotExists = true)
          _ <- dml.insert(LogEntry(-1, "DEBUG", "Debug 1"))
          _ <- dml.insert(LogEntry(-1, "DEBUG", "Debug 2"))
          _ <- dml.insert(LogEntry(-1, "INFO", "Important info"))

          // Delete all DEBUG entries
          rowsDeleted <- dml.deleteWhere[LogEntry](sql"${logs.level} = ${"DEBUG"}")

          // Delete with WHERE and RETURNING (get all deleted rows)
          deletedEntries <- dml.deleteWhereReturning[LogEntry](sql"${logs.level} = ${"ERROR"}")

          remaining <- sql"SELECT * FROM $logs".query[LogEntry]
        yield (rowsDeleted, deletedEntries, remaining)).either
      }.assert {
        case Right((rowsDeleted, deletedEntries, remaining)) =>
          assertTrue(rowsDeleted >= 2 && remaining.exists(_.level == "INFO") && !remaining.exists(_.level == "DEBUG"))
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("Type-Safe Mutation Builders")(
      md"""Saferis provides type-safe builders for INSERT, UPDATE, and DELETE operations. These builders use Scala 3 macros to extract column names at compile time.""",
      section("Insert Builder")(
        md"""Build INSERT statements with type-safe column selectors:""",
        exampleValue {
          // Type-safe INSERT builder
          Insert[BuilderUser]
            .value(_.name, "Alice")
            .value(_.email, "alice@example.com")
            .value(_.age, 30)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("insert") && sql.contains("?"))),
        exampleValue {
          // INSERT with RETURNING clause
          Insert[BuilderUser]
            .value(_.name, "Bob")
            .value(_.email, "bob@example.com")
            .value(_.age, 25)
            .returning
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
      ),
      section("Update Builder (Builder/Ready Pattern)")(
        md"""The Update builder uses a **Builder/Ready pattern** to prevent accidental updates of all rows. You must either:
- Call `.where(...)` to specify which rows to update
- Call `.all` to explicitly update all rows""",
        exampleZIO {
          xa.run(for
            _     <- ddl.createTable[BuilderUser](ifNotExists = true)
            _     <- dml.insert(BuilderUser(-1, "Alice", "alice@example.com", 30))
            _     <- dml.insert(BuilderUser(-1, "Bob", "bob@example.com", 25))
            users <- sql"SELECT * FROM ${Table[BuilderUser]}".query[BuilderUser]
          yield users).either
        }.assert {
          case Right(users) => assertTrue(users.size >= 2)
          case Left(err)    => assertTrue(false).label(err.message)
        },
        exampleValue {
          // Update with type-safe WHERE clause
          Update[BuilderUser]
            .set(_.name, "Alice Updated")
            .set(_.age, 31)
            .where(_.id)
            .eq(1)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("update") && sql.toLowerCase.contains("where"))),
        exampleValue {
          // Chain multiple WHERE conditions
          Update[BuilderUser]
            .set(_.email, "new@example.com")
            .where(_.name)
            .eq("Bob")
            .where(_.age)
            .gt(20)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
        exampleValue {
          // Update with RETURNING clause
          Update[BuilderUser]
            .set(_.age, 35)
            .where(_.id)
            .eq(1)
            .returning
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
        exampleValue {
          // Explicitly update all rows (requires .all)
          Update[BuilderUser]
            .set(_.age, 0)
            .all // Required - prevents accidental "UPDATE ... SET" without WHERE
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("update"))),
      ),
      section("Delete Builder (Builder/Ready Pattern)")(
        md"""Like Update, the Delete builder requires either `.where(...)` or `.all`:""",
        exampleValue {
          // Delete with type-safe WHERE clause
          Delete[BuilderUser]
            .where(_.id)
            .eq(1)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("delete") && sql.toLowerCase.contains("where"))),
        exampleValue {
          // Chain multiple WHERE conditions
          Delete[BuilderUser]
            .where(_.age)
            .lt(18)
            .where(_.name)
            .neq("Admin")
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
        exampleValue {
          // Delete with RETURNING clause
          Delete[BuilderUser]
            .where(_.email)
            .eq("old@example.com")
            .returning
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
        exampleValue {
          // Explicitly delete all rows (requires .all)
          Delete[BuilderUser]
            .all // Required - prevents accidental "DELETE FROM ..."
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("delete"))),
      ),
      section("Available WHERE Operators")(
        md"""All mutation builders support these operators in WHERE clauses:

| Method | SQL | Description |
|--------|-----|-------------|
| `.eq(value)` | `= ?` | Equality |
| `.neq(value)` | `<> ?` | Not equal |
| `.lt(value)` | `< ?` | Less than |
| `.lte(value)` | `<= ?` | Less than or equal |
| `.gt(value)` | `> ?` | Greater than |
| `.gte(value)` | `>= ?` | Greater than or equal |
| `.isNull()` | `is null` | Null check |
| `.isNotNull()` | `is not null` | Non-null check |

You can also use raw `SqlFragment` for complex conditions:""",
        exampleValue {
          // Using SqlFragment for complex WHERE
          val users = Table[BuilderUser]
          Update[BuilderUser]
            .set(_.age, 25)
            .where(sql"${users.name} LIKE ${"A%"}")
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("like") || sql.contains("?"))),
      ),
      section("Complex WHERE with OR and Grouping")(
        md"""Use `andWhere` with a lambda for complex conditions with OR logic:""",
        exampleValue {
          // Query for unclaimed or expired claims
          val now = java.time.Instant.now()
          Update[ClaimTask]
            .set(_.claimedBy, Some("worker-1"))
            .where(_.deadline)
            .lte(now)
            .andWhere(w => w(_.claimedBy).isNull.or(_.claimedUntil).lt(Some(now)))
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("where") && sql.toLowerCase.contains("or"))),
        md"""This generates: `update ... where deadline <= ? and (claimed_by is null or claimed_until < ?)`

The `andWhere` lambda provides a builder that supports:
- `w(_.column)` - Start a condition on a column
- `.isNull` / `.isNotNull` - Null checks
- `.eq(value)` / `.lt(value)` / etc. - Comparisons
- `.or(_.column)` - Chain with OR
- `.and(_.column)` - Chain with AND

Delete also supports `andWhere`:""",
        exampleValue {
          val now2 = java.time.Instant.now()
          Delete[ClaimTask]
            .where(_.deadline)
            .lt(now2)
            .andWhere(w => w(_.claimedBy).isNotNull.or(_.claimedUntil).lt(Some(now2)))
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("delete") && sql.toLowerCase.contains("or"))),
      ),
      section("Type-Safe UPDATE with RETURNING")(
        md"""Return updated rows atomically using `returningAs`:""",
        exampleValue {
          // returningAs provides type-safe query execution
          val newExpiry = java.time.Instant.now().plusSeconds(60)
          Update[LockRow]
            .set(_.expiresAt, newExpiry)
            .where(_.instanceId)
            .eq("instance-1")
            .where(_.nodeId)
            .eq("node-1")
            .returningAs
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
        md"""The `returningAs` method:
- Returns `ReturningQuery[A]` with type-safe `query` and `queryOne` methods
- Only compiles when the dialect supports RETURNING (PostgreSQL, SQLite)
- Uses capability constraint: requires `Dialect & ReturningSupport`""",
        exampleValue {
          // Execute and get the updated row
          val result: ScopedQuery[Option[LockRow]] = Update[LockRow]
            .set(_.expiresAt, java.time.Instant.now())
            .where(_.instanceId)
            .eq("id")
            .returningAs
            .queryOne // Returns ScopedQuery[Option[LockRow]]
          result.getClass.getSimpleName
        }.assert(name => assertTrue(name.nonEmpty)),
        md"""Delete also supports `returningAs`:""",
        exampleValue {
          Delete[LockRow]
            .where(_.instanceId)
            .eq("instance-1")
            .returningAs
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
      ),
    ),
  )
end Dml
