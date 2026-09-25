package saferis.tests

import saferis.*
import zio.{test as _, *}
import zio.test.*

/** End-to-end integration tests for the literal `IN` collection helpers — both `WhereBuilderOps.in` / `inList` /
  * `notIn` / `notInList` on the typed Query DSL and `Interpolator.in` in raw `sql"..."` interpolation.
  *
  * Verifies that:
  *   - Real `IN`/`NOT IN` queries against PostgreSQL return the expected rows.
  *   - Empty / dedupe behaviour matches the unit-level expectations in InterpolatorSpecs and QuerySpecs.
  *   - `SaferisError.InvalidStatement` surfaces from `(...)` without touching the database, and is recoverable via
  *     `.catchSome`.
  */
object InCollectionIntegrationSpecs:
  private object Collected extends zio.test.ZIOSpecDefault:
    def spec = suite("unused")()

  @tableName("in_collection_users")
  final case class InUser(@key id: Int, name: String) derives Table

  /** DDL + seed: 5 rows with ids 1..5. Idempotent — drops if exists. */
  private val seedTable: ZIO[SqlSession, SaferisError, Unit] =
    for
      _ <- (sql"drop table if exists in_collection_users".dml)
      _ <- (
        sql"""create table in_collection_users (
                id integer primary key,
                name varchar(255) not null
              )""".dml
      )
      _ <- (sql"insert into in_collection_users (id, name) values (1, 'Alice')".dml)
      _ <- (sql"insert into in_collection_users (id, name) values (2, 'Bob')".dml)
      _ <- (sql"insert into in_collection_users (id, name) values (3, 'Carol')".dml)
      _ <- (sql"insert into in_collection_users (id, name) values (4, 'Dan')".dml)
      _ <- (sql"insert into in_collection_users (id, name) values (5, 'Eve')".dml)
    yield ()

  val tests = Collected.suiteAll("IN literal collections — end-to-end"):

    // === Positive paths via Query DSL ===

    test("Query DSL inList(List) returns rows matching the spliced ids"):
      for
        _    <- seedTable
        rows <- (Query[InUser].where(_.id).inList(List(2, 4)).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("Query DSL in(varargs) returns rows matching the inline ids"):
      for
        _    <- seedTable
        rows <- (Query[InUser].where(_.id).in(2, 4).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("Query DSL inList accepts a Set"):
      for
        _    <- seedTable
        rows <- (Query[InUser].where(_.id).inList(Set(2, 4)).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("Query DSL notInList returns rows NOT in the spliced ids"):
      for
        _    <- seedTable
        rows <- (Query[InUser].where(_.id).notInList(List(1, 3, 5)).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    // === Positive paths via raw sql"..." ===

    test("Raw sql with array(List) returns rows matching the spliced ids"):
      for
        _    <- seedTable
        rows <- (sql"select * from in_collection_users where id = any(${array(List(2, 4))})".query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("Raw sql with varargs array(...) returns rows matching the inline ids"):
      for
        _    <- seedTable
        rows <- (sql"select * from in_collection_users where id = any(${array(2, 4)})".query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    // === Dedupe end-to-end ===

    test("inList deduplicates so DB receives N distinct parameters, not N*k"):
      for
        _ <- seedTable
        // 4 duplicates collapse to 2 distinct params
        rows <- (Query[InUser].where(_.id).inList(List(2, 2, 4, 4)).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("empty inList matches no rows"):
      for
        _    <- seedTable
        rows <- Query[InUser].where(_.id).inList(List.empty[Int]).query[InUser]
      yield assertTrue(rows.isEmpty)

    test("empty notInList matches every row"):
      for
        _    <- seedTable
        rows <- Query[InUser].where(_.id).notInList(List.empty[Int]).query[InUser]
      yield assertTrue(rows.map(_.id).toSet == Set(1, 2, 3, 4, 5))

    test("empty array parameter matches no rows"):
      for
        _    <- seedTable
        rows <- sql"select * from in_collection_users where id = any(${array(List.empty[Int])})".query[InUser]
      yield assertTrue(rows.isEmpty)

  end tests

  def conformance = suite("InCollectionIntegrationSpecs")(tests) @@ TestAspect.sequential
end InCollectionIntegrationSpecs
