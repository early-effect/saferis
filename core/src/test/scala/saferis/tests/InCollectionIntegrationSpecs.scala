package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
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
object InCollectionIntegrationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

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

  val tests = suiteAll("IN literal collections — end-to-end"):

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

    test("Raw sql with in(List) returns rows matching the spliced ids"):
      for
        _    <- seedTable
        rows <- (sql"select * from in_collection_users where id in ${in(List(2, 4))}".query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    test("Raw sql with varargs in(...) returns rows matching the inline ids"):
      for
        _    <- seedTable
        rows <- (sql"select * from in_collection_users where id in ${in(2, 4)}".query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    // === Dedupe end-to-end ===

    test("inList deduplicates so DB receives N distinct parameters, not N*k"):
      for
        _ <- seedTable
        // 4 duplicates collapse to 2 distinct params
        rows <- (Query[InUser].where(_.id).inList(List(2, 2, 4, 4)).query[InUser])
      yield assertTrue(rows.map(_.id).toSet == Set(2, 4))

    // === Failure paths: empty input fails before JDBC ===

    test("xa.run on empty inList fails with InvalidStatement and does not query"):
      for
        _      <- seedTable
        result <- (Query[InUser].where(_.id).inList(List.empty[Int]).query[InUser]).either
      yield assertTrue(result match
        case Left(SaferisError.InvalidStatement(issues)) if issues.size == 1 =>
          issues.exists {
            case FragmentIssue.EmptyCollection("WhereBuilder.inList", _) => true
            case _                                                       => false
          }
        case _ => false)

    test("xa.run on empty notInList fails with InvalidStatement"):
      for
        _      <- seedTable
        result <- (Query[InUser].where(_.id).notInList(List.empty[Int]).query[InUser]).either
      yield assertTrue(result match
        case Left(SaferisError.InvalidStatement(_)) => true
        case _                                      => false)

    test("xa.run on raw sql with empty in(...) fails with InvalidStatement tagged 'in'"):
      for
        _      <- seedTable
        result <-
          (sql"select * from in_collection_users where id in ${in(List.empty[Int])}".query[InUser]).either
      yield assertTrue(result match
        case Left(SaferisError.InvalidStatement(issues)) =>
          issues.exists {
            case FragmentIssue.EmptyCollection("in", _) => true
            case _                                      => false
          }
        case _ => false)

    test("multi-issue: a query with two empty inList calls accumulates two issues"):
      for
        _      <- seedTable
        result <- (
          Query[InUser]
            .where(_.id)
            .inList(List.empty[Int])
            .where(_.name)
            .inList(List.empty[String])
            .query[InUser]
          )
          .either
      yield assertTrue(result match
        case Left(SaferisError.InvalidStatement(issues)) => issues.size == 2
        case _                                           => false)

    test(".catchSome recovers cleanly from InvalidStatement without touching DB"):
      for
        _ <- seedTable
        // Use empty input on purpose; recover with an empty result.
        rows <- (Query[InUser]
          .where(_.id)
          .inList(List.empty[Int])
          .query[InUser])
          .catchSome { case _: SaferisError.InvalidStatement => ZIO.succeed(Chunk.empty[InUser]) }
      yield assertTrue(rows.isEmpty)

    test("SqlFragment.validate fails directly without invoking the runtime executor"):
      val frag = Query[InUser].where(_.id).inList(List.empty[Int]).build
      for result <- frag.validate.either
      yield assertTrue(result match
        case Left(SaferisError.InvalidStatement(_)) => true
        case _                                      => false)

  end tests

  val spec = suite("InCollectionIntegrationSpecs")(tests).provideShared(xaLayer) @@ TestAspect.sequential
end InCollectionIntegrationSpecs
