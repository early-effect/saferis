package saferis.docs

import saferis.*
import saferis.jdbc.*
import saferis.postgres.jdbc.PostgresJdbc
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object StatementTimeouts extends SaferisDocSpecSuite:

  @tableName("statement_timeouts_users")
  case class User(@generated @key id: Int, name: String) derives Table

  def doc = page("Statement Timeouts")(
    md"""Long-running queries can hold a database connection for minutes and starve a pool. Saferis caps the time the database spends on a statement. The JDBC driver calls `Statement.setQueryTimeout`, which, unlike `ZIO.timeout`, asks the driver to cancel the query server-side.

There are three places to set a timeout, highest priority first:

1. **`sql"...".withTimeout(d)`**: this statement only.
2. **`Saferis.queryTimeout(d)` aspect**: any Saferis fragment executed on the current fiber. It composes with other ZIO aspects.
3. **`JdbcSessionConfig.defaultTimeout`**: every statement on that session when neither of the above is set.

A set fragment timeout wins over the aspect. The aspect wins over the session default.""",
    section("The `Saferis.queryTimeout` aspect")(
      exampleValue {
        def slowReport(session: SqlSession) =
          (sql"SELECT * FROM ${Table[User]}".query[User]) @@ Saferis.queryTimeout(5.seconds)
        slowReport
      }.assert(_ => assertTrue(true)),
      md"""Because `queryTimeout` is a regular `ZIOAspect`, it composes with the rest of ZIO's aspect ecosystem (`@@ ZIOAspect.loggedWith(...)`, etc.) and stacks naturally with `>>=`/`flatMap`.""",
    ),
    section("Session-wide default")(
      exampleValue {
        // Every statement on this session is bounded by 30 seconds, unless a fragment or the aspect sets one.
        val session = PostgresJdbc.layer(JdbcSessionConfig(defaultTimeout = Some(30.seconds)))
        session
      }.assert(_ => assertTrue(true))
    ),
    section("Resolution order")(
      md"""When a statement is about to execute, Saferis picks the timeout in this priority order:

1. `withTimeout` on the fragment, if set.
2. The value installed by `Saferis.queryTimeout(d)` for the current fiber, if any.
3. `JdbcSessionConfig.defaultTimeout`, if set.
4. No timeout."""
    ),
    section("Granularity")(
      md"""JDBC's `setQueryTimeout` accepts whole seconds. Saferis accepts a `zio.Duration` and rounds **up** to the nearest second, with a minimum of 1 second. This is intentional: `setQueryTimeout(0)` means *no limit* in JDBC, so a sub-second duration must not silently disable the cap."""
    ),
    section("Handling timeout errors")(
      md"""A timed-out statement surfaces as `SaferisError.Timeout`. This applies to both client-side timeouts triggered by `setQueryTimeout` and server-side cancellations (SQLState `57014`):""",
      exampleValue {
        def safeReport(session: SqlSession) =
          (sql"SELECT * FROM ${Table[User]}"
            .query[User])
            .catchSome:
              case _: SaferisError.Timeout =>
                ZIO.logWarning("Report query timed out, returning empty result").as(Chunk.empty)
          @@ Saferis.queryTimeout(5.seconds)
        safeReport
      }.assert(_ => assertTrue(true)),
    ),
    section("A timeout firing for real")(
      md"""Here is a timeout actually firing against the live database: a `SELECT pg_sleep(3)`
bounded by a 1-second timeout. The effect fails, and the real `SaferisError.Timeout`
is shown beneath the snippet:""",
      expectCrash {
        // pg_sleep(3) would take 3 seconds, but we cap the statement at 1 second.
        ((sql"SELECT pg_sleep(3)".queryValue[Int]) @@ Saferis.queryTimeout(
          1.second
        )).unit
          .provideLayer(DocsTransactor.layer)
      }.assert(c => assertTrue(c.failures.nonEmpty)),
    ),
  )
end StatementTimeouts
