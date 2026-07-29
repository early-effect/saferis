package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object StatementTimeouts extends SaferisDocSpecSuite:

  @tableName("statement_timeouts_users")
  case class User(@generated @key id: Int, name: String) derives Table

  def doc = page("Statement Timeouts")(
    md"""Long-running queries can hold a database connection for minutes and starve a pool. Saferis lets you cap the time the database spends on a statement using JDBC's `Statement.setQueryTimeout`, which, unlike `ZIO.timeout`, asks the driver to **actually cancel the query server-side**.

There are two composable layers for setting a timeout:

1. **`Saferis.queryTimeout(d)` aspect**: scopes a timeout to any Saferis fragments executed inside the decorated effect. Composes with other ZIO aspects.
2. **Transactor `defaultTimeout`**: applies to every statement run through that Transactor.

When both apply, the aspect wins (per-scope ad-hoc overrides beat the Transactor-wide default).""",
    section("The `Saferis.queryTimeout` aspect")(
      exampleValue {
        def slowReport(xa: Transactor) =
          xa.run(sql"SELECT * FROM ${Table[User]}".query[User]) @@ Saferis.queryTimeout(5.seconds)
        slowReport
      }.assert(_ => assertTrue(true)),
      md"""Because `queryTimeout` is a regular `ZIOAspect`, it composes with the rest of ZIO's aspect ecosystem (`@@ ZIOAspect.loggedWith(...)`, etc.) and stacks naturally with `>>=`/`flatMap`.""",
    ),
    section("Transactor-wide default")(
      exampleValue {
        // Every statement run through this Transactor is bounded by 30 seconds, unless overridden by the aspect.
        val xaLayer = Transactor.layer(defaultTimeout = Some(30.seconds))
        xaLayer
      }.assert(_ => assertTrue(true))
    ),
    section("Resolution order")(
      md"""When a statement is about to execute, Saferis picks the timeout in this priority order:

1. The value installed by `Saferis.queryTimeout(d)` for the current fiber, if any.
2. The Transactor's `defaultTimeout`, if set.
3. No timeout (current default behavior)."""
    ),
    section("Granularity")(
      md"""JDBC's `setQueryTimeout` accepts whole seconds. Saferis accepts a `zio.Duration` and rounds **up** to the nearest second, with a minimum of 1 second. This is intentional: `setQueryTimeout(0)` means *no limit* in JDBC, so a sub-second duration must not silently disable the cap."""
    ),
    section("Handling timeout errors")(
      md"""A timed-out statement surfaces as `SaferisError.Timeout`. This applies to both client-side timeouts triggered by `setQueryTimeout` and server-side cancellations (SQLState `57014`):""",
      exampleValue {
        def safeReport(xa: Transactor) =
          xa.run(sql"SELECT * FROM ${Table[User]}".query[User])
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
        (xa.run(sql"SELECT pg_sleep(3)".queryValue[Int]) @@ Saferis.queryTimeout(
          1.second
        )).unit: ZIO[Scope, SaferisError, Unit]
      }.assert(c => assertTrue(c.failures.nonEmpty)),
    ),
  )
end StatementTimeouts
