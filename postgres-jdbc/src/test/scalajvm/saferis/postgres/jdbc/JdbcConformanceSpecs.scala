package saferis.postgres.jdbc

import saferis.postgres.PostgresConformance
import saferis.sql
import saferis.tests.DatabaseTarget
import saferis.tests.PostgresTestContainer
import saferis.tests.SchemaConformance
import saferis.tests.SqlSessionConformance
import saferis.tests.TransactionConformance

import zio.durationInt
import zio.test.*

/** Postgres over JDBC. The shared suite, the catalog suite, and the Postgres suite. */
object JdbcConformanceSpecs extends ZIOSpecDefault:
  def spec =
    suite("postgres jdbc")(
      SqlSessionConformance.suite,
      SchemaConformance.conformance,
      PostgresConformance.suite,
      TransactionConformance.cancelsLongStatement(sql"select pg_sleep(5)"),
    ).provideShared(PostgresTestContainer.live >>> DataSourceProvider.session, DatabaseTarget.postgres)
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)
end JdbcConformanceSpecs
