package saferis.tests

import zio.test.*

/** Postgres over JDBC proves itself by providing its session and target to the common suite. */
object JdbcConformanceSpecs extends ZIOSpecDefault:
  def spec =
    suite("postgres jdbc")(SqlSessionConformance.suite)
      .provideShared(PostgresTestContainer.live >>> DataSourceProvider.session, DatabaseTarget.postgres)
