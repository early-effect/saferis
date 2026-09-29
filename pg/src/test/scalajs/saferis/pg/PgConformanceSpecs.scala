package saferis.pg

import saferis.SaferisError
import saferis.SqlSession
import saferis.postgres.DatabaseName
import saferis.postgres.Host
import saferis.postgres.PgConnectionConfig
import saferis.postgres.UserName
import saferis.postgres.PostgresConformance
import saferis.sql
import saferis.tests.DatabaseTarget
import saferis.tests.PostgresTestContainer
import saferis.tests.SchemaConformance
import saferis.tests.SqlSessionConformance
import saferis.tests.TransactionConformance

import zio.*
import zio.test.*

object PgConformanceSpecs extends ZIOSpecDefault:
  private val sessions: ZLayer[PostgresTestContainer, SaferisError, SqlSession] =
    ZLayer.fromZIO(
      ZIO.serviceWith[PostgresTestContainer]: pg =>
        PgConfig(
          connection = PgConnectionConfig(
            host = Host(pg.host),
            port = pg.port,
            database = DatabaseName(pg.database),
            user = UserName(pg.user),
            password = zio.Config.Secret(pg.password),
          )
        )
    ) >>> NodeSession.layer

  /** Postgres over Node. The shared suite, the catalog suite, and the Postgres suite. */
  def spec =
    suite("postgres node")(
      SqlSessionConformance.suite,
      SchemaConformance.conformance,
      PostgresConformance.suite,
      TransactionConformance.cancelsLongStatement(sql"select pg_sleep(5)"),
    ).provideShared(PostgresTestContainer.live >>> sessions, DatabaseTarget.postgres)
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)
end PgConformanceSpecs
