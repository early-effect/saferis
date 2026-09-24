package saferis.pg

import saferis.SaferisError
import saferis.SqlSession
import saferis.postgres.PgConnectionConfig
import saferis.tests.PostgresTestContainer
import saferis.tests.SqlSessionConformance

import zio.*
import zio.test.*

object PgConformanceSpecs extends ZIOSpecDefault:
  private val sessions: ZLayer[PostgresTestContainer, SaferisError, SqlSession] =
    ZLayer.fromZIO(
      ZIO.serviceWith[PostgresTestContainer]: pg =>
        PgConfig(
          connection = PgConnectionConfig(
            host = pg.host,
            port = pg.port,
            database = pg.database,
            user = pg.user,
            password = zio.Config.Secret(pg.password),
          )
        )
    ) >>> NodeSession.layer

  def spec =
    SqlSessionConformance.postgres("node", sessions).provideShared(PostgresTestContainer.live)
end PgConformanceSpecs
