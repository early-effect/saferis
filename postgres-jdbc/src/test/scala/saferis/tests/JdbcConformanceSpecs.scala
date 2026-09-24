package saferis.tests

import zio.test.*

object JdbcConformanceSpecs extends ZIOSpecDefault:
  def spec =
    SqlSessionConformance
      .suite("jdbc", DataSourceProvider.session)
      .provideShared(PostgresTestContainer.live)
