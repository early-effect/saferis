package saferis.tests

import org.postgresql.ds.PGSimpleDataSource
import saferis.SqlSession
import saferis.jdbc.JdbcSessionConfig
import saferis.postgres.jdbc.PostgresJdbc
import zio.*

import javax.sql.DataSource

object DataSourceProvider:
  val datasource: URLayer[PostgresTestContainer, DataSource] =
    ZLayer.fromZIO(ZIO.serviceWith[PostgresTestContainer](dataSource))
  val session: ZLayer[PostgresTestContainer, Nothing, SqlSession] =
    datasource >>> PostgresJdbc.layer()
  val default: ZLayer[Any, Throwable, SqlSession] =
    PostgresTestContainer.live >>> session
  def configured(config: JdbcSessionConfig): ZLayer[Any, Throwable, SqlSession] =
    PostgresTestContainer.live >>> datasource >>> PostgresJdbc.layer(config)

  private def dataSource(pg: PostgresTestContainer): DataSource =
    val ds = PGSimpleDataSource()
    ds.setURL(s"jdbc:postgresql://${pg.host}:${pg.port}/${pg.database}")
    ds.setUser(pg.user)
    ds.setPassword(pg.password)
    ds
end DataSourceProvider
