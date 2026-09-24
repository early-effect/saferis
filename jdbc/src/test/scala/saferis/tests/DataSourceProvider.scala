package saferis.tests

import org.postgresql.ds.PGSimpleDataSource
import saferis.JdbcSession
import saferis.JdbcSessionConfig
import saferis.SqlSession
import zio.*

import javax.sql.DataSource

object DataSourceProvider:
  val datasource: URLayer[PostgresTestContainer, DataSource] =
    ZLayer.fromZIO(ZIO.serviceWith[PostgresTestContainer](dataSource))
  val session: ZLayer[PostgresTestContainer, Nothing, SqlSession] =
    datasource >>> JdbcSession.layer()
  val default: ZLayer[Any, Throwable, SqlSession] =
    PostgresTestContainer.live >>> session
  def configured(config: JdbcSessionConfig): ZLayer[Any, Throwable, SqlSession] =
    PostgresTestContainer.live >>> datasource >>> JdbcSession.layer(config)

  private def dataSource(pg: PostgresTestContainer): DataSource =
    val ds = PGSimpleDataSource()
    ds.setURL(s"jdbc:postgresql://${pg.host}:${pg.port}/${pg.database}")
    ds.setUser(pg.user)
    ds.setPassword(pg.password)
    ds
end DataSourceProvider
