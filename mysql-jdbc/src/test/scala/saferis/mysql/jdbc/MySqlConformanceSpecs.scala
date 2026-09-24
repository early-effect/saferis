package saferis.mysql.jdbc

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.tests.Capability
import saferis.tests.DatabaseTarget
import saferis.tests.MySqlTestContainer
import saferis.tests.SqlSessionConformance

import com.mysql.cj.jdbc.MysqlDataSource
import zio.*
import zio.test.*

import javax.sql.DataSource

object MySqlConformanceSpecs extends ZIOSpecDefault:
  given Dialect = MySQLDialect

  /** The container has no TLS, so the client may fetch the server's public key for `caching_sha2_password`. */
  val dataSource: URLayer[MySqlTestContainer, DataSource] =
    ZLayer.fromFunction: (mysql: MySqlTestContainer) =>
      val ds = MysqlDataSource()
      ds.setUrl(s"jdbc:mysql://${mysql.host}:${mysql.port}/${mysql.database}")
      ds.setUser(mysql.user)
      ds.setPassword(mysql.password)
      ds.setUseSSL(false)
      ds.setAllowPublicKeyRetrieval(true)
      ds

  val session: ZLayer[MySqlTestContainer, Nothing, SqlSession] =
    dataSource >>> MySqlJdbc.layer()

  val target: ULayer[DatabaseTarget] =
    ZLayer.succeed(DatabaseTarget(MySQLDialect, Set(Capability.Catalog), Some(sql"select sleep(5)")))

  /** MySQL proves itself by providing its session and target to the common suite, then runs what only MySQL does. */
  def spec =
    suite("mysql")(
      SqlSessionConformance.suite,
      MySqlValueSpecs.spec,
    ).provideShared(MySqlTestContainer.live >>> session, target) @@ TestAspect.sequential
end MySqlConformanceSpecs
