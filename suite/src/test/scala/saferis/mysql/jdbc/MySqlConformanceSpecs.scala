package saferis.mysql.jdbc

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.tests.DatabaseTarget
import saferis.tests.MySqlTestContainer
import saferis.tests.SchemaConformance
import saferis.tests.SqlSessionConformance
import saferis.tests.TransactionConformance

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
    ZLayer.succeed(DatabaseTarget(MySQLDialect))

  /** MySQL runs the portable suite, its catalog, its own values, and a statement it can cancel. */
  def spec =
    suite("mysql")(
      SqlSessionConformance.suite,
      SchemaConformance.conformance,
      TransactionConformance.cancelsLongStatement(sql"select sleep(5)"),
      MySqlValueSpecs.spec,
      TransactionConformance.continuesAfterUniqueViolation,
    ).provideShared(MySqlTestContainer.live >>> session, target)
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)
end MySqlConformanceSpecs
