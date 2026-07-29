package saferis.docs

import org.postgresql.ds.PGSimpleDataSource
import org.testcontainers.containers.PostgreSQLContainer
import saferis.*
import zio.*

/** Shared Postgres + [[Transactor]] for Specular DocSpecs (one container per JVM). */
object DocsTransactor:

  private lazy val container: PostgreSQLContainer[?] =
    val thread   = Thread.currentThread()
    val previous = thread.getContextClassLoader()
    thread.setContextClassLoader(getClass.getClassLoader())
    try
      val c = new PostgreSQLContainer("postgres:16")
      c.withEnv("POSTGRES_HOST_AUTH_METHOD", "trust")
      c.start()
      c
    finally thread.setContextClassLoader(previous)

  lazy val transactor: Transactor =
    val dataSource = PGSimpleDataSource()
    dataSource.setURL(container.getJdbcUrl())
    dataSource.setUser(container.getUsername())
    dataSource.setPassword(container.getPassword())
    Transactor(ConnectionProvider.FromDataSource(dataSource), _ => (), None)

  val xa: Transactor = transactor

  val transactorLayer: ZLayer[Any, Nothing, Transactor] =
    ZLayer.succeed(transactor)
end DocsTransactor
