package saferis.docs

import org.postgresql.ds.PGSimpleDataSource
import org.testcontainers.containers.PostgreSQLContainer
import saferis.JdbcSession
import saferis.SqlSession
import zio.*

import javax.sql.DataSource

/** Shared Postgres `SqlSession` for Specular DocSpecs (one container per JVM). */
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
  end container

  lazy val dataSource: DataSource =
    val ds = PGSimpleDataSource()
    ds.setURL(container.getJdbcUrl())
    ds.setUser(container.getUsername())
    ds.setPassword(container.getPassword())
    ds

  val layer: ULayer[SqlSession] =
    ZLayer.succeed(dataSource) >>> JdbcSession.layer()
end DocsTransactor
