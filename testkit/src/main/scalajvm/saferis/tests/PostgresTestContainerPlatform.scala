package saferis.tests

import org.testcontainers.containers.PostgreSQLContainer
import zio.*

private[tests] object PostgresTestContainerPlatform:
  /** Started Postgres. The scope stops the container. Trust auth, same init script as Node. */
  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    ZLayer.scoped:
      ZIO.acquireRelease(ZIO.attempt(open()))(container => ZIO.succeed(container.stop())).map(expose)

  private def open(): PostgreSQLContainer[?] =
    val container: PostgreSQLContainer[?] =
      new PostgreSQLContainer(PostgresTestContainer.Image).withEnv("POSTGRES_HOST_AUTH_METHOD", "trust")
    container.withInitScript("init.sql")
    container.start()
    container

  private def expose(container: PostgreSQLContainer[?]): PostgresTestContainer =
    JvmPostgres(
      host = container.getHost(),
      port = container.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT),
      database = container.getDatabaseName(),
      user = container.getUsername(),
      password = container.getPassword(),
    )

  private final class JvmPostgres(
      val host: String,
      val port: Int,
      val database: String,
      val user: String,
      val password: String,
  ) extends PostgresTestContainer
end PostgresTestContainerPlatform
