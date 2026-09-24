package saferis.tests

import org.testcontainers.containers.PostgreSQLContainer
import zio.*

final case class ContainerConfig(
    initScriptPath: String = "init.sql",
    imageName: String = s"${PostgreSQLContainer.IMAGE}:latest",
)
object ContainerConfig:
  val default: ULayer[ContainerConfig] = ZLayer.succeed(ContainerConfig())

private[tests] object PostgresTestContainerPlatform:
  /** Started Postgres. The scope stops the container. */
  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    ContainerConfig.default >>> started

  private val started: ZLayer[ContainerConfig, Throwable, PostgresTestContainer] =
    ZLayer.scoped:
      ZIO.serviceWithZIO[ContainerConfig]: config =>
        ZIO.acquireRelease(ZIO.attempt(open(config)))(container => ZIO.succeed(container.stop())).map(expose)

  private def open(config: ContainerConfig): PostgreSQLContainer[?] =
    val container: PostgreSQLContainer[?] =
      new PostgreSQLContainer(config.imageName).withEnv("POSTGRES_HOST_AUTH_METHOD", "trust")
    container.withInitScript(config.initScriptPath)
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
