package saferis.tests

import org.testcontainers.containers.MySQLContainer
import zio.*

/** One MySQL a suite started. JVM only: there is no Node MySQL driver. */
trait MySqlTestContainer:
  def host: String
  def port: Int
  def database: String
  def user: String
  def password: String

object MySqlTestContainer:
  /** Pinned so a MySQL release cannot change results overnight. CI pre-pulls this tag. */
  val Image = "mysql:8.4"

  /** Started MySQL. The scope stops the container. */
  val live: ZLayer[Any, Throwable, MySqlTestContainer] =
    ZLayer.scoped:
      ZIO.acquireRelease(ZIO.attemptBlocking(open()))(container => ZIO.succeed(container.stop())).map(expose)

  private def open(): MySQLContainer[?] =
    val container: MySQLContainer[?] = new MySQLContainer(Image)
    container.start()
    container

  private def expose(container: MySQLContainer[?]): MySqlTestContainer =
    Started(
      host = container.getHost(),
      port = container.getMappedPort(MySQLContainer.MYSQL_PORT),
      database = container.getDatabaseName(),
      user = container.getUsername(),
      password = container.getPassword(),
    )

  private final class Started(
      val host: String,
      val port: Int,
      val database: String,
      val user: String,
      val password: String,
  ) extends MySqlTestContainer
end MySqlTestContainer
