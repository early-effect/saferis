package saferis.tests

import saferis.PgPromises
import zio.*

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

private[tests] object PostgresTestContainerPlatform:
  /** Same tag as jdbc `ContainerConfig`: `postgres:latest`. */
  private val Image = "postgres:latest"

  /** Started Postgres. `@testcontainers/postgresql` is `require`d from Node, same as `pg`. The scope stops it. */
  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    ZLayer.scoped:
      ZIO.uninterruptible:
        for
          started <- PgPromises.complete(None, new PostgreSqlContainer(Image).start(), _ => ())
          _       <- ZIO.addFinalizer(stop(started))
        yield NodePostgres(
          host = started.getHost(),
          port = started.getPort().toInt,
          database = started.getDatabase(),
          user = started.getUsername(),
          password = started.getPassword(),
        )

  private def stop(started: StartedPostgreSqlContainer): UIO[Unit] =
    PgPromises
      .complete(None, started.stop(), _ => ())
      .timeout(20.seconds)
      .provideLayer(ZLayer.succeed(Clock.ClockLive))
      .flatMap:
        case Some(_) => ZIO.unit
        case None    => ZIO.dieMessage("Postgres container stop timed out")
      .orDie

  private final class NodePostgres(
      val host: String,
      val port: Int,
      val database: String,
      val user: String,
      val password: String,
  ) extends PostgresTestContainer

  /** CommonJS `require("@testcontainers/postgresql").PostgreSqlContainer`. */
  @js.native
  @JSImport("@testcontainers/postgresql", "PostgreSqlContainer")
  private class PostgreSqlContainer(@unused image: String) extends js.Object:
    def start(): js.Promise[StartedPostgreSqlContainer] = js.native

  @js.native
  private trait StartedPostgreSqlContainer extends js.Object:
    def getHost(): String        = js.native
    def getPort(): Double        = js.native
    def getDatabase(): String    = js.native
    def getUsername(): String    = js.native
    def getPassword(): String    = js.native
    def stop(): js.Promise[Unit] = js.native
end PostgresTestContainerPlatform
