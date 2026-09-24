package saferis.tests

import saferis.PgConfig
import saferis.PgPromises
import saferis.SqlListener
import zio.*

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Connection coordinates for one Postgres this suite started.
  *
  * `@testcontainers/postgresql` is `require`d from Node, same as `pg`. The scope stops the container.
  */
final case class NodePostgres(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
):
  def config(
      defaultTimeout: Option[Duration] = None,
      listener: SqlListener = SqlListener.noop,
      poolSize: Int = 4,
  ): PgConfig =
    PgConfig(
      host = host,
      port = port,
      database = database,
      user = user,
      password = password,
      poolSize = poolSize,
      defaultTimeout = defaultTimeout,
      listener = listener,
    )
end NodePostgres

object NodePostgres:
  /** Same tag as jdbc `ContainerConfig`: `postgres:latest`. */
  val Image = "postgres:latest"

  val layer: ZLayer[Any, Throwable, NodePostgres] =
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
end NodePostgres
