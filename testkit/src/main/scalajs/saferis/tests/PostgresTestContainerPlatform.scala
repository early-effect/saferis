package saferis.tests

import zio.*

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

private[tests] object PostgresTestContainerPlatform:
  /** Started Postgres. `@testcontainers/postgresql` is `require`d from Node. The scope stops it. */
  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    ZLayer.scoped:
      ZIO.uninterruptible:
        for
          started <- await(open().start())
          _       <- ZIO.addFinalizer(stop(started))
        yield NodePostgres(
          host = started.getHost(),
          port = started.getPort().toInt,
          database = started.getDatabase(),
          user = started.getUsername(),
          password = started.getPassword(),
        )

  private def open(): PostgreSqlContainer =
    new PostgreSqlContainer(PostgresTestContainer.Image)
      .withEnvironment(js.Dictionary("POSTGRES_HOST_AUTH_METHOD" -> "trust"))
      .withCopyContentToContainer(
        js.Array(
          js.Dynamic.literal(
            content = InitScript.sql,
            target = PostgresTestContainer.InitScript,
          )
        )
      )

  /** The suite's clock is the live clock. This finalizer does not replace it. */
  private def stop(started: StartedPostgreSqlContainer): UIO[Unit] =
    await(started.stop()).orDie
      .timeout(20.seconds)
      .flatMap:
        case Some(_) => ZIO.unit
        case None    => ZIO.dieMessage("Postgres container stop timed out")

  private def await[A](p: js.Promise[A]): Task[A] =
    ZIO.asyncInterrupt[Any, Throwable, A]: register =>
      var settled = false
      p.`then`[Unit](
        (value: A) =>
          if !settled then
            settled = true
            register(ZIO.succeed(value))
          else (),
        (err: scala.Any) =>
          if !settled then
            settled = true
            val message = if err == null then "container call failed" else err.toString
            register(ZIO.fail(new RuntimeException(message)))
          else (),
      )
      Left(ZIO.succeed { settled = true })

  private final class NodePostgres(
      val host: String,
      val port: Int,
      val database: String,
      val user: String,
      val password: String,
  ) extends PostgresTestContainer

  @js.native
  @JSImport("@testcontainers/postgresql", "PostgreSqlContainer")
  private class PostgreSqlContainer(@unused image: String) extends js.Object:
    def withEnvironment(environment: js.Dictionary[String]): PostgreSqlContainer       = js.native
    def withCopyContentToContainer(contents: js.Array[js.Object]): PostgreSqlContainer = js.native
    def start(): js.Promise[StartedPostgreSqlContainer]                                = js.native

  @js.native
  private trait StartedPostgreSqlContainer extends js.Object:
    def getHost(): String        = js.native
    def getPort(): Double        = js.native
    def getDatabase(): String    = js.native
    def getUsername(): String    = js.native
    def getPassword(): String    = js.native
    def stop(): js.Promise[Unit] = js.native
end PostgresTestContainerPlatform
