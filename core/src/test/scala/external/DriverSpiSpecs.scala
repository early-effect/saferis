package external

import saferis.*

import zio.*
import zio.stream.ZStream
import zio.test.*

/** Compiles outside `package saferis`. A third-party driver uses this surface and nothing `private[saferis]`. */
object DriverSpiSpecs extends ZIOSpecDefault:
  def spec = suite("driver SPI from outside package saferis")(
    test("SqlSession.pooled runs a statement on a connection this package owns"):
      val session = SqlSession.pooled(ZIO.succeed(ProbeConnection))
      for n <- sql"select 1".execute.provide(ZLayer.succeed(session))
      yield assertTrue(n == 1L)
  )

private object ProbeConnection extends SqlConnection:
  def execute(command: SqlCommand): IO[SaferisError, Long] =
    ZIO.succeed(1L)

  def query(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
    ZIO.succeed(Chunk.empty)

  def queryAtMostOne(command: SqlCommand): IO[SaferisError, Option[SqlRow]] =
    ZIO.succeed(None)

  def cursor(command: SqlCommand): ZStream[Any, SaferisError, SqlRow] =
    ZStream.empty

  def begin: IO[SaferisError, Unit] =
    ZIO.unit

  def commit: IO[SaferisError, Unit] =
    ZIO.unit

  def rollback: UIO[Unit] =
    ZIO.unit
end ProbeConnection
