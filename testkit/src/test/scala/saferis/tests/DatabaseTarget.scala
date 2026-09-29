package saferis.tests

import saferis.Dialect
import saferis.postgres.PostgresDialect

import zio.ULayer
import zio.ZLayer

/** The dialect the conformance suite renders with. A driver provides this next to its `SqlSession`. */
final case class DatabaseTarget(dialect: Dialect)

object DatabaseTarget:
  val postgres: ULayer[DatabaseTarget] =
    ZLayer.succeed(DatabaseTarget(PostgresDialect))
