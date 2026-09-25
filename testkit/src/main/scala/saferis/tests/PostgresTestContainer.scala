package saferis.tests

import zio.ZLayer

/** One Postgres a suite started. `live` is the platform: JVM, Node, and later Native. */
trait PostgresTestContainer:
  def host: String
  def port: Int
  def database: String
  def user: String
  def password: String

object PostgresTestContainer:
  /** Same image on every platform. CI pre-pulls this tag. */
  val Image = "postgres:17"

  /** Runs once, on first start, from the image entrypoint. */
  val InitScript = "/docker-entrypoint-initdb.d/init.sql"

  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    PostgresTestContainerPlatform.live
