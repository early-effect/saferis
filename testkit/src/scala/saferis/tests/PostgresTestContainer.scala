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
  val live: ZLayer[Any, Throwable, PostgresTestContainer] =
    PostgresTestContainerPlatform.live
