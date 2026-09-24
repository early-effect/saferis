package saferis.tests

import saferis.Dialect
import saferis.SqlFragment
import saferis.postgres.PostgresDialect
import saferis.sql

import zio.ULayer
import zio.ZLayer

/** What the conformance suite may assume about a database beyond the portable core. */
enum Capability:
  /** `Schema.verify` reads the database catalog. */
  case Catalog

  /** Speaks Postgres SQL: arrays, enums, `jsonb`, `RETURNING`, `ON CONFLICT`, and `pg_catalog`. */
  case PostgresSql
end Capability

/** The database under test. A driver provides this as a layer next to its `SqlSession`, and the conformance suite reads
  * the dialect and capabilities from it.
  *
  * @param longStatement
  *   A statement that runs for several seconds, when the database has one. The timeout test runs only then.
  */
final case class DatabaseTarget(
    dialect: Dialect,
    capabilities: Set[Capability] = Set.empty,
    longStatement: Option[SqlFragment] = None,
):
  def has(capability: Capability): Boolean = capabilities.contains(capability)

object DatabaseTarget:
  val postgres: ULayer[DatabaseTarget] =
    ZLayer.succeed(
      DatabaseTarget(PostgresDialect, Set(Capability.Catalog, Capability.PostgresSql), Some(sql"select pg_sleep(5)"))
    )
