package saferis.pg

import saferis.ServerError
import saferis.SqlState
import saferis.postgres.PgConnectionConfig

import zio.Duration

/** Node pool settings around a [[PgConnectionConfig]]. */
final case class PgConfig(
    connection: PgConnectionConfig,
    poolSize: Int = 10,
    defaultTimeout: Option[Duration] = None,
    retry: ServerError => Boolean = SqlState.defaultRetryable,
)
