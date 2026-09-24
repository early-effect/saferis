package saferis

import zio.Duration

/** Node `pg` pool settings. Retry sees SQLSTATE, constraint, and message. It does not see a throwable. */
final case class PgConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
    poolSize: Int = 10,
    ssl: Boolean = false,
    defaultTimeout: Option[Duration] = None,
    retry: ServerError => Boolean = SqlState.defaultRetryable,
    listener: SqlListener = SqlListener.noop,
)
