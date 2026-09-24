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
    retry: PgErrorInfo => Boolean = e => SqlState.defaultRetryable(e.code),
    listener: SqlListener = SqlListener.noop,
)

/** Fields taken off a rejected `pg` error before classification. */
final case class PgErrorInfo(code: Option[String], constraint: Option[String], message: String)
