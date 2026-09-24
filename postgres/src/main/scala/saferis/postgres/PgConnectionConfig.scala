package saferis.postgres

import zio.Config.Secret
import zio.Duration
import zio.durationInt

/** How a Postgres driver asks the server to speak TLS. */
enum SslMode:
  case Disable
  case Require
  case VerifyCa(ca: String)
  case VerifyFull(ca: String)

/** Host, credentials, TLS, and how long `connect` may wait. Drivers add their own pool settings around this. */
final case class PgConnectionConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: Secret,
    ssl: SslMode = SslMode.Disable,
    connectTimeout: Duration = 10.seconds,
)
