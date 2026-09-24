package saferis.postgres

import zio.Config.Secret
import zio.Duration
import zio.durationInt

/** PEM-encoded certificate material. Not a filesystem path. */
opaque type Pem = String

object Pem:
  def apply(text: String): Pem          = text
  extension (pem: Pem) def text: String = pem

/** How a Postgres driver asks the server to speak TLS. */
enum SslMode:
  case Disable
  case Require

  /** Trust the CA and do not check the server hostname. */
  case VerifyCa(ca: Pem)

  /** Trust the CA and check the server hostname. */
  case VerifyFull(ca: Pem)

/** Host, credentials, TLS, and how long `connect` may wait. Drivers add their own pool settings around this. */
final case class PgConnectionConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: Secret,
    ssl: SslMode = SslMode.Disable,
    connectTimeout: Duration = 10.seconds,
    /** Extra `-c name=value` startup parameters. `DateStyle=ISO` is always set by the Node driver. */
    parameters: Map[String, String] = Map.empty,
)
