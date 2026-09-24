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
    /** Extra startup parameters, such as `search_path` or `statement_timeout`. A parameter the driver requires (Node
      * sets `DateStyle=ISO`, which its text decode depends on) replaces an entry with the same name.
      */
    parameters: Map[String, String] = Map.empty,
):
  /** The startup `options` string for drivers that send one (node-postgres, libpq): `-c name=value` per parameter.
    * `required` replaces a parameter of the same name, compared case-insensitively, and comes last so the server keeps
    * it either way.
    */
  def startupOptions(required: Map[String, String]): String =
    val names   = required.keySet.map(_.toLowerCase(java.util.Locale.ROOT))
    val chosen  = parameters.filterNot((name, _) => names.contains(name.toLowerCase(java.util.Locale.ROOT)))
    val ordered = chosen.toSeq.sortBy(_._1) ++ required.toSeq.sortBy(_._1)
    ordered.map((name, value) => s"-c ${PgConnectionConfig.escapeOption(s"$name=$value")}").mkString(" ")
end PgConnectionConfig

object PgConnectionConfig:
  /** The server splits `options` on whitespace. A backslash escapes the next character, so both are escaped. */
  def escapeOption(text: String): String =
    val out = new StringBuilder(text.length)
    text.foreach: c =>
      if c == '\\' || c.isWhitespace then out.append('\\')
      out.append(c)
    out.toString
