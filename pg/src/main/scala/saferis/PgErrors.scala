package saferis.pg

import saferis.*
import saferis.postgres.PostgresSqlState

import scala.scalajs.js

/** Reads `code`, `constraint`, and `message` off a `pg` error. */
private[pg] object PgErrors:
  def message(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)

  /** A dead socket must not go back to the pool. Connection errors and class `57` shutdown are dead. A message that
    * merely mentions a connection is not.
    */
  def broken(err: SaferisError): Boolean = err match
    case _: SaferisError.ConnectionError => true
    case _: SaferisError.ConnectionLost  => true
    case _: SaferisError.Shutdown        => true
    case _                               => false

  def from(error: PgDatabaseError): ServerError =
    reported(text(error.code), text(error.message).getOrElse("connection failed"), text(error.constraint))

  /** A transport code (`ECONNRESET`, `EPIPE`) is not a SQLSTATE. The socket failed, so the condition is `Connection`
    * and `sqlState` stays empty. The message keeps the code.
    */
  private[pg] def reported(code: Option[String], message: String, constraint: Option[String]): ServerError =
    val named = constraint.map(ConstraintName(_))
    code.flatMap(SqlState.parse) match
      case Some(state) =>
        PostgresSqlState(
          ServerError(SqlCondition.fromSqlState(Some(state)), message, Some(state)),
          named,
        )
      case None =>
        val textMessage = code.fold(message)(c => s"$message ($c)")
        ServerError(SqlCondition.Connection, textMessage)
  end reported

  private def text(value: js.UndefOr[String]): Option[String] =
    value.toOption.filter(raw => raw != null && raw.nonEmpty)
end PgErrors
