package saferis.pg

import saferis.*
import saferis.postgres.PostgresSqlState

import scala.scalajs.js

/** Reads `code`, `constraint`, and `message` off a rejected `pg` value. */
private[pg] object PgErrors:
  def info(t: Throwable): ServerError =
    t match
      case js.JavaScriptException(value) => fromDynamic(value)
      case other                         => ServerError(SqlCondition.Connection, messageOf(other))

  def message(t: Throwable): String =
    val text = info(t).message
    if text.nonEmpty then text else "connection failed"

  /** A dead socket must not go back to the pool. Connection errors and class `57` shutdown are dead. A message that
    * merely mentions a connection is not.
    */
  def broken(err: SaferisError): Boolean = err match
    case _: SaferisError.ConnectionError => true
    case _: SaferisError.ConnectionLost  => true
    case _: SaferisError.Shutdown        => true
    case _                               => false

  /** A transport code (`ECONNRESET`, `EPIPE`) is not a SQLSTATE. The socket failed, so the condition is `Connection`
    * and `sqlState` stays empty. The message keeps the code.
    */
  private def fromDynamic(value: Any): ServerError =
    if value == null then ServerError(SqlCondition.Connection, "connection failed")
    else
      val dyn        = value.asInstanceOf[js.Dynamic]
      val message    = text(dyn, "message").getOrElse(value.toString)
      val code       = text(dyn, "code")
      val parsed     = code.flatMap(SqlState.parse)
      val constraint = text(dyn, "constraint").map(ConstraintName(_))
      parsed match
        case Some(state) =>
          PostgresSqlState(
            ServerError(SqlCondition.fromSqlState(Some(state)), message, Some(state)),
            constraint,
          )
        case None =>
          val textMessage = code.fold(message)(c => s"$message ($c)")
          ServerError(SqlCondition.Connection, textMessage)

  private def text(dyn: js.Dynamic, name: String): Option[String] =
    val value = dyn.selectDynamic(name)
    if js.isUndefined(value) || (value eq null) then None
    else
      val raw = value.asInstanceOf[js.Any].toString
      if raw.isEmpty then None else Some(raw)

  private def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end PgErrors
