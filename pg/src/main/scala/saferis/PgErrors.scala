package saferis.pg

import saferis.*

import scala.scalajs.js

/** Reads `code`, `constraint`, and `message` off a rejected `pg` value. */
private[pg] object PgErrors:
  def info(t: Throwable): ServerError =
    t match
      case js.JavaScriptException(value) => fromDynamic(value)
      case other                         => ServerError(None, messageOf(other))

  def message(t: Throwable): String =
    val text = info(t).message
    if text.nonEmpty then text else "connection failed"

  /** A dead socket must not go back to the pool. Connection errors and class `57` shutdown are dead. A message that
    * merely mentions a connection is not.
    */
  def broken(err: SaferisError): Boolean = err match
    case _: SaferisError.ConnectionError            => true
    case _: SaferisError.ConnectionLost             => true
    case SaferisError.QueryError(Some(state), _, _) => state.isShutdown
    case SaferisError.Retryable(Some(state), _, _)  => state.isShutdown
    case _                                          => false

  /** A transport code (`ECONNRESET`, `EPIPE`) is not a SQLSTATE. The socket failed, so it is `08006`, and the message
    * keeps the code.
    */
  private def fromDynamic(value: Any): ServerError =
    if value == null then ServerError(None, "connection failed")
    else
      val dyn     = value.asInstanceOf[js.Dynamic]
      val message = text(dyn, "message").getOrElse(value.toString)
      val code    = text(dyn, "code")
      val state   = code.map(c => SqlState.parse(c).getOrElse(SqlState.ConnectionFailure))
      ServerError(
        state,
        code.filter(c => SqlState.parse(c).isEmpty).fold(message)(c => s"$message ($c)"),
        text(dyn, "constraint").map(ConstraintName(_)),
      )

  private def text(dyn: js.Dynamic, name: String): Option[String] =
    val value = dyn.selectDynamic(name)
    if js.isUndefined(value) || (value eq null) then None
    else
      val raw = value.asInstanceOf[js.Any].toString
      if raw.isEmpty then None else Some(raw)

  private def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end PgErrors
