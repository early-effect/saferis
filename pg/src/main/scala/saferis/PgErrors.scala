package saferis

import scala.scalajs.js

/** Reads `code`, `constraint`, and `message` off a rejected `pg` value. */
private[saferis] object PgErrors:
  def info(t: Throwable): PgErrorInfo =
    t match
      case js.JavaScriptException(value) => fromDynamic(value)
      case other                         => PgErrorInfo(None, None, messageOf(other))

  def message(t: Throwable): String =
    val text = info(t).message
    if text.nonEmpty then text else "connection failed"

  /** A dead socket must not go back to the pool. A SQLSTATE error is still a live connection. */
  def broken(err: SaferisError): Boolean = err match
    case _: SaferisError.ConnectionError                                => true
    case _: SaferisError.ConnectionLost                                 => true
    case SaferisError.Unexpected(text)                                  => mentionsConnection(text)
    case SaferisError.QueryError(Some(code), _, _) if !isSqlState(code) => true
    case SaferisError.QueryError(None, text, _)                         => mentionsConnection(text)
    case _                                                              => false

  private def isSqlState(code: String): Boolean =
    code.length == 5 && code.forall(c => c.isDigit || (c >= 'A' && c <= 'Z'))

  private def mentionsConnection(text: String): Boolean =
    val lower = text.toLowerCase
    lower.contains("connection") || lower.contains("econn") || lower.contains("socket")

  private def fromDynamic(value: Any): PgErrorInfo =
    if value == null then PgErrorInfo(None, None, "connection failed")
    else
      val dyn = value.asInstanceOf[js.Dynamic]
      PgErrorInfo(text(dyn, "code"), text(dyn, "constraint"), text(dyn, "message").getOrElse(value.toString))

  private def text(dyn: js.Dynamic, name: String): Option[String] =
    val value = dyn.selectDynamic(name)
    if js.isUndefined(value) || (value eq null) then None
    else
      val raw = value.asInstanceOf[js.Any].toString
      if raw.isEmpty then None else Some(raw)

  private def messageOf(t: Throwable): String =
    Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getName)
end PgErrors
