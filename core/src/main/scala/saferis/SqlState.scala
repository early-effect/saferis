package saferis

/** SQLSTATE classification shared by drivers. Drivers pull the state, message, and constraint name off their native
  * error, then call this. Timeout wins over a vendor retry hook.
  */
object SqlState:
  def defaultRetryable(sqlState: Option[String]): Boolean =
    sqlState match
      case Some(s) if s.startsWith("08") => true
      case Some("40001") | Some("40P01") => true
      case _                             => false

  def classify(
      sqlState: Option[String],
      message: String,
      constraint: Option[String],
      sql: Option[String],
      vendorRetry: Boolean,
      timedOut: Boolean,
  ): SaferisError =
    if timedOut || sqlState.contains("57014") then SaferisError.Timeout(message, sql)
    else
      sqlState match
        case Some("23505") =>
          SaferisError.UniqueViolation(constraint, "unique violation", sql)
        case Some("40P01") =>
          SaferisError.Deadlock(message, sql)
        case Some("40001") =>
          SaferisError.SerializationFailure(message, sql)
        case Some(state) if state.startsWith("08") =>
          SaferisError.ConnectionLost(state, message, sql)
        case Some("23514") =>
          SaferisError.ConstraintViolation("23514", constraint, "check violation", sql)
        case Some(state) if state.startsWith("23") =>
          SaferisError.ConstraintViolation(state, constraint, message, sql)
        case Some(state) if state.startsWith("42") =>
          SaferisError.SyntaxError(state, message, sql)
        case Some(state) if state.startsWith("22") =>
          SaferisError.DataError(state, message, sql)
        case _ if vendorRetry =>
          SaferisError.Retryable(sqlState, message, sql)
        case _ =>
          SaferisError.QueryError(sqlState, message, sql)
end SqlState
