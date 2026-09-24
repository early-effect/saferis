package saferis

/** Fields a driver can read off a server error before classification. Not a throwable. */
final case class ServerError(
    sqlState: Option[String],
    message: String,
    constraint: Option[String] = None,
    vendorCode: Option[Int] = None,
)

/** SQLSTATE classification shared by drivers. A timeout is `57014` before this is called. */
object SqlState:
  def defaultRetryable(error: ServerError): Boolean =
    error.sqlState match
      case Some(state) if isConnectionException(state) => true
      case Some("40001") | Some("40P01")               => true
      case _                                           => false

  def defaultRetryable(sqlState: Option[String]): Boolean =
    defaultRetryable(ServerError(sqlState, ""))

  def isConnectionException(sqlState: String): Boolean = sqlState.startsWith("08")

  def isIntegrityViolation(sqlState: String): Boolean = sqlState.startsWith("23")

  def classify(
      error: ServerError,
      sql: Option[String],
      retryable: ServerError => Boolean,
  ): SaferisError =
    error.sqlState match
      case Some("57014") =>
        SaferisError.Timeout(error.message, sql)
      case Some("23505") =>
        SaferisError.UniqueViolation(error.constraint, "unique violation", sql)
      case Some("40P01") =>
        SaferisError.Deadlock(error.message, sql)
      case Some("40001") =>
        SaferisError.SerializationFailure(error.message, sql)
      case Some(state) if isConnectionException(state) =>
        SaferisError.ConnectionLost(state, error.message, sql)
      case Some("23514") =>
        SaferisError.ConstraintViolation("23514", error.constraint, "check violation", sql)
      case Some(state) if isIntegrityViolation(state) =>
        SaferisError.ConstraintViolation(state, error.constraint, error.message, sql)
      case Some(state) if state.startsWith("42") =>
        SaferisError.SyntaxError(state, error.message, sql)
      case Some(state) if state.startsWith("22") =>
        SaferisError.DataError(state, error.message, sql)
      case _ if retryable(error) =>
        SaferisError.Retryable(error.sqlState, error.message, sql)
      case _ =>
        SaferisError.QueryError(error.sqlState, error.message, sql)
end SqlState
