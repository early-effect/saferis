package saferis

/** A SQLSTATE: the five-character code, digits and upper-case letters, that SQL databases report a failure with. The
  * first two characters are its class. It reads as a `String`, but only [[SqlState.parse]] or a named constant makes
  * one.
  */
opaque type SqlState <: String = String

object SqlState:
  /** A code a driver reported, when it has the SQLSTATE form. */
  def parse(code: String): Option[SqlState] =
    Option.when(code.length == 5 && code.forall(c => c.isDigit || (c >= 'A' && c <= 'Z')))(code)

  val ConnectionException: SqlState  = "08000"
  val ConnectionFailure: SqlState    = "08006"
  val DataException: SqlState        = "22000"
  val NotNullViolation: SqlState     = "23502"
  val ForeignKeyViolation: SqlState  = "23503"
  val UniqueViolation: SqlState      = "23505"
  val CheckViolation: SqlState       = "23514"
  val InFailedTransaction: SqlState  = "25P02"
  val SerializationFailure: SqlState = "40001"
  val Deadlock: SqlState             = "40P01"
  val SyntaxError: SqlState          = "42601"
  val UndefinedColumn: SqlState      = "42703"
  val UndefinedTable: SqlState       = "42P01"
  val QueryCanceled: SqlState        = "57014"
  val AdminShutdown: SqlState        = "57P01"
  val CrashShutdown: SqlState        = "57P02"
  val CannotConnectNow: SqlState     = "57P03"

  extension (state: SqlState)
    /** The two-character class, `23` for every integrity violation. */
    def sqlClass: String = state.take(2)

    def isConnectionException: Boolean = sqlClass == "08"
    def isDataException: Boolean       = sqlClass == "22"
    def isIntegrityViolation: Boolean  = sqlClass == "23"
    def isSyntaxOrAccessRule: Boolean  = sqlClass == "42"

    /** The server is shutting down or not accepting connections, so the connection is dead. */
    def isShutdown: Boolean = state == AdminShutdown || state == CrashShutdown || state == CannotConnectNow
  end extension

  def defaultRetryable(error: ServerError): Boolean =
    error.sqlState.exists(state => state.isConnectionException || state == SerializationFailure || state == Deadlock)

  def defaultRetryable(sqlState: Option[SqlState]): Boolean =
    defaultRetryable(ServerError(sqlState, ""))

  /** One `SaferisError` for every driver. A timeout is `57014` before this is called. */
  def classify(
      error: ServerError,
      sql: Option[SqlText],
      retryable: ServerError => Boolean,
  ): SaferisError =
    error.sqlState match
      case Some(QueryCanceled) =>
        SaferisError.Timeout(error.message, sql)
      case Some(UniqueViolation) =>
        SaferisError.UniqueViolation(error.constraint, "unique violation", sql)
      case Some(Deadlock) =>
        SaferisError.Deadlock(error.message, sql)
      case Some(SerializationFailure) =>
        SaferisError.SerializationFailure(error.message, sql)
      case Some(state) if state.isConnectionException =>
        SaferisError.ConnectionLost(state, error.message, sql)
      case Some(CheckViolation) =>
        SaferisError.ConstraintViolation(CheckViolation, error.constraint, "check violation", sql)
      case Some(state) if state.isIntegrityViolation =>
        SaferisError.ConstraintViolation(state, error.constraint, error.message, sql)
      case Some(state) if state.isSyntaxOrAccessRule =>
        SaferisError.SyntaxError(state, error.message, sql)
      case Some(state) if state.isDataException =>
        SaferisError.DataError(state, error.message, sql)
      case _ if retryable(error) =>
        SaferisError.Retryable(error.sqlState, error.message, sql)
      case _ =>
        SaferisError.QueryError(error.sqlState, error.message, sql)
end SqlState

/** Fields a driver can read off a server error before classification. Not a throwable. */
final case class ServerError(
    sqlState: Option[SqlState],
    message: String,
    constraint: Option[ConstraintName] = None,
    vendorCode: Option[Int] = None,
)
