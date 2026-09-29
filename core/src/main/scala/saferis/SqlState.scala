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

  /** A code this library names. Callers outside the companion use [[parse]]. */
  private[saferis] def code(raw: String): SqlState = raw

  val ConnectionException: SqlState  = "08000"
  val DataException: SqlState        = "22000"
  val NotNullViolation: SqlState     = "23502"
  val ForeignKeyViolation: SqlState  = "23503"
  val UniqueViolation: SqlState      = "23505"
  val CheckViolation: SqlState       = "23514"
  val SerializationFailure: SqlState = "40001"

  extension (state: SqlState)
    /** The two-character class, `23` for every integrity violation. */
    def sqlClass: String = state.take(2)

    def isConnectionException: Boolean = sqlClass == "08"
    def isDataException: Boolean       = sqlClass == "22"
    def isIntegrityViolation: Boolean  = sqlClass == "23"
    def isSyntaxOrAccessRule: Boolean  = sqlClass == "42"

  end extension

  def defaultRetryable(error: ServerError): Boolean =
    error.condition match
      case SqlCondition.Connection | SqlCondition.Serialization | SqlCondition.Deadlock => true
      case _                                                                            => false

  /** One `SaferisError` for every driver. The message and SQLSTATE are the ones on `error`. */
  def classify(
      error: ServerError,
      sql: Option[SqlText],
      retryable: ServerError => Boolean,
  ): SaferisError =
    val detail = ServerDetail(error.message, sql, error.sqlState, error.vendorCode, error.condition.named)
    error.condition match
      case SqlCondition.Unique(_)                 => SaferisError.UniqueViolation(detail)
      case SqlCondition.ForeignKey(_)             => SaferisError.ForeignKeyViolation(detail)
      case SqlCondition.NotNull                   => SaferisError.NotNullViolation(detail)
      case SqlCondition.Check(_)                  => SaferisError.CheckViolation(detail)
      case SqlCondition.Deadlock                  => SaferisError.Deadlock(detail)
      case SqlCondition.Serialization             => SaferisError.SerializationFailure(detail)
      case SqlCondition.Canceled                  => SaferisError.Timeout(detail)
      case SqlCondition.Connection                => SaferisError.ConnectionLost(detail)
      case SqlCondition.Shutdown                  => SaferisError.Shutdown(detail)
      case SqlCondition.Syntax                    => SaferisError.SyntaxError(detail)
      case SqlCondition.UndefinedTable            => SaferisError.UndefinedTable(detail)
      case SqlCondition.UndefinedColumn           => SaferisError.UndefinedColumn(detail)
      case SqlCondition.Data                      => SaferisError.DataError(detail)
      case SqlCondition.Aborted                   => SaferisError.Aborted(detail)
      case SqlCondition.Other if retryable(error) => SaferisError.Retryable(detail)
      case SqlCondition.Other                     => SaferisError.QueryError(detail)
    end match
  end classify
end SqlState

/** What a driver read off a server error, before it becomes a [[SaferisError]]. Not a throwable. */
final case class ServerError(
    condition: SqlCondition,
    message: String,
    sqlState: Option[SqlState] = None,
    vendorCode: Option[Int] = None,
)
