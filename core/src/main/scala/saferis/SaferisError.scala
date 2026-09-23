package saferis

/** Failure of a Saferis operation. SQL cases carry the server message and placeholder SQL. They do not carry a
  * throwable.
  */
sealed trait SaferisError:
  def message: String

object SaferisError:
  final case class UniqueViolation(
      constraint: Option[String],
      message: String,
      sql: Option[String],
  ) extends SaferisError

  final case class ConstraintViolation(
      sqlState: String,
      constraint: Option[String],
      message: String,
      sql: Option[String],
  ) extends SaferisError

  final case class Deadlock(message: String, sql: Option[String]) extends SaferisError

  final case class SerializationFailure(message: String, sql: Option[String]) extends SaferisError

  final case class ConnectionLost(sqlState: String, message: String, sql: Option[String]) extends SaferisError

  final case class Timeout(message: String, sql: Option[String]) extends SaferisError

  final case class SyntaxError(sqlState: String, message: String, sql: Option[String]) extends SaferisError

  final case class DataError(sqlState: String, message: String, sql: Option[String]) extends SaferisError

  final case class QueryError(sqlState: Option[String], message: String, sql: Option[String]) extends SaferisError

  /** Vendor extension only. Named retryable states stay their own cases. */
  final case class Retryable(sqlState: Option[String], message: String, sql: Option[String]) extends SaferisError

  final case class ConnectionError(message: String) extends SaferisError

  final case class DecodingError(columnName: String, expectedType: String, detail: String) extends SaferisError:
    def message = s"Failed to decode column '$columnName' as $expectedType: $detail"

  final case class EncodingError(parameterIndex: Int, detail: String) extends SaferisError:
    def message = s"Failed to encode parameter at index $parameterIndex: $detail"

  final case class ReturningOperationFailed(operation: String, tableName: String) extends SaferisError:
    def message = s"$operation returning failed on table '$tableName'"

  final case class SchemaValidation(issues: List[SchemaIssue]) extends SaferisError:
    def message =
      val count   = issues.length
      val summary = if count == 1 then "1 issue" else s"$count issues"
      s"Schema validation failed with $summary:\n${issues.map(i => s"  - ${i.description}").mkString("\n")}"

  final case class InvalidStatement(issues: List[FragmentIssue]) extends SaferisError:
    def message =
      val count   = issues.length
      val summary = if count == 1 then "1 issue" else s"$count issues"
      s"Statement construction failed with $summary:\n${issues.map(i => s"  - ${i.description}").mkString("\n")}"

  final case class Unsupported(message: String) extends SaferisError

  final case class Unexpected(message: String) extends SaferisError
end SaferisError
