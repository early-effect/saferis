package saferis

/** Failure of a Saferis operation. SQL cases carry the server message and the statement with placeholders. They do not
  * carry a throwable.
  */
sealed trait SaferisError:
  def message: String

object SaferisError:
  final case class UniqueViolation(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class ForeignKeyViolation(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class NotNullViolation(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class CheckViolation(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class Deadlock(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class SerializationFailure(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class ConnectionLost(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class Timeout(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class Shutdown(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class SyntaxError(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class UndefinedTable(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class UndefinedColumn(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class DataError(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class Aborted(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class QueryError(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  /** Vendor extension only. Named retryable conditions stay their own cases. */
  final case class Retryable(detail: ServerDetail) extends SaferisError:
    def message: String = detail.message

  final case class ConnectionError(message: String) extends SaferisError

  final case class DecodingError(columnName: ColumnName, expectedType: TypeName, detail: String) extends SaferisError:
    def message = s"Failed to decode column '$columnName' as $expectedType: $detail"

  final case class EncodingError(parameterIndex: Int, detail: String) extends SaferisError:
    def message = s"Failed to encode parameter at index $parameterIndex: $detail"

  final case class ReturningOperationFailed(operation: String, tableName: TableName) extends SaferisError:
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
