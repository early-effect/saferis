package saferis

/** What a server error means, separate from the code and the sentence the server sent. */
enum SqlCondition:
  case Unique(constraint: Option[ConstraintName])
  case ForeignKey(constraint: Option[ConstraintName])
  case NotNull
  case Check(constraint: Option[ConstraintName])
  case Deadlock
  case Serialization
  case Canceled
  case Connection
  case Shutdown
  case Syntax
  case UndefinedTable
  case UndefinedColumn
  case Data
  case Aborted
  case Other

  def named: Option[ConstraintName] = this match
    case Unique(name)     => name
    case ForeignKey(name) => name
    case Check(name)      => name
    case _                => None
end SqlCondition

object SqlCondition:
  /** A standard SQLSTATE. Postgres-only subclasses stay in `saferis.postgres`. */
  def fromSqlState(state: Option[SqlState]): SqlCondition = state match
    case Some(code) if code == SqlState.UniqueViolation      => SqlCondition.Unique(None)
    case Some(code) if code == SqlState.ForeignKeyViolation  => SqlCondition.ForeignKey(None)
    case Some(code) if code == SqlState.NotNullViolation     => SqlCondition.NotNull
    case Some(code) if code == SqlState.CheckViolation       => SqlCondition.Check(None)
    case Some(code) if code == SqlState.SerializationFailure => SqlCondition.Serialization
    case Some(code) if code.isConnectionException            => SqlCondition.Connection
    case Some(code) if code.isDataException                  => SqlCondition.Data
    case Some(code) if code.isSyntaxOrAccessRule             => SqlCondition.Syntax
    case _                                                   => SqlCondition.Other
end SqlCondition

/** The server's own words, copied onto a [[SaferisError]]. Classification does not rewrite them. */
final case class ServerDetail(
    message: String,
    sql: Option[SqlText],
    sqlState: Option[SqlState],
    vendorCode: Option[Int],
    constraint: Option[ConstraintName],
)
