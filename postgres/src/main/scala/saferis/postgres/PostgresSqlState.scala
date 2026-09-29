package saferis.postgres

import saferis.ConstraintName
import saferis.SqlCondition
import saferis.SqlState
import saferis.ServerError

/** SQLSTATEs Postgres sends that are not in the shared SQL standard list. */
object PostgresSqlState:
  val ConnectionFailure: SqlState   = SqlState.code("08006")
  val InFailedTransaction: SqlState = SqlState.code("25P02")
  val Deadlock: SqlState            = SqlState.code("40P01")
  val SyntaxError: SqlState         = SqlState.code("42601")
  val UndefinedColumn: SqlState     = SqlState.code("42703")
  val UndefinedTable: SqlState      = SqlState.code("42P01")
  val QueryCanceled: SqlState       = SqlState.code("57014")
  val AdminShutdown: SqlState       = SqlState.code("57P01")
  val CrashShutdown: SqlState       = SqlState.code("57P02")
  val CannotConnectNow: SqlState    = SqlState.code("57P03")

  def isShutdown(state: SqlState): Boolean =
    state == AdminShutdown || state == CrashShutdown || state == CannotConnectNow

  /** The condition for a SQLSTATE Postgres actually sent. The code itself is not rewritten. */
  def condition(state: Option[SqlState], constraint: Option[ConstraintName]): SqlCondition =
    state match
      case Some(code) if code == InFailedTransaction          => SqlCondition.Aborted
      case Some(code) if code == Deadlock                     => SqlCondition.Deadlock
      case Some(code) if code == QueryCanceled                => SqlCondition.Canceled
      case Some(code) if code == UndefinedTable               => SqlCondition.UndefinedTable
      case Some(code) if code == UndefinedColumn              => SqlCondition.UndefinedColumn
      case Some(code) if code == SyntaxError                  => SqlCondition.Syntax
      case Some(code) if isShutdown(code)                     => SqlCondition.Shutdown
      case Some(code) if code == SqlState.UniqueViolation     => SqlCondition.Unique(constraint)
      case Some(code) if code == SqlState.ForeignKeyViolation => SqlCondition.ForeignKey(constraint)
      case Some(code) if code == SqlState.CheckViolation      => SqlCondition.Check(constraint)
      case _                                                  =>
        SqlCondition.fromSqlState(state) match
          case SqlCondition.Unique(_)     => SqlCondition.Unique(constraint)
          case SqlCondition.ForeignKey(_) => SqlCondition.ForeignKey(constraint)
          case SqlCondition.Check(_)      => SqlCondition.Check(constraint)
          case other                      => other

  def apply(error: ServerError, constraint: Option[ConstraintName]): ServerError =
    error.copy(condition = condition(error.sqlState, constraint))
end PostgresSqlState
