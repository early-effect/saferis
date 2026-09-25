package saferis

import zio.Chunk

/** Internal representation of a condition in ON or WHERE clauses. */
sealed trait Condition:
  def toFragment: SqlFragment

/** Binary condition: column op column (e.g., t1.id = t2.user_id) */
final case class BinaryCondition(
    leftAlias: Alias,
    leftColumn: Column[?],
    operator: Operator,
    rightAlias: Alias,
    rightColumn: Column[?],
) extends Condition:
  def toFragment: SqlFragment =
    SqlFragment.text(
      s"${leftAlias.toSql}.${leftColumn.label} ${operator.sql} ${rightAlias.toSql}.${rightColumn.label}"
    )
end BinaryCondition

/** Unary condition: column IS NULL / IS NOT NULL */
final case class UnaryCondition(
    alias: Alias,
    column: Column[?],
    operator: Operator,
) extends Condition:
  def toFragment: SqlFragment =
    SqlFragment.text(s"${alias.toSql}.${column.label} ${operator.sql}")

/** Literal condition: column op parameter. The value is a `Param` piece, never interpolated text. */
final case class LiteralCondition(
    alias: Alias,
    column: Column[?],
    operator: Operator,
    value: SqlValue,
) extends Condition:
  def toFragment: SqlFragment =
    SqlFragment(
      Chunk(
        SqlPiece.Text(SqlText(s"${alias.toSql}.${column.label} ${operator.sql} ")),
        SqlPiece.Param(value),
      )
    )
end LiteralCondition

/** Condition comparing a column to the EXCLUDED pseudo-table (for upsert WHERE clauses). */
final case class ExcludedCondition(
    alias: Alias,
    column: Column[?],
    operator: Operator,
    excludedColumn: Column[?],
) extends Condition:
  def toFragment: SqlFragment =
    SqlFragment.text(s"${alias.toSql}.${column.label} ${operator.sql} excluded.${excludedColumn.label}")

object Condition:
  def toSqlFragment(conditions: Seq[Condition]): SqlFragment =
    if conditions.isEmpty then SqlFragment.empty
    else
      conditions
        .map(_.toFragment)
        .reduce: (left, right) =>
          left.append(SqlFragment.text(" and ")).append(right)
end Condition
