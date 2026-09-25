package saferis

import zio.Trace
import zio.ZIO

/** Type-safe aggregate function DSL for SQL aggregate operations.
  *
  * Usage:
  * {{{
  *   // Get max sequence number with default
  *   Query[EventRow]
  *     .where(_.instanceId).eq(instanceId)
  *     .selectAggregate(_.sequenceNr.max.coalesce(0L))
  *     .queryValue[Long]
  *
  *   // Count all rows
  *   Query[User]
  *     .where(_.status).eq("active")
  *     .selectAggregate(countAll)
  *     .queryValue[Long]
  * }}}
  */

// ============================================================================
// Aggregate Function Enum
// ============================================================================

/** SQL aggregate functions */
enum AggregateFunction(val sql: String):
  case Max   extends AggregateFunction("max")
  case Min   extends AggregateFunction("min")
  case Sum   extends AggregateFunction("sum")
  case Avg   extends AggregateFunction("avg")
  case Count extends AggregateFunction("count")

// ============================================================================
// Aggregate Expression Types
// ============================================================================

/** Base trait for aggregate expressions */
sealed trait AggregateExpr[T]:
  def toFragment: SqlFragment

/** Aggregate function applied to a column */
final case class ColumnAggregate[T](function: AggregateFunction, column: Column[T]) extends AggregateExpr[T]:
  def toFragment: SqlFragment = SqlFragment.text(s"${function.sql}(${column.label})")

  /** Wrap the aggregate in COALESCE with a default value */
  def coalesce(default: T)(using enc: Encoder[T]): CoalesceExpr[T] =
    CoalesceExpr(this, enc.encode(default))

/** COALESCE wrapper for aggregate expressions */
final case class CoalesceExpr[T](expr: AggregateExpr[T], default: SqlValue) extends AggregateExpr[T]:
  def toFragment: SqlFragment =
    SqlFragment
      .text("coalesce(")
      .append(expr.toFragment)
      .append(SqlFragment.text(", "))
      .append(SqlFragment.param(default))
      .append(SqlFragment.text(")"))

/** COUNT(*) aggregate */
case object CountAll extends AggregateExpr[Long]:
  def toFragment: SqlFragment = SqlFragment.text("count(*)")

// ============================================================================
// Column Extensions for Aggregates
// ============================================================================

/** Extension methods to add aggregate functions to columns */
extension [T](column: Column[T])
  /** MAX aggregate function */
  def max: ColumnAggregate[T] = ColumnAggregate(AggregateFunction.Max, column)

  /** MIN aggregate function */
  def min: ColumnAggregate[T] = ColumnAggregate(AggregateFunction.Min, column)

  /** SUM aggregate function */
  def sum: ColumnAggregate[T] = ColumnAggregate(AggregateFunction.Sum, column)

  /** AVG aggregate function */
  def avg: ColumnAggregate[T] = ColumnAggregate(AggregateFunction.Avg, column)

  /** COUNT aggregate function (counts non-null values) */
  def count: ColumnAggregate[Long] = ColumnAggregate(AggregateFunction.Count, column.asInstanceOf[Column[Long]])
end extension

/** Convenience function for COUNT(*) */
def countAll: CountAll.type = CountAll

// ============================================================================
// AggregateQuery - Query returning aggregate value
// ============================================================================

/** A query that returns a single aggregate value.
  *
  * Created by calling `.selectAggregate(...)` on a Query1Ready.
  */
final case class AggregateQuery[A, T](
    tableName: String,
    tableAlias: Option[Alias],
    wherePredicates: Vector[SqlFragment],
    aggregate: AggregateExpr[T],
):
  /** Build the SELECT aggregate SQL */
  def build: SqlFragment =
    val fromClause = tableAlias.fold(tableName)(a => s"$tableName as ${a.value}")
    val head = SqlFragment.text(s"select ").append(aggregate.toFragment).append(SqlFragment.text(s" from $fromClause"))
    if wherePredicates.isEmpty then head
    else
      val whereJoined = Placeholder.join(wherePredicates, " and ")
      head.append(SqlFragment.text(" where ")).append(SqlFragment(whereJoined))
  end build

  /** Execute and return the aggregate value */
  inline def queryValue[R](using RowDecoder[R], Trace): ZIO[SqlSession, SaferisError, Option[R]] =
    build.queryValue[R]
end AggregateQuery
