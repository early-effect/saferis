package saferis

/** Shared trait for WHERE clause builders.
  *
  * Provides all WHERE operator implementations (eq, gt, isNull, in, etc.) that can be reused by Query's
  * WhereBuilder1/2/etc and mutation builders (UpdateWhereBuilder, DeleteWhereBuilder).
  *
  * @tparam Parent
  *   The type returned after adding a predicate (e.g., Query1[A], Update[A, HasWhere])
  * @tparam T
  *   The column type being filtered
  */
trait WhereBuilderOps[Parent, T]:
  /** The table alias for column qualification */
  protected def whereAlias: Alias

  /** The column being filtered */
  protected def whereColumn: Column[T]

  /** Add a predicate to the parent and return the updated parent */
  protected def addPredicate(predicate: SqlFragment): Parent

  // === Literal comparison operators ===

  private def completeLiteral(operator: Operator, value: T)(using enc: Encoder[T]): Parent =
    val condition = LiteralCondition(whereAlias, whereColumn, operator, enc.encode(value))
    val whereFrag = Condition.toSqlFragment(Vector(condition))
    addPredicate(whereFrag)

  /** Compare to literal value with equality */
  def eq(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Eq, value)

  /** Compare to literal value with not equal */
  def neq(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Neq, value)

  /** Compare to literal value with less than */
  def lt(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Lt, value)

  /** Compare to literal value with less than or equal */
  def lte(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Lte, value)

  /** Compare to literal value with greater than */
  def gt(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Gt, value)

  /** Compare to literal value with greater than or equal */
  def gte(value: T)(using Encoder[T]): Parent =
    completeLiteral(Operator.Gte, value)

  /** Compare with custom operator */
  def op(operator: Operator)(value: T)(using Encoder[T]): Parent =
    completeLiteral(operator, value)

  // === Unary operators ===

  /** IS NULL check */
  def isNull(): Parent =
    val condition = UnaryCondition(whereAlias, whereColumn, Operator.IsNull)
    val whereFrag = Condition.toSqlFragment(Vector(condition))
    addPredicate(whereFrag)

  /** IS NOT NULL check */
  def isNotNull(): Parent =
    val condition = UnaryCondition(whereAlias, whereColumn, Operator.IsNotNull)
    val whereFrag = Condition.toSqlFragment(Vector(condition))
    addPredicate(whereFrag)

  // === Subquery operators ===

  /** IN subquery - check if value is in the results of another query.
    *
    * Type-safe: the subquery must return the same type T as this column.
    *
    * {{{
    *   val activeIds = Query[Order].where(_.status).eq("active").select(_.userId)
    *   Query[User].where(_.id).inSubquery(activeIds)
    * }}}
    */
  def inSubquery(subquery: SelectQuery[T]): Parent =
    val whereFrag =
      SqlFragment
        .text(s"${whereAlias.toSql}.${whereColumn.label} in (")
        .append(subquery.build)
        .append(SqlFragment.text(")"))
    addPredicate(whereFrag)

  /** NOT IN subquery - type-safe variant */
  def notInSubquery(subquery: SelectQuery[T]): Parent =
    val whereFrag =
      SqlFragment
        .text(s"${whereAlias.toSql}.${whereColumn.label} not in (")
        .append(subquery.build)
        .append(SqlFragment.text(")"))
    addPredicate(whereFrag)

  // === Literal collection operators ===

  /** Varargs membership. One array parameter: `col = ANY($1)`. At least one element is supplied.
    *
    * {{{
    *   Query[User].where(_.status).in("active", "pending")
    * }}}
    *
    * For runtime collections use [[inList]] instead. For subqueries use [[inSubquery]].
    */
  def in(first: T, rest: T*)(using Encoder[T]): Parent =
    inList(first +: rest)

  /** Membership for any `Iterable[T]`. Duplicates are removed. One array parameter: `col = ANY($1)`.
    *
    * On empty (or degenerate-empty-after-dedupe) input the resulting fragment carries a
    * [[FragmentIssue.EmptyCollection]] that surfaces as [[SaferisError.InvalidStatement]] at execution. No DB
    * round-trip on failure.
    *
    * {{{
    *   Query[User].where(_.id).inList(runIds)
    * }}}
    */
  def inList(values: Iterable[T])(using Encoder[T]): Parent =
    membership("= ANY(", values, "WhereBuilder.inList")

  /** Varargs exclusion. One array parameter: `col <> ALL($1)`. */
  def notIn(first: T, rest: T*)(using Encoder[T]): Parent =
    notInList(first +: rest)

  /** Exclusion for any `Iterable[T]`. Same array rules as [[inList]]: `col <> ALL($1)`. */
  def notInList(values: Iterable[T])(using Encoder[T]): Parent =
    membership("<> ALL(", values, "WhereBuilder.notInList")

  private def membership(op: String, values: Iterable[T], helper: String)(using Encoder[T]): Parent =
    val list      = Placeholder.listTagged(values, helper = helper, origin = Placeholder.captureOrigin())
    val whereFrag =
      SqlFragment
        .text(s"${whereAlias.toSql}.${whereColumn.label} $op")
        .append(SqlFragment(list))
        .append(SqlFragment.text(")"))
    addPredicate(whereFrag)

end WhereBuilderOps
