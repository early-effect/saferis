package saferis

/** Represents a group of conditions that can be combined with AND/OR.
  *
  * Used with `.andWhere` for complex conditions like: `.andWhere(w => w(_.status).isNull.or(_.expired).eq(true))`
  *
  * Generates SQL with proper parentheses: `(status IS NULL OR expired = ?)`
  */
sealed trait WhereGroup:
  /** Generate the SQL fragment for this condition group */
  def toSqlFragment: SqlFragment

/** A single condition (leaf node) */
final case class SingleCondition(fragment: SqlFragment) extends WhereGroup:
  def toSqlFragment: SqlFragment = fragment

/** OR group: combines conditions with OR, wrapped in parentheses.
  *
  * Example: `(a OR b OR c)`
  */
final case class OrGroup(conditions: Vector[WhereGroup]) extends WhereGroup:
  def toSqlFragment: SqlFragment =
    if conditions.isEmpty then SqlFragment.empty
    else
      val parts  = conditions.map(_.toSqlFragment)
      val joined = Placeholder.join(parts, " or ")
      SqlFragment.text("(").append(SqlFragment(joined)).append(SqlFragment.text(")"))

/** AND group: combines conditions with AND, wrapped in parentheses.
  *
  * Example: `(a AND b AND c)`
  */
final case class AndGroup(conditions: Vector[WhereGroup]) extends WhereGroup:
  def toSqlFragment: SqlFragment =
    if conditions.isEmpty then SqlFragment.empty
    else
      val parts  = conditions.map(_.toSqlFragment)
      val joined = Placeholder.join(parts, " and ")
      SqlFragment.text("(").append(SqlFragment(joined)).append(SqlFragment.text(")"))

// ============================================================================
// WhereGroupBuilder - Entry point for grouped conditions
// ============================================================================

/** Builder for creating grouped WHERE conditions within a lambda.
  *
  * Usage:
  * {{{
  *   Query[User]
  *     .where(_.active).eq(true)
  *     .andWhere(w => w(_.status).isNull.or(_.deletedAt).isNotNull)
  *     .query[User]
  * }}}
  *
  * Generates: `WHERE active = ? AND (status IS NULL OR deleted_at IS NOT NULL)`
  */
final case class WhereGroupBuilder[A](
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val tableAlias: Alias,
)(using Table[A]):
  /** Start a condition on a column: `w(_.field)` */
  inline def apply[T](inline selector: A => T): WhereGroupColumnBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    WhereGroupColumnBuilder(this, tableAlias, col)

// ============================================================================
// WhereGroupColumnBuilder - Building a single condition
// ============================================================================

/** Builds a single condition within a group.
  *
  * After completing (e.g., `.eq(value)`), returns a WhereGroupChain that allows `.or()` or `.and()` chaining.
  */
final case class WhereGroupColumnBuilder[A: Table, T](
    builder: WhereGroupBuilder[A],
    alias: Alias,
    column: Column[T],
):
  private def complete(operator: Operator, value: SqlValue): WhereGroupChain[A] =
    val condition = LiteralCondition(alias, column, operator, value)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    WhereGroupChain(builder, SingleCondition(fragment))

  private def completeUnary(operator: Operator): WhereGroupChain[A] =
    val condition = UnaryCondition(alias, column, operator)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    WhereGroupChain(builder, SingleCondition(fragment))

  /** Equals comparison */
  def eq(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Eq, enc.encode(value))

  /** Not equals comparison */
  def neq(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Neq, enc.encode(value))

  /** Less than comparison */
  def lt(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Lt, enc.encode(value))

  /** Less than or equal comparison */
  def lte(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Lte, enc.encode(value))

  /** Greater than comparison */
  def gt(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Gt, enc.encode(value))

  /** Greater than or equal comparison */
  def gte(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Gte, enc.encode(value))

  /** IS NULL check */
  def isNull: WhereGroupChain[A] =
    completeUnary(Operator.IsNull)

  /** IS NOT NULL check */
  def isNotNull: WhereGroupChain[A] =
    completeUnary(Operator.IsNotNull)

  /** Custom operator */
  def op(operator: Operator)(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(operator, enc.encode(value))

end WhereGroupColumnBuilder

// ============================================================================
// WhereGroupChain - Allows chaining with .or() or .and()
// ============================================================================

/** Chain for building complex conditions with OR/AND.
  *
  * After completing a condition, use `.or(_.field)` or `.and(_.field)` to add more conditions.
  */
final case class WhereGroupChain[A: Table](
    builder: WhereGroupBuilder[A],
    group: WhereGroup,
):
  /** Chain with OR: `w(_.a).eq(1).or(_.b).eq(2)` produces `(a = ? OR b = ?)` */
  transparent inline def or[T](inline selector: A => T): WhereGroupOrBuilder[A, T] =
    WhereGroupChain.chainOr(this, selector)

  /** Chain with AND: `w(_.a).eq(1).and(_.b).eq(2)` produces `(a = ? AND b = ?)` */
  transparent inline def and[T](inline selector: A => T): WhereGroupOrBuilder[A, T] =
    WhereGroupChain.chainAnd(this, selector)

  /** Get the underlying WhereGroup for building SQL */
  def toWhereGroup: WhereGroup = group

end WhereGroupChain

object WhereGroupChain:
  inline def chainOr[A: Table, T](
      chain: WhereGroupChain[A],
      inline selector: A => T,
  ): WhereGroupOrBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = chain.builder.fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    WhereGroupOrBuilder(chain.builder, chain.builder.tableAlias, col, chain.group, isOr = true)

  inline def chainAnd[A: Table, T](
      chain: WhereGroupChain[A],
      inline selector: A => T,
  ): WhereGroupOrBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = chain.builder.fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    WhereGroupOrBuilder(chain.builder, chain.builder.tableAlias, col, chain.group, isOr = false)
end WhereGroupChain

// ============================================================================
// WhereGroupOrBuilder - Continues chain after .or() or .and()
// ============================================================================

/** Builder for the next condition after `.or()` or `.and()`. */
final case class WhereGroupOrBuilder[A: Table, T](
    builder: WhereGroupBuilder[A],
    alias: Alias,
    column: Column[T],
    previous: WhereGroup,
    isOr: Boolean,
):
  private def complete(operator: Operator, value: SqlValue): WhereGroupChain[A] =
    val condition    = LiteralCondition(alias, column, operator, value)
    val fragment     = Condition.toSqlFragment(Vector(condition))
    val newCondition = SingleCondition(fragment)
    val combined     =
      if isOr then
        previous match
          case OrGroup(conds) => OrGroup(conds :+ newCondition)
          case other          => OrGroup(Vector(other, newCondition))
      else
        previous match
          case AndGroup(conds) => AndGroup(conds :+ newCondition)
          case other           => AndGroup(Vector(other, newCondition))
    WhereGroupChain(builder, combined)
  end complete

  private def completeUnary(operator: Operator): WhereGroupChain[A] =
    val condition    = UnaryCondition(alias, column, operator)
    val fragment     = Condition.toSqlFragment(Vector(condition))
    val newCondition = SingleCondition(fragment)
    val combined     =
      if isOr then
        previous match
          case OrGroup(conds) => OrGroup(conds :+ newCondition)
          case other          => OrGroup(Vector(other, newCondition))
      else
        previous match
          case AndGroup(conds) => AndGroup(conds :+ newCondition)
          case other           => AndGroup(Vector(other, newCondition))
    WhereGroupChain(builder, combined)
  end completeUnary

  /** Equals comparison */
  def eq(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Eq, enc.encode(value))

  /** Not equals comparison */
  def neq(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Neq, enc.encode(value))

  /** Less than comparison */
  def lt(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Lt, enc.encode(value))

  /** Less than or equal comparison */
  def lte(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Lte, enc.encode(value))

  /** Greater than comparison */
  def gt(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Gt, enc.encode(value))

  /** Greater than or equal comparison */
  def gte(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(Operator.Gte, enc.encode(value))

  /** IS NULL check */
  def isNull: WhereGroupChain[A] =
    completeUnary(Operator.IsNull)

  /** IS NOT NULL check */
  def isNotNull: WhereGroupChain[A] =
    completeUnary(Operator.IsNotNull)

  /** Custom operator */
  def op(operator: Operator)(value: T)(using enc: Encoder[T]): WhereGroupChain[A] =
    complete(operator, enc.encode(value))

end WhereGroupOrBuilder
