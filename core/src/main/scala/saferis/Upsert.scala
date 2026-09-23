package saferis

/** Type-safe UPSERT DSL for conditional insert-or-update operations.
  *
  * Usage:
  * {{{
  *   given dialect: Dialect & UpsertSupport & ReturningSupport = PostgresDialect
  *
  *   // Basic upsert
  *   Upsert[Lock]
  *     .values(Lock(instanceId, nodeId, now, expiresAt))
  *     .onConflict(_.instanceId)
  *     .doUpdateAll
  *     .build
  *     .execute
  *
  *   // Conditional upsert with WHERE on conflict
  *   Upsert[Lock]
  *     .values(Lock(instanceId, nodeId, now, expiresAt))
  *     .onConflict(_.instanceId)
  *     .doUpdateAll
  *     .where(_.expiresAt).lt(now)
  *     .or(_.nodeId).eqExcluded  // Only update if expired OR same node
  *     .returning
  *     .queryOne[Lock]
  * }}}
  */
object Upsert:
  /** Create an Upsert builder for a table type */
  inline def apply[A: Table]: UpsertBuilder[A] =
    val table = summon[Table[A]]
    UpsertBuilder(table.name, table.columnMap, table.columns.toVector)

// ============================================================================
// UpsertBuilder - Entry point (needs .values())
// ============================================================================

/** Initial upsert builder - needs entity values. */
final case class UpsertBuilder[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val allColumns: Vector[Column[?]],
):
  /** Provide the entity to insert/update */
  def values(entity: A): UpsertValuesReady[A] =
    UpsertValuesReady(tableName, fieldNamesToColumns, allColumns, entity)

// ============================================================================
// UpsertValuesReady - Has entity, needs .onConflict()
// ============================================================================

/** Upsert builder with entity values - needs conflict columns. */
final case class UpsertValuesReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val allColumns: Vector[Column[?]],
    private[saferis] val entity: A,
):
  /** Specify the conflict column(s) for ON CONFLICT */
  transparent inline def onConflict[T](inline selector: A => T): UpsertConflictReady[A] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName)
    UpsertConflictReady(tableName, fieldNamesToColumns, allColumns, entity, Vector(col.label))
end UpsertValuesReady

// ============================================================================
// UpsertConflictReady - Has conflict columns, needs action (doUpdateAll/doNothing)
// ============================================================================

/** Upsert builder with conflict columns - needs update action. */
final case class UpsertConflictReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val allColumns: Vector[Column[?]],
    private[saferis] val entity: A,
    private[saferis] val conflictColumns: Vector[String],
):
  /** Add another conflict column */
  transparent inline def and[T](inline selector: A => T): UpsertConflictReady[A] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName)
    copy(conflictColumns = conflictColumns :+ col.label)

  /** DO UPDATE SET all non-key, non-generated columns */
  transparent inline def doUpdateAll: UpsertActionReady[A] =
    val table         = summon[Table[A]]
    val updateColumns = table.updateSetClause(entity)
    UpsertActionReady(
      tableName,
      fieldNamesToColumns,
      allColumns,
      entity,
      conflictColumns,
      updateColumns,
      doNothing = false,
    )
  end doUpdateAll

  /** DO NOTHING - only insert if no conflict */
  def doNothing: UpsertDoNothingReady[A] =
    UpsertDoNothingReady(tableName, fieldNamesToColumns, allColumns, entity, conflictColumns)
end UpsertConflictReady

// ============================================================================
// UpsertDoNothingReady - DO NOTHING variant, ready to build
// ============================================================================

/** Upsert with DO NOTHING - ready to build. */
final case class UpsertDoNothingReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val allColumns: Vector[Column[?]],
    private[saferis] val entity: A,
    private[saferis] val conflictColumns: Vector[String],
):
  /** Build the INSERT ... ON CONFLICT DO NOTHING SQL */
  transparent inline def build(using dialect: Dialect & UpsertSupport): SqlFragment =
    val table     = summon[Table[A]]
    val conflicts = conflictColumns.mkString(", ")
    val _         = dialect.upsertDoNothingSql(tableName, "", conflictColumns)
    SqlFragment
      .text(s"insert into $tableName ")
      .append(table.insertColumnsSql)
      .append(SqlFragment.text(" values "))
      .append(table.insertPlaceholdersSql(entity))
      .append(SqlFragment.text(s" on conflict ($conflicts) do nothing"))
  end build
end UpsertDoNothingReady

// ============================================================================
// UpsertActionReady - Has DO UPDATE, can add WHERE or build
// ============================================================================

/** Upsert with DO UPDATE - can add WHERE clause or build directly. */
final case class UpsertActionReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val allColumns: Vector[Column[?]],
    private[saferis] val entity: A,
    private[saferis] val conflictColumns: Vector[String],
    private[saferis] val updateColumns: SqlFragment,
    private[saferis] val doNothing: Boolean,
):
  /** Add a WHERE clause for conditional update (starts the condition) */
  transparent inline def where[T](inline selector: A => T): UpsertWhereBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    UpsertWhereBuilder(this, Alias.unsafe(tableName), col)

  /** Build without WHERE clause */
  transparent inline def build(using dialect: Dialect & UpsertSupport): SqlFragment =
    val table     = summon[Table[A]]
    val conflicts = conflictColumns.mkString(", ")
    val _         = dialect.upsertSql(tableName, "", conflictColumns, "")
    SqlFragment
      .text(s"insert into $tableName ")
      .append(table.insertColumnsSql)
      .append(SqlFragment.text(" values "))
      .append(table.insertPlaceholdersSql(entity))
      .append(SqlFragment.text(s" on conflict ($conflicts) do update set "))
      .append(updateColumns)
  end build

  /** Build with RETURNING clause (no WHERE) */
  transparent inline def returning(using dialect: Dialect & UpsertSupport & ReturningSupport): ReturningQuery[A] =
    ReturningQuery(build :+ SqlFragment.text(" returning *"))
end UpsertActionReady

// ============================================================================
// UpsertWhereBuilder - Building WHERE condition for conflict update
// ============================================================================

/** Builder for WHERE conditions on upsert conflict update. */
final case class UpsertWhereBuilder[A: Table, T](
    action: UpsertActionReady[A],
    alias: Alias,
    column: Column[T],
):
  private def complete(operator: Operator, value: SqlValue): UpsertWhereReady[A] =
    val condition = LiteralCondition(alias, column, operator, value)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    UpsertWhereReady(action, Vector(fragment))

  private def completeUnary(operator: Operator): UpsertWhereReady[A] =
    val condition = UnaryCondition(alias, column, operator)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    UpsertWhereReady(action, Vector(fragment))

  /** Equals comparison */
  def eq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Eq, enc.encode(value))

  /** Not equals comparison */
  def neq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Neq, enc.encode(value))

  /** Less than comparison */
  def lt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lt, enc.encode(value))

  /** Less than or equal comparison */
  def lte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lte, enc.encode(value))

  /** Greater than comparison */
  def gt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gt, enc.encode(value))

  /** Greater than or equal comparison */
  def gte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gte, enc.encode(value))

  /** IS NULL check */
  def isNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNull)

  /** IS NOT NULL check */
  def isNotNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNotNull)

  /** Compare to EXCLUDED pseudo-table value (same column) */
  def eqExcluded: UpsertWhereReady[A] =
    val condition = ExcludedCondition(alias, column, Operator.Eq, column)
    val fragment  = condition.toFragment
    UpsertWhereReady(action, Vector(fragment))

  /** Compare to EXCLUDED pseudo-table with not equal */
  def neqExcluded: UpsertWhereReady[A] =
    val condition = ExcludedCondition(alias, column, Operator.Neq, column)
    val fragment  = condition.toFragment
    UpsertWhereReady(action, Vector(fragment))

end UpsertWhereBuilder

// ============================================================================
// UpsertWhereReady - Has WHERE, can add more conditions or build
// ============================================================================

/** Upsert with WHERE clause - can chain more conditions or build. */
final case class UpsertWhereReady[A: Table](
    private[saferis] val action: UpsertActionReady[A],
    private[saferis] val wherePredicates: Vector[SqlFragment],
):
  /** Chain with OR */
  transparent inline def or[T](inline selector: A => T): UpsertOrBuilder[A, T] =
    UpsertWhereReady.chainOr(this, selector)

  /** Chain with AND */
  transparent inline def and[T](inline selector: A => T): UpsertAndBuilder[A, T] =
    UpsertWhereReady.chainAnd(this, selector)

  /** Build the complete upsert SQL */
  transparent inline def build(using dialect: Dialect & UpsertSupport): SqlFragment =
    val table     = summon[Table[A]]
    val conflicts = action.conflictColumns.mkString(", ")
    val _         = dialect.upsertWithWhereSql(action.tableName, "", action.conflictColumns, "", None)
    val base      =
      SqlFragment
        .text(s"insert into ${action.tableName} ")
        .append(table.insertColumnsSql)
        .append(SqlFragment.text(" values "))
        .append(table.insertPlaceholdersSql(action.entity))
        .append(SqlFragment.text(s" on conflict ($conflicts) do update set "))
        .append(action.updateColumns)
    if wherePredicates.isEmpty then base
    else
      val joined = Placeholder.join(wherePredicates, " or ")
      base.append(SqlFragment.text(" where ")).append(SqlFragment(joined))
  end build

  /** Build with RETURNING clause */
  transparent inline def returning(using dialect: Dialect & UpsertSupport & ReturningSupport): ReturningQuery[A] =
    ReturningQuery(build :+ SqlFragment.text(" returning *"))

end UpsertWhereReady

object UpsertWhereReady:
  inline def chainOr[A: Table, T](
      ready: UpsertWhereReady[A],
      inline selector: A => T,
  ): UpsertOrBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = ready.action.fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    UpsertOrBuilder(ready, Alias.unsafe(ready.action.tableName), col)

  inline def chainAnd[A: Table, T](
      ready: UpsertWhereReady[A],
      inline selector: A => T,
  ): UpsertAndBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = ready.action.fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    UpsertAndBuilder(ready, Alias.unsafe(ready.action.tableName), col)
end UpsertWhereReady

// ============================================================================
// UpsertOrBuilder - Building OR condition
// ============================================================================

/** Builder for OR conditions in upsert WHERE clause. */
final case class UpsertOrBuilder[A: Table, T](
    ready: UpsertWhereReady[A],
    alias: Alias,
    column: Column[T],
):
  private def complete(operator: Operator, value: SqlValue): UpsertWhereReady[A] =
    val condition = LiteralCondition(alias, column, operator, value)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

  private def completeUnary(operator: Operator): UpsertWhereReady[A] =
    val condition = UnaryCondition(alias, column, operator)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

  /** Equals comparison */
  def eq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Eq, enc.encode(value))

  /** Not equals comparison */
  def neq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Neq, enc.encode(value))

  /** Less than comparison */
  def lt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lt, enc.encode(value))

  /** Less than or equal comparison */
  def lte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lte, enc.encode(value))

  /** Greater than comparison */
  def gt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gt, enc.encode(value))

  /** Greater than or equal comparison */
  def gte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gte, enc.encode(value))

  /** IS NULL check */
  def isNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNull)

  /** IS NOT NULL check */
  def isNotNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNotNull)

  /** Compare to EXCLUDED pseudo-table value (same column) */
  def eqExcluded: UpsertWhereReady[A] =
    val condition = ExcludedCondition(alias, column, Operator.Eq, column)
    val fragment  = condition.toFragment
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

  /** Compare to EXCLUDED pseudo-table with not equal */
  def neqExcluded: UpsertWhereReady[A] =
    val condition = ExcludedCondition(alias, column, Operator.Neq, column)
    val fragment  = condition.toFragment
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

end UpsertOrBuilder

// ============================================================================
// UpsertAndBuilder - Building AND condition
// ============================================================================

/** Builder for AND conditions in upsert WHERE clause. */
final case class UpsertAndBuilder[A: Table, T](
    ready: UpsertWhereReady[A],
    alias: Alias,
    column: Column[T],
):
  private def complete(operator: Operator, value: SqlValue): UpsertWhereReady[A] =
    // For AND, we need to group the current predicates and add a new one
    // This is simplified - full implementation would track AND/OR grouping
    val condition = LiteralCondition(alias, column, operator, value)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

  private def completeUnary(operator: Operator): UpsertWhereReady[A] =
    val condition = UnaryCondition(alias, column, operator)
    val fragment  = Condition.toSqlFragment(Vector(condition))
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

  /** Equals comparison */
  def eq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Eq, enc.encode(value))

  /** Not equals comparison */
  def neq(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Neq, enc.encode(value))

  /** Less than comparison */
  def lt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lt, enc.encode(value))

  /** Less than or equal comparison */
  def lte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Lte, enc.encode(value))

  /** Greater than comparison */
  def gt(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gt, enc.encode(value))

  /** Greater than or equal comparison */
  def gte(value: T)(using enc: Encoder[T]): UpsertWhereReady[A] =
    complete(Operator.Gte, enc.encode(value))

  /** IS NULL check */
  def isNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNull)

  /** IS NOT NULL check */
  def isNotNull: UpsertWhereReady[A] =
    completeUnary(Operator.IsNotNull)

  /** Compare to EXCLUDED pseudo-table value (same column) */
  def eqExcluded: UpsertWhereReady[A] =
    val condition = ExcludedCondition(alias, column, Operator.Eq, column)
    val fragment  = condition.toFragment
    ready.copy(wherePredicates = ready.wherePredicates :+ fragment)

end UpsertAndBuilder
