package saferis

import zio.Chunk
import zio.Trace
import zio.ZIO
import zio.stream.ZStream

/** Join types for SQL JOIN operations */
enum JoinType:
  case Inner, Left, Right, Full, Cross

  def toSql: String = this match
    case Inner => "inner join"
    case Left  => "left join"
    case Right => "right join"
    case Full  => "full join"
    case Cross => "cross join"

/** Internal representation of a join clause */
final case class JoinClause(
    tableName: String,
    alias: Alias,
    joinType: JoinType,
    condition: SqlFragment,
)

/** Scoped alias generator - creates deterministic aliases per query chain. Each Query[A] call creates a fresh
  * generator, so the same query always produces the same SQL.
  */
private[saferis] class AliasGenerator:
  import scala.collection.mutable

  // Map of table name -> counter (mutable, but instance is scoped per query)
  private val counters = mutable.Map[String, Int]()

  /** Generate next alias for a table name. Uses format: tablename_ref_N (e.g., "users_ref_1", "orders_ref_1").
    *
    * For schema-qualified table names (e.g. "prd.foo"), the schema prefix is stripped and any remaining non-identifier
    * characters are replaced with underscores so the alias is a valid unquoted SQL identifier. The `counters` map is
    * keyed on the *sanitized* bare name so distinct schemas with the same bare name (e.g. "prd.foo" vs "staging.foo")
    * share a counter and therefore get distinct aliases (`foo_ref_1`, `foo_ref_2`).
    */
  def next(tableName: String): Alias =
    val bare      = tableName.substring(tableName.lastIndexOf('.') + 1)
    val sanitized = bare.replaceAll("[^A-Za-z0-9_]", "_")
    val count     = counters.getOrElse(sanitized, 0) + 1
    counters(sanitized) = count
    Alias.unsafe(s"${sanitized}_ref_$count")

  /** Create an aliased Instance from a Table with a generated alias */
  def aliasedInstance[A](using table: Table[A]): Instance[A] =
    val alias   = next(table.name)
    val columns = table.columns.map(_.withTableAlias(Some(alias)))
    Instance[A](table.name, columns, Some(alias), Vector.empty)
end AliasGenerator

private[saferis] object AliasGenerator:
  /** Create a new scoped generator for a query chain */
  def create(): AliasGenerator = new AliasGenerator()

  /** For backwards compatibility with tests - no-op since each query gets fresh generator */
  def reset(): Unit = ()

// ============================================================================
// QueryBase - Base trait for all query types
// ============================================================================

/** Base trait for all query types (Query1 through Query5).
  *
  * This allows subqueries to be arbitrarily complex - a subquery can be a simple single-table query or a multi-table
  * join.
  */
trait QueryBase:
  /** Build the SQL fragment for this query */
  def build: SqlFragment

/** A query with a known result type.
  *
  * Created by calling `.select(_.column)` on a query. The type parameter `T` tracks the result type for compile-time
  * checking in IN subqueries.
  *
  * Usage:
  * {{{
  *   // Type T is inferred from the selected column
  *   val subquery: SelectQuery[Int] = Query[Order].select(_.userId)
  *
  *   // IN requires matching types - this compiles because _.id is Int
  *   Query[User].where(_.id).inSubquery(subquery)
  *
  *   // This would NOT compile: _.name is String, subquery returns Int
  *   // Query[User].where(_.name).inSubquery(subquery)
  * }}}
  */
final case class SelectQuery[T](query: QueryBase) extends QueryBase:
  def build: SqlFragment = query.build

// ============================================================================
// Query1Builder - Single table (entry point, not yet ready to execute)
// ============================================================================

/** Derived table source - subquery with user-provided alias */
final case class DerivedSource(subquery: SelectQuery[?], alias: Alias)

/** Query builder for a single table - the starting point for building queries.
  *
  * This builder does NOT have execution methods (.query, .queryOne, .build). You must call .where(), .limit(),
  * .seekAfter(), or .all to get a Query1Ready which can be executed.
  *
  * Usage:
  * {{{
  *   Query[User]
  *     .where(_.name).eq("Alice")  // Returns Query1Ready
  *     .orderBy(users.name.asc)
  *     .limit(20)
  *     .query[User]
  * }}}
  */
final case class Query1Builder[A: Table](
    private[saferis] val gen: AliasGenerator,
    baseInstance: Instance[A],
    sorts: Vector[Sort[?]] = Vector.empty,
    selectColumns: Vector[Column[?]] = Vector.empty,
    derivedSource: Option[DerivedSource] = None,
):
  // === ORDER BY Methods (stay on Builder) ===

  /** Add an ORDER BY clause */
  def orderBy(sort: Sort[?]): Query1Builder[A] =
    copy(sorts = sorts :+ sort)

  /** Add multiple ORDER BY clauses */
  def orderBy(sort: Sort[?], moreSorts: Sort[?]*): Query1Builder[A] =
    copy(sorts = sorts ++ (sort +: moreSorts))

  // === SELECT Methods (for subqueries, stay on Builder) ===

  /** Select a single column (for use in IN subqueries). */
  inline def select[T](inline selector: A => T): SelectQuery[T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val column    = baseInstance.fieldNamesToColumns(fieldName)
    // Create a Ready version for the subquery (subqueries don't need safety constraints)
    SelectQuery[T](
      Query1Ready(baseInstance, Vector.empty, sorts, Vector.empty, None, None, Vector(column), derivedSource)
    )

  /** Select multiple columns. */
  def select(columns: Column[?]*): Query1Builder[A] =
    copy(selectColumns = columns.toVector)

  /** Select all columns with a specified result type (for use in derived tables). */
  def selectAll[R](using @scala.annotation.unused t: Table[R]): SelectQuery[R] =
    SelectQuery[R](
      Query1Ready(baseInstance, Vector.empty, sorts, Vector.empty, None, None, selectColumns, derivedSource)
    )

  // === JOIN Methods (stay on Builder) ===

  /** Start an INNER JOIN */
  def innerJoin[B: Table]: JoinBuilder1[A, B] =
    JoinBuilder1(this, JoinType.Inner)

  /** Start a LEFT JOIN */
  def leftJoin[B: Table]: JoinBuilder1[A, B] =
    JoinBuilder1(this, JoinType.Left)

  /** Start a RIGHT JOIN */
  def rightJoin[B: Table]: JoinBuilder1[A, B] =
    JoinBuilder1(this, JoinType.Right)

  /** Start a FULL JOIN */
  def fullJoin[B: Table]: JoinBuilder1[A, B] =
    JoinBuilder1(this, JoinType.Full)

  // === WHERE Methods (transition to Ready) ===

  /** Add a WHERE predicate using SqlFragment - transitions to Ready */
  def where(predicate: SqlFragment): Query1Ready[A] =
    Query1Ready(baseInstance, Vector(predicate), sorts, Vector.empty, None, None, selectColumns, derivedSource)

  /** Start a type-safe WHERE condition - transitions to Ready via WhereBuilder1 */
  inline def where[T](inline selector: A => T): WhereBuilder1[A, T] =
    val col   = baseInstance.column(selector)
    val alias = baseInstance.alias.getOrElse(Alias.unsafe(baseInstance.tableName))
    WhereBuilder1(this, alias, col)

  /** EXISTS subquery */
  def whereExists(subquery: QueryBase): Query1Ready[A] =
    val whereFrag = SqlFragment.text("exists (").append(subquery.build).append(SqlFragment.text(")"))
    Query1Ready(baseInstance, Vector(whereFrag), sorts, Vector.empty, None, None, selectColumns, derivedSource)

  /** NOT EXISTS subquery */
  def whereNotExists(subquery: QueryBase): Query1Ready[A] =
    val whereFrag = SqlFragment.text("not exists (").append(subquery.build).append(SqlFragment.text(")"))
    Query1Ready(baseInstance, Vector(whereFrag), sorts, Vector.empty, None, None, selectColumns, derivedSource)

  // === LIMIT/SEEK Methods (transition to Ready) ===

  /** Set LIMIT - transitions to Ready (bounded query is safe) */
  def limit(n: Int): Query1Ready[A] =
    Query1Ready(baseInstance, Vector.empty, sorts, Vector.empty, Some(n), None, selectColumns, derivedSource)

  /** Add seek-based pagination cursor - transitions to Ready */
  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query1Ready[A] =
    Query1Ready(
      baseInstance,
      Vector.empty,
      sorts,
      Vector(Seek(column, direction, value, sortOrder, nullOrder)),
      None,
      None,
      selectColumns,
      derivedSource,
    )

  /** Convenience for forward seek (>). Gets rows AFTER the cursor value. */
  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query1Ready[A] =
    seek(column, SeekDir.Gt, value, sortOrder)

  /** Convenience for backward seek (<). Gets rows BEFORE the cursor value. */
  def seekBefore[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Desc): Query1Ready[A] =
    seek(column, SeekDir.Lt, value, sortOrder)

  // === ALL Method (explicit opt-in to fetch all rows) ===

  /** Explicitly mark this as fetching all rows.
    *
    * This is required to prevent accidental unbounded queries.
    */
  def all: Query1Ready[A] =
    Query1Ready(baseInstance, Vector.empty, sorts, Vector.empty, None, None, selectColumns, derivedSource)

end Query1Builder

// ============================================================================
// Query1Ready - Single table query ready to execute
// ============================================================================

/** Query for a single table that is ready to execute.
  *
  * This type has execution methods (.query, .queryOne, .build) because it has been constrained by WHERE, LIMIT,
  * pagination, or explicit .all.
  */
final case class Query1Ready[A: Table](
    baseInstance: Instance[A],
    wherePredicates: Vector[SqlFragment],
    sorts: Vector[Sort[?]],
    seeks: Vector[Seek[?]],
    limitValue: Option[Int],
    offsetValue: Option[Long],
    selectColumns: Vector[Column[?]],
    derivedSource: Option[DerivedSource],
) extends QueryBase:
  // === WHERE Methods (chain on Ready) ===

  /** Add a WHERE predicate using SqlFragment */
  def where(predicate: SqlFragment): Query1Ready[A] =
    copy(wherePredicates = wherePredicates :+ predicate)

  /** Start a type-safe WHERE condition */
  inline def where[T](inline selector: A => T): WhereBuilder1Ready[A, T] =
    val col   = baseInstance.column(selector)
    val alias = baseInstance.alias.getOrElse(Alias.unsafe(baseInstance.tableName))
    WhereBuilder1Ready(this, alias, col)

  /** EXISTS subquery */
  def whereExists(subquery: QueryBase): Query1Ready[A] =
    val whereFrag = SqlFragment.text("exists (").append(subquery.build).append(SqlFragment.text(")"))
    copy(wherePredicates = wherePredicates :+ whereFrag)

  /** NOT EXISTS subquery */
  def whereNotExists(subquery: QueryBase): Query1Ready[A] =
    val whereFrag = SqlFragment.text("not exists (").append(subquery.build).append(SqlFragment.text(")"))
    copy(wherePredicates = wherePredicates :+ whereFrag)

  /** Add a grouped WHERE condition using lambda syntax for complex OR/AND expressions.
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
  def andWhere(builder: WhereGroupBuilder[A] => WhereGroupChain[A]): Query1Ready[A] =
    val groupBuilder = WhereGroupBuilder[A](
      baseInstance.fieldNamesToColumns,
      baseInstance.alias.getOrElse(Alias.unsafe(baseInstance.tableName)),
    )
    val chain     = builder(groupBuilder)
    val whereFrag = chain.toWhereGroup.toSqlFragment
    copy(wherePredicates = wherePredicates :+ whereFrag)

  // === ORDER BY Methods ===

  /** Add an ORDER BY clause */
  def orderBy(sort: Sort[?]): Query1Ready[A] =
    copy(sorts = sorts :+ sort)

  /** Add multiple ORDER BY clauses */
  def orderBy(sort: Sort[?], moreSorts: Sort[?]*): Query1Ready[A] =
    copy(sorts = sorts ++ (sort +: moreSorts))

  // === PAGINATION Methods ===

  /** Set LIMIT */
  def limit(n: Int): Query1Ready[A] = copy(limitValue = Some(n))

  /** Set OFFSET (safe because already Ready) */
  def offset(n: Long): Query1Ready[A] = copy(offsetValue = Some(n))

  // === SEEK Methods ===

  /** Add seek-based pagination cursor */
  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query1Ready[A] =
    copy(seeks = seeks :+ Seek(column, direction, value, sortOrder, nullOrder))

  /** Add a pre-built Seek specification */
  def seek(s: Seek[?]): Query1Ready[A] = copy(seeks = seeks :+ s)

  /** Convenience for forward seek (>). Gets rows AFTER the cursor value. */
  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query1Ready[A] =
    seek(column, SeekDir.Gt, value, sortOrder)

  /** Convenience for backward seek (<). Gets rows BEFORE the cursor value. */
  def seekBefore[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Desc): Query1Ready[A] =
    seek(column, SeekDir.Lt, value, sortOrder)

  // === SELECT Methods (for subqueries) ===

  /** Select a single column (for use in IN subqueries). */
  inline def select[T](inline selector: A => T): SelectQuery[T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val column    = baseInstance.fieldNamesToColumns(fieldName)
    SelectQuery[T](copy(selectColumns = Vector(column)))

  /** Select multiple columns. */
  def select(columns: Column[?]*): Query1Ready[A] =
    copy(selectColumns = columns.toVector)

  /** Select all columns with a specified result type (for use in derived tables). */
  def selectAll[R](using @scala.annotation.unused t: Table[R]): SelectQuery[R] =
    SelectQuery[R](this)

  // === AGGREGATE Methods ===

  /** Select an aggregate function on a column.
    *
    * Usage:
    * {{{
    *   Query[EventRow]
    *     .where(_.instanceId).eq(instanceId)
    *     .selectAggregate(_.sequenceNr)(_.max.coalesce(0L))
    *     .queryValue[Long]
    * }}}
    */
  transparent inline def selectAggregate[T, R](inline selector: A => T)(
      f: Column[T] => AggregateExpr[R]
  ): AggregateQuery[A, R] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val column    = baseInstance.fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    val agg       = f(column)
    AggregateQuery(baseInstance.tableName, baseInstance.alias, wherePredicates, agg)

  /** Select a pre-built aggregate expression directly. */
  def selectAggregate[T](aggregate: AggregateExpr[T]): AggregateQuery[A, T] =
    AggregateQuery(baseInstance.tableName, baseInstance.alias, wherePredicates, aggregate)

  // === BUILD Methods ===

  /** Build the SQL fragment for this query */
  def build: SqlFragment =
    val selectClause = if selectColumns.isEmpty then "*" else selectColumns.map(_.label).mkString(", ")

    // For derived tables, use (subquery) as alias; otherwise use table as alias
    val from = derivedSource match
      case Some(derived) =>
        SqlFragment.text("(").append(derived.subquery.build).append(SqlFragment.text(s") as ${derived.alias.value}"))
      case None =>
        val fromSqlPart =
          baseInstance.alias.fold(baseInstance.tableName)(a => s"${baseInstance.tableName} as ${a.value}")
        SqlFragment.text(fromSqlPart)

    var result = SqlFragment.text(s"select $selectClause from ").append(from)

    // WHERE clause (user predicates + seek predicates)
    val seekPredicates = seeks.map(_.toWherePredicate)
    val allPredicates  = (wherePredicates ++ seekPredicates).filter(_.sql.trim.nonEmpty)
    if allPredicates.nonEmpty then
      val joined = Placeholder.join(allPredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(joined)

    // ORDER BY clause (explicit sorts + seek sorts)
    val seekSorts = seeks.map(_.toSort)
    val allSorts  = sorts ++ seekSorts
    if allSorts.nonEmpty then
      val sortFragments = allSorts.map(_.toSqlFragment)
      val joined        = Placeholder.join(sortFragments, ", ")
      result = result :+ SqlFragment.text(" order by ") :+ SqlFragment(joined)

    // LIMIT/OFFSET
    limitValue.foreach(n => result = result :+ SqlFragment.text(s" limit $n"))
    offsetValue.foreach(n => result = result :+ SqlFragment.text(s" offset $n"))

    result
  end build

  /** Execute query */
  inline def query[R: Table](using Trace): ZIO[SqlSession, SaferisError, Chunk[R]] = build.query[R]

  /** Execute query returning first row */
  inline def queryOne[R: Table](using Trace): ZIO[SqlSession, SaferisError, Option[R]] = build.queryOne[R]

  /** Execute query as lazy stream */
  inline def queryStream[R: Table](using Trace): ZStream[SqlSession, SaferisError, R] =
    build.queryStream[R]

  /** Stream pages of results using seek pagination.
    *
    * Each page is fetched with a separate query, releasing the connection between pages. Use this for long-running
    * operations where you want to checkpoint progress or avoid holding connections open.
    *
    * @param cursorSelector
    *   Field selector for the cursor column (e.g., `_.id`)
    * @param pageSize
    *   Number of items per page
    * @param startAfter
    *   Optional cursor to resume from (exclusive - starts AFTER this value)
    * @return
    *   A ZStream of Page objects containing items and cursor for checkpointing
    */
  inline def pagedStream[K: Encoder](
      inline cursorSelector: A => K,
      pageSize: Int,
      startAfter: Option[K] = None,
  )(using Trace): ZStream[SqlSession, SaferisError, Page[A, K]] =
    val column        = baseInstance.column(cursorSelector)
    val extractCursor = Macros.extractFieldValueFunc(cursorSelector)
    PagedStreamOps.pagedStream[A, K](this, column, extractCursor, pageSize, startAfter)

  /** Stream rows using seek pagination with connection release between batches.
    *
    * Like `pagedStream` but returns individual rows instead of Page objects. Items are emitted lazily to downstream
    * consumers, though each batch is fetched eagerly to release the connection.
    *
    * @param cursorSelector
    *   Field selector for the cursor column (e.g., `_.id`)
    * @param batchSize
    *   Number of rows to fetch before reconnecting
    * @param startAfter
    *   Optional cursor to resume from (exclusive)
    * @return
    *   A ZStream of individual rows
    */
  inline def seekingStream[K: Encoder](
      inline cursorSelector: A => K,
      batchSize: Int,
      startAfter: Option[K] = None,
  )(using Trace): ZStream[SqlSession, SaferisError, A] =
    val column        = baseInstance.column(cursorSelector)
    val extractCursor = Macros.extractFieldValueFunc(cursorSelector)
    PagedStreamOps.seekingStream[A, K](this, column, extractCursor, batchSize, startAfter)

end Query1Ready

// ============================================================================
// WhereBuilder1 - Type-safe WHERE condition builder (Builder -> Ready)
// ============================================================================

/** Builder for type-safe WHERE conditions on a single table.
  *
  * Returns Query1Ready since adding a WHERE clause makes the query safe.
  */
final case class WhereBuilder1[A: Table, T](
    builder: Query1Builder[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[Query1Ready[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): Query1Ready[A] =
    Query1Ready(
      builder.baseInstance,
      Vector(predicate),
      builder.sorts,
      Vector.empty,
      None,
      None,
      builder.selectColumns,
      builder.derivedSource,
    )

end WhereBuilder1

// ============================================================================
// WhereBuilder1Ready - Type-safe WHERE for chaining on Ready
// ============================================================================

/** Builder for chaining additional WHERE conditions on Query1Ready. */
final case class WhereBuilder1Ready[A: Table, T](
    ready: Query1Ready[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[Query1Ready[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): Query1Ready[A] =
    ready.copy(wherePredicates = ready.wherePredicates :+ predicate)

end WhereBuilder1Ready

// ============================================================================
// JoinBuilder1 - Building ON clause for first join
// ============================================================================

final case class JoinBuilder1[A: Table, B: Table](
    query: Query1Builder[A],
    joinType: JoinType,
):
  /** Start a type-safe ON condition by selecting a column from the left table (A). */
  inline def on[T](inline selector: A => T): OnBuilder1[A, B, T] =
    val bInstance = query.gen.aliasedInstance[B]
    val leftCol   = query.baseInstance.column(selector)
    val leftAlias = query.baseInstance.alias.getOrElse(Alias.unsafe(query.baseInstance.tableName))
    OnBuilder1(query, joinType, leftAlias, leftCol, bInstance.alias.get.asInstanceOf[Alias], bInstance)
end JoinBuilder1

// ============================================================================
// OnBuilder1 - Building ON condition for first join
// ============================================================================

/** Builder for the first ON condition - awaiting the right-hand side. */
final case class OnBuilder1[A: Table, B: Table, T](
    query: Query1Builder[A],
    joinType: JoinType,
    leftAlias: Alias,
    leftColumn: Column[T],
    rightAlias: Alias,
    rightInstance: Instance[B],
):
  private def complete(operator: Operator, rightCol: Column[?]): OnChain1[A, B] =
    val condition = BinaryCondition(leftAlias, leftColumn, operator, rightAlias, rightCol)
    OnChain1(query, joinType, leftAlias, rightAlias, rightInstance, Vector(condition))

  /** Complete with equality (=) */
  inline def eq(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Eq, rightInstance.column(selector))

  /** Complete with not equal (<>) */
  inline def neq(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Neq, rightInstance.column(selector))

  /** Complete with less than (<) */
  inline def lt(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Lt, rightInstance.column(selector))

  /** Complete with less than or equal (<=) */
  inline def lte(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Lte, rightInstance.column(selector))

  /** Complete with greater than (>) */
  inline def gt(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Gt, rightInstance.column(selector))

  /** Complete with greater than or equal (>=) */
  inline def gte(inline selector: B => T): OnChain1[A, B] =
    complete(Operator.Gte, rightInstance.column(selector))

  /** Complete with custom operator */
  inline def op(operator: Operator)(inline selector: B => T): OnChain1[A, B] =
    complete(operator, rightInstance.column(selector))

  /** Complete with IS NULL (unary) */
  def isNull(): OnChain1[A, B] =
    val condition = UnaryCondition(leftAlias, leftColumn, Operator.IsNull)
    OnChain1(query, joinType, leftAlias, rightAlias, rightInstance, Vector(condition))

  /** Complete with IS NOT NULL (unary) */
  def isNotNull(): OnChain1[A, B] =
    val condition = UnaryCondition(leftAlias, leftColumn, Operator.IsNotNull)
    OnChain1(query, joinType, leftAlias, rightAlias, rightInstance, Vector(condition))

end OnBuilder1

// ============================================================================
// OnChain1 - Chaining ON conditions and finalizing to Query2Builder
// ============================================================================

/** Chain state after first ON condition - allows .and() or finalization. */
final case class OnChain1[A: Table, B: Table](
    query: Query1Builder[A],
    joinType: JoinType,
    leftAlias: Alias,
    rightAlias: Alias,
    rightInstance: Instance[B],
    conditions: Vector[Condition],
):
  /** Add another ON condition from the left table (A) */
  inline def and[T2](inline selector: A => T2): OnAndBuilder1[A, B, T2, A] =
    val col = query.baseInstance.column(selector)
    OnAndBuilder1(this, leftAlias, col)

  /** Add another ON condition from the right table (B) */
  inline def andRight[T2](inline selector: B => T2): OnAndBuilder1[A, B, T2, B] =
    val col = rightInstance.column(selector)
    OnAndBuilder1(this, rightAlias, col)

  /** Finalize the JOIN and create Query2Builder */
  def endJoin: Query2Builder[A, B] =
    val bTable        = summon[Table[B]]
    val conditionFrag = Condition.toSqlFragment(conditions)
    Query2Builder(
      query.gen,
      query.baseInstance,
      rightInstance,
      Vector(JoinClause(bTable.name, rightAlias, joinType, conditionFrag)),
      query.sorts,
      query.derivedSource,
    )
  end endJoin

  // Convenience methods that implicitly finalize

  def orderBy(sort: Sort[?]): Query2Builder[A, B]      = endJoin.orderBy(sort)
  def limit(n: Int): Query2Ready[A, B]                 = endJoin.limit(n)
  def offset(n: Long): Query2Builder[A, B]             = endJoin.offset(n)
  def where(predicate: SqlFragment): Query2Ready[A, B] = endJoin.where(predicate)
  def all: Query2Ready[A, B]                           = endJoin.all

  inline def where[T](inline selector: A => T): WhereBuilder2[A, B, T] =
    val alias = query.baseInstance.alias.getOrElse(Alias.unsafe(query.baseInstance.tableName))
    val col   = query.baseInstance.column(selector)
    WhereBuilder2(endJoin, alias, col)

  inline def whereFrom[T](inline selector: B => T): WhereBuilder2[A, B, T] =
    val alias = rightInstance.alias.getOrElse(Alias.unsafe(rightInstance.tableName))
    val col   = rightInstance.column(selector)
    WhereBuilder2(endJoin, alias, col)

  def innerJoin[C: Table]: JoinBuilder2[A, B, C] = endJoin.innerJoin[C]
  def leftJoin[C: Table]: JoinBuilder2[A, B, C]  = endJoin.leftJoin[C]
  def rightJoin[C: Table]: JoinBuilder2[A, B, C] = endJoin.rightJoin[C]
  def fullJoin[C: Table]: JoinBuilder2[A, B, C]  = endJoin.fullJoin[C]

end OnChain1

// ============================================================================
// OnAndBuilder1 - Building chained ON condition
// ============================================================================

final case class OnAndBuilder1[A: Table, B: Table, T, From](
    chain: OnChain1[A, B],
    fromAlias: Alias,
    fromColumn: Column[T],
):
  private def completeWith(operator: Operator, toAlias: Alias, toCol: Column[?]): OnChain1[A, B] =
    val condition = BinaryCondition(fromAlias, fromColumn, operator, toAlias, toCol)
    chain.copy(conditions = chain.conditions :+ condition)

  /** Complete comparing to right table */
  inline def eq(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Eq, chain.rightAlias, chain.rightInstance.column(selector))

  inline def neq(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Neq, chain.rightAlias, chain.rightInstance.column(selector))

  inline def lt(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Lt, chain.rightAlias, chain.rightInstance.column(selector))

  inline def lte(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Lte, chain.rightAlias, chain.rightInstance.column(selector))

  inline def gt(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Gt, chain.rightAlias, chain.rightInstance.column(selector))

  inline def gte(inline selector: B => T): OnChain1[A, B] =
    completeWith(Operator.Gte, chain.rightAlias, chain.rightInstance.column(selector))

  /** Complete comparing to left table */
  inline def eqLeft(inline selector: A => T): OnChain1[A, B] =
    completeWith(Operator.Eq, chain.leftAlias, chain.query.baseInstance.column(selector))

  def isNull(): OnChain1[A, B] =
    val condition = UnaryCondition(fromAlias, fromColumn, Operator.IsNull)
    chain.copy(conditions = chain.conditions :+ condition)

  def isNotNull(): OnChain1[A, B] =
    val condition = UnaryCondition(fromAlias, fromColumn, Operator.IsNotNull)
    chain.copy(conditions = chain.conditions :+ condition)

end OnAndBuilder1

// ============================================================================
// Query2Builder - Two tables joined (not yet ready to execute)
// ============================================================================

final case class Query2Builder[A: Table, B: Table](
    private[saferis] val gen: AliasGenerator,
    t1: Instance[A],
    t2: Instance[B],
    joins: Vector[JoinClause],
    sorts: Vector[Sort[?]] = Vector.empty,
    derivedSource: Option[DerivedSource] = None,
):
  // === ORDER BY Methods ===

  def orderBy(sort: Sort[?]): Query2Builder[A, B] =
    copy(sorts = sorts :+ sort)

  def orderBy(sort: Sort[?], moreSorts: Sort[?]*): Query2Builder[A, B] =
    copy(sorts = sorts ++ (sort +: moreSorts))

  // === OFFSET (stays on Builder - still needs constraint) ===

  def offset(@scala.annotation.unused n: Long): Query2Builder[A, B] = copy() // Offset alone doesn't make safe

  // === JOIN Methods ===

  def innerJoin[C: Table]: JoinBuilder2[A, B, C] = JoinBuilder2(this, JoinType.Inner)
  def leftJoin[C: Table]: JoinBuilder2[A, B, C]  = JoinBuilder2(this, JoinType.Left)
  def rightJoin[C: Table]: JoinBuilder2[A, B, C] = JoinBuilder2(this, JoinType.Right)
  def fullJoin[C: Table]: JoinBuilder2[A, B, C]  = JoinBuilder2(this, JoinType.Full)

  // === WHERE Methods (transition to Ready) ===

  def where(predicate: SqlFragment): Query2Ready[A, B] =
    Query2Ready(t1, t2, joins, Vector(predicate), sorts, Vector.empty, None, None, derivedSource)

  inline def where[T](inline selector: A => T): WhereBuilder2[A, B, T] =
    val alias = t1.alias.getOrElse(Alias.unsafe(t1.tableName))
    val col   = t1.column(selector)
    WhereBuilder2(this, alias, col)

  inline def whereFrom[T](inline selector: B => T): WhereBuilder2[A, B, T] =
    val alias = t2.alias.getOrElse(Alias.unsafe(t2.tableName))
    val col   = t2.column(selector)
    WhereBuilder2(this, alias, col)

  // === LIMIT/SEEK Methods (transition to Ready) ===

  def limit(n: Int): Query2Ready[A, B] =
    Query2Ready(t1, t2, joins, Vector.empty, sorts, Vector.empty, Some(n), None, derivedSource)

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query2Ready[A, B] =
    Query2Ready(
      t1,
      t2,
      joins,
      Vector.empty,
      sorts,
      Vector(Seek(column, direction, value, sortOrder, nullOrder)),
      None,
      None,
      derivedSource,
    )

  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query2Ready[A, B] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def seekBefore[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Desc): Query2Ready[A, B] =
    seek(column, SeekDir.Lt, value, sortOrder)

  // === ALL Method ===

  def all: Query2Ready[A, B] =
    Query2Ready(t1, t2, joins, Vector.empty, sorts, Vector.empty, None, None, derivedSource)

end Query2Builder

// ============================================================================
// Query2Ready - Two tables joined, ready to execute
// ============================================================================

final case class Query2Ready[A: Table, B: Table](
    t1: Instance[A],
    t2: Instance[B],
    joins: Vector[JoinClause],
    wherePredicates: Vector[SqlFragment],
    sorts: Vector[Sort[?]],
    seeks: Vector[Seek[?]],
    limitValue: Option[Int],
    offsetValue: Option[Long],
    derivedSource: Option[DerivedSource],
) extends QueryBase:
  // === WHERE Methods ===

  def where(predicate: SqlFragment): Query2Ready[A, B] =
    copy(wherePredicates = wherePredicates :+ predicate)

  inline def where[T](inline selector: A => T): WhereBuilder2Ready[A, B, T] =
    val alias = t1.alias.getOrElse(Alias.unsafe(t1.tableName))
    val col   = t1.column(selector)
    WhereBuilder2Ready(this, alias, col)

  inline def whereFrom[T](inline selector: B => T): WhereBuilder2Ready[A, B, T] =
    val alias = t2.alias.getOrElse(Alias.unsafe(t2.tableName))
    val col   = t2.column(selector)
    WhereBuilder2Ready(this, alias, col)

  // === ORDER BY Methods ===

  def orderBy(sort: Sort[?]): Query2Ready[A, B] =
    copy(sorts = sorts :+ sort)

  def orderBy(sort: Sort[?], moreSorts: Sort[?]*): Query2Ready[A, B] =
    copy(sorts = sorts ++ (sort +: moreSorts))

  // === PAGINATION Methods ===

  def limit(n: Int): Query2Ready[A, B]   = copy(limitValue = Some(n))
  def offset(n: Long): Query2Ready[A, B] = copy(offsetValue = Some(n))

  // === SEEK Methods ===

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query2Ready[A, B] =
    copy(seeks = seeks :+ Seek(column, direction, value, sortOrder, nullOrder))

  def seek(s: Seek[?]): Query2Ready[A, B] = copy(seeks = seeks :+ s)

  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query2Ready[A, B] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def seekBefore[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Desc): Query2Ready[A, B] =
    seek(column, SeekDir.Lt, value, sortOrder)

  // === BUILD Methods ===

  def build: SqlFragment =
    // For derived tables, use (subquery) as alias; otherwise use table as alias
    val from = derivedSource match
      case Some(derived) =>
        SqlFragment.text("(").append(derived.subquery.build).append(SqlFragment.text(s") as ${derived.alias.value}"))
      case None =>
        val fromSqlPart = t1.alias.fold(t1.tableName)(a => s"${t1.tableName} as ${a.value}")
        SqlFragment.text(fromSqlPart)

    var result = SqlFragment.text("select * from ").append(from)

    // Add joins
    for join <- joins do
      val joinSql = s" ${join.joinType.toSql} ${join.tableName} as ${join.alias.value}"
      result = result :+ SqlFragment.text(joinSql)
      if join.condition.sql.nonEmpty then result = result :+ SqlFragment.text(" on ") :+ join.condition

    // WHERE clause
    val seekPredicates = seeks.map(_.toWherePredicate)
    val allPredicates  = (wherePredicates ++ seekPredicates).filter(_.sql.trim.nonEmpty)
    if allPredicates.nonEmpty then
      val joined = Placeholder.join(allPredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(joined)

    // ORDER BY clause
    val seekSorts = seeks.map(_.toSort)
    val allSorts  = sorts ++ seekSorts
    if allSorts.nonEmpty then
      val sortFragments = allSorts.map(_.toSqlFragment)
      val joined        = Placeholder.join(sortFragments, ", ")
      result = result :+ SqlFragment.text(" order by ") :+ SqlFragment(joined)

    // LIMIT/OFFSET
    limitValue.foreach(n => result = result :+ SqlFragment.text(s" limit $n"))
    offsetValue.foreach(n => result = result :+ SqlFragment.text(s" offset $n"))

    result
  end build

  inline def query[R: Table](using Trace): ZIO[SqlSession, SaferisError, Chunk[R]]     = build.query[R]
  inline def queryOne[R: Table](using Trace): ZIO[SqlSession, SaferisError, Option[R]] = build.queryOne[R]
  inline def queryStream[R: Table](using Trace): ZStream[SqlSession, SaferisError, R]  =
    build.queryStream[R]

end Query2Ready

// ============================================================================
// WhereBuilder2 - Type-safe WHERE for two-table queries (Builder -> Ready)
// ============================================================================

final case class WhereBuilder2[A: Table, B: Table, T](
    builder: Query2Builder[A, B],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[Query2Ready[A, B], T]:
  protected def whereAlias: Alias                                       = fromAlias
  protected def whereColumn: Column[T]                                  = fromColumn
  protected def addPredicate(predicate: SqlFragment): Query2Ready[A, B] =
    Query2Ready(
      builder.t1,
      builder.t2,
      builder.joins,
      Vector(predicate),
      builder.sorts,
      Vector.empty,
      None,
      None,
      builder.derivedSource,
    )

  // Column comparisons (specific to join queries)
  private def completeColumn(operator: Operator, toAlias: Alias, toCol: Column[?]): Query2Ready[A, B] =
    val condition = BinaryCondition(fromAlias, fromColumn, operator, toAlias, toCol)
    val whereFrag = Condition.toSqlFragment(Vector(condition))
    addPredicate(whereFrag)

  inline def eqCol(inline selector: B => T): Query2Ready[A, B] =
    val toAlias = builder.t2.alias.getOrElse(Alias.unsafe(builder.t2.tableName))
    val toCol   = builder.t2.column(selector)
    completeColumn(Operator.Eq, toAlias, toCol)

  inline def neqCol(inline selector: B => T): Query2Ready[A, B] =
    val toAlias = builder.t2.alias.getOrElse(Alias.unsafe(builder.t2.tableName))
    val toCol   = builder.t2.column(selector)
    completeColumn(Operator.Neq, toAlias, toCol)

end WhereBuilder2

// ============================================================================
// WhereBuilder2Ready - Type-safe WHERE for chaining on Ready
// ============================================================================

final case class WhereBuilder2Ready[A: Table, B: Table, T](
    ready: Query2Ready[A, B],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[Query2Ready[A, B], T]:
  protected def whereAlias: Alias                                       = fromAlias
  protected def whereColumn: Column[T]                                  = fromColumn
  protected def addPredicate(predicate: SqlFragment): Query2Ready[A, B] =
    ready.copy(wherePredicates = ready.wherePredicates :+ predicate)

  // Column comparisons
  private def completeColumn(operator: Operator, toAlias: Alias, toCol: Column[?]): Query2Ready[A, B] =
    val condition = BinaryCondition(fromAlias, fromColumn, operator, toAlias, toCol)
    val whereFrag = Condition.toSqlFragment(Vector(condition))
    addPredicate(whereFrag)

  inline def eqCol(inline selector: B => T): Query2Ready[A, B] =
    val toAlias = ready.t2.alias.getOrElse(Alias.unsafe(ready.t2.tableName))
    val toCol   = ready.t2.column(selector)
    completeColumn(Operator.Eq, toAlias, toCol)

  inline def neqCol(inline selector: B => T): Query2Ready[A, B] =
    val toAlias = ready.t2.alias.getOrElse(Alias.unsafe(ready.t2.tableName))
    val toCol   = ready.t2.column(selector)
    completeColumn(Operator.Neq, toAlias, toCol)

end WhereBuilder2Ready

// ============================================================================
// JoinBuilder2 - Building ON clause for second join
// ============================================================================

final case class JoinBuilder2[A: Table, B: Table, C: Table](
    query: Query2Builder[A, B],
    joinType: JoinType,
):
  /** ON condition from first table (A) */
  inline def on[T](inline selector: A => T): OnBuilder2[A, B, C, T, A] =
    val cInstance = query.gen.aliasedInstance[C]
    val leftCol   = query.t1.column(selector)
    val leftAlias = query.t1.alias.getOrElse(Alias.unsafe(query.t1.tableName))
    OnBuilder2(query, joinType, leftAlias, leftCol, cInstance.alias.get.asInstanceOf[Alias], cInstance)

  /** ON condition from second table (B) - the "previous" table */
  inline def onPrev[T](inline selector: B => T): OnBuilder2[A, B, C, T, B] =
    val cInstance = query.gen.aliasedInstance[C]
    val leftCol   = query.t2.column(selector)
    val leftAlias = query.t2.alias.getOrElse(Alias.unsafe(query.t2.tableName))
    OnBuilder2(query, joinType, leftAlias, leftCol, cInstance.alias.get.asInstanceOf[Alias], cInstance)

end JoinBuilder2

// ============================================================================
// OnBuilder2 - Building ON condition for second join
// ============================================================================

final case class OnBuilder2[A: Table, B: Table, C: Table, T, From](
    query: Query2Builder[A, B],
    joinType: JoinType,
    leftAlias: Alias,
    leftColumn: Column[T],
    rightAlias: Alias,
    rightInstance: Instance[C],
):
  private def complete(operator: Operator, rightCol: Column[?]): OnChain2[A, B, C] =
    val condition = BinaryCondition(leftAlias, leftColumn, operator, rightAlias, rightCol)
    OnChain2(query, joinType, rightAlias, rightInstance, Vector(condition))

  inline def eq(inline selector: C => T): OnChain2[A, B, C]  = complete(Operator.Eq, rightInstance.column(selector))
  inline def neq(inline selector: C => T): OnChain2[A, B, C] = complete(Operator.Neq, rightInstance.column(selector))
  inline def lt(inline selector: C => T): OnChain2[A, B, C]  = complete(Operator.Lt, rightInstance.column(selector))
  inline def lte(inline selector: C => T): OnChain2[A, B, C] = complete(Operator.Lte, rightInstance.column(selector))
  inline def gt(inline selector: C => T): OnChain2[A, B, C]  = complete(Operator.Gt, rightInstance.column(selector))
  inline def gte(inline selector: C => T): OnChain2[A, B, C] = complete(Operator.Gte, rightInstance.column(selector))
  inline def op(operator: Operator)(inline selector: C => T): OnChain2[A, B, C] =
    complete(operator, rightInstance.column(selector))

  def isNull(): OnChain2[A, B, C] =
    val condition = UnaryCondition(leftAlias, leftColumn, Operator.IsNull)
    OnChain2(query, joinType, rightAlias, rightInstance, Vector(condition))

  def isNotNull(): OnChain2[A, B, C] =
    val condition = UnaryCondition(leftAlias, leftColumn, Operator.IsNotNull)
    OnChain2(query, joinType, rightAlias, rightInstance, Vector(condition))

end OnBuilder2

// ============================================================================
// OnChain2 - Chaining ON conditions for second join
// ============================================================================

final case class OnChain2[A: Table, B: Table, C: Table](
    query: Query2Builder[A, B],
    joinType: JoinType,
    rightAlias: Alias,
    rightInstance: Instance[C],
    conditions: Vector[Condition],
):
  def endJoin: Query3Builder[A, B, C] =
    val cTable        = summon[Table[C]]
    val conditionFrag = Condition.toSqlFragment(conditions)
    Query3Builder(
      query.gen,
      query.t1,
      query.t2,
      rightInstance,
      query.joins :+ JoinClause(cTable.name, rightAlias, joinType, conditionFrag),
      query.sorts,
    )
  end endJoin

  def orderBy(sort: Sort[?]): Query3Builder[A, B, C]      = endJoin.orderBy(sort)
  def limit(n: Int): Query3Ready[A, B, C]                 = endJoin.limit(n)
  def offset(n: Long): Query3Builder[A, B, C]             = endJoin.offset(n)
  def where(predicate: SqlFragment): Query3Ready[A, B, C] = endJoin.where(predicate)
  def all: Query3Ready[A, B, C]                           = endJoin.all

end OnChain2

// ============================================================================
// Query3Builder - Three tables joined (not yet ready to execute)
// ============================================================================

final case class Query3Builder[A: Table, B: Table, C: Table](
    private[saferis] val gen: AliasGenerator,
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    joins: Vector[JoinClause],
    sorts: Vector[Sort[?]] = Vector.empty,
):
  def where(predicate: SqlFragment): Query3Ready[A, B, C] =
    Query3Ready(t1, t2, t3, joins, Vector(predicate), sorts, Vector.empty, None, None)
  def orderBy(sort: Sort[?]): Query3Builder[A, B, C]                   = copy(sorts = sorts :+ sort)
  def offset(@scala.annotation.unused n: Long): Query3Builder[A, B, C] = copy() // Offset alone doesn't make safe

  def limit(n: Int): Query3Ready[A, B, C] =
    Query3Ready(t1, t2, t3, joins, Vector.empty, sorts, Vector.empty, Some(n), None)

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query3Ready[A, B, C] =
    Query3Ready(
      t1,
      t2,
      t3,
      joins,
      Vector.empty,
      sorts,
      Vector(Seek(column, direction, value, sortOrder, nullOrder)),
      None,
      None,
    )

  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query3Ready[A, B, C] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def all: Query3Ready[A, B, C] =
    Query3Ready(t1, t2, t3, joins, Vector.empty, sorts, Vector.empty, None, None)

  // JOIN Methods
  def innerJoin[D: Table]: JoinBuilder3[A, B, C, D] = JoinBuilder3(this, JoinType.Inner)
  def leftJoin[D: Table]: JoinBuilder3[A, B, C, D]  = JoinBuilder3(this, JoinType.Left)
  def rightJoin[D: Table]: JoinBuilder3[A, B, C, D] = JoinBuilder3(this, JoinType.Right)
  def fullJoin[D: Table]: JoinBuilder3[A, B, C, D]  = JoinBuilder3(this, JoinType.Full)

end Query3Builder

// ============================================================================
// Query3Ready - Three tables joined, ready to execute
// ============================================================================

final case class Query3Ready[A: Table, B: Table, C: Table](
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    joins: Vector[JoinClause],
    wherePredicates: Vector[SqlFragment],
    sorts: Vector[Sort[?]],
    seeks: Vector[Seek[?]],
    limitValue: Option[Int],
    offsetValue: Option[Long],
) extends QueryBase:
  def where(predicate: SqlFragment): Query3Ready[A, B, C] = copy(wherePredicates = wherePredicates :+ predicate)
  def orderBy(sort: Sort[?]): Query3Ready[A, B, C]        = copy(sorts = sorts :+ sort)
  def limit(n: Int): Query3Ready[A, B, C]                 = copy(limitValue = Some(n))
  def offset(n: Long): Query3Ready[A, B, C]               = copy(offsetValue = Some(n))

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query3Ready[A, B, C] =
    copy(seeks = seeks :+ Seek(column, direction, value, sortOrder, nullOrder))

  def seekAfter[T: Encoder](column: Column[T], value: T, sortOrder: SortOrder = SortOrder.Asc): Query3Ready[A, B, C] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def build: SqlFragment =
    val t1SqlPart = t1.alias.fold(t1.tableName)(a => s"${t1.tableName} as ${a.value}")
    var result    = SqlFragment.text(s"select * from $t1SqlPart")

    for join <- joins do
      val joinSql = s" ${join.joinType.toSql} ${join.tableName} as ${join.alias.value}"
      result = result :+ SqlFragment.text(joinSql)
      if join.condition.sql.nonEmpty then result = result :+ SqlFragment.text(" on ") :+ join.condition

    val seekPredicates = seeks.map(_.toWherePredicate)
    val allPredicates  = (wherePredicates ++ seekPredicates).filter(_.sql.trim.nonEmpty)
    if allPredicates.nonEmpty then
      val joined = Placeholder.join(allPredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(joined)

    val seekSorts = seeks.map(_.toSort)
    val allSorts  = sorts ++ seekSorts
    if allSorts.nonEmpty then
      val sortFragments = allSorts.map(_.toSqlFragment)
      val joined        = Placeholder.join(sortFragments, ", ")
      result = result :+ SqlFragment.text(" order by ") :+ SqlFragment(joined)

    limitValue.foreach(n => result = result :+ SqlFragment.text(s" limit $n"))
    offsetValue.foreach(n => result = result :+ SqlFragment.text(s" offset $n"))

    result
  end build

  inline def query[R: Table](using Trace): ZIO[SqlSession, SaferisError, Chunk[R]]     = build.query[R]
  inline def queryOne[R: Table](using Trace): ZIO[SqlSession, SaferisError, Option[R]] = build.queryOne[R]
  inline def queryStream[R: Table](using Trace): ZStream[SqlSession, SaferisError, R]  =
    build.queryStream[R]

end Query3Ready

// ============================================================================
// JoinBuilder3 - Building ON clause for third join
// ============================================================================

final case class JoinBuilder3[A: Table, B: Table, C: Table, D: Table](
    query: Query3Builder[A, B, C],
    joinType: JoinType,
):
  inline def on[T](inline selector: A => T): OnBuilder3[A, B, C, D, T] =
    val dInstance = query.gen.aliasedInstance[D]
    val leftCol   = query.t1.column(selector)
    val leftAlias = query.t1.alias.getOrElse(Alias.unsafe(query.t1.tableName))
    OnBuilder3(query, joinType, leftAlias, leftCol, dInstance.alias.get.asInstanceOf[Alias], dInstance)

  inline def onPrev[T](inline selector: C => T): OnBuilder3[A, B, C, D, T] =
    val dInstance = query.gen.aliasedInstance[D]
    val leftCol   = query.t3.column(selector)
    val leftAlias = query.t3.alias.getOrElse(Alias.unsafe(query.t3.tableName))
    OnBuilder3(query, joinType, leftAlias, leftCol, dInstance.alias.get.asInstanceOf[Alias], dInstance)

end JoinBuilder3

final case class OnBuilder3[A: Table, B: Table, C: Table, D: Table, T](
    query: Query3Builder[A, B, C],
    joinType: JoinType,
    leftAlias: Alias,
    leftColumn: Column[T],
    rightAlias: Alias,
    rightInstance: Instance[D],
):
  private def complete(operator: Operator, rightCol: Column[?]): OnChain3[A, B, C, D] =
    val condition = BinaryCondition(leftAlias, leftColumn, operator, rightAlias, rightCol)
    OnChain3(query, joinType, rightAlias, rightInstance, Vector(condition))

  inline def eq(inline selector: D => T): OnChain3[A, B, C, D]  = complete(Operator.Eq, rightInstance.column(selector))
  inline def neq(inline selector: D => T): OnChain3[A, B, C, D] = complete(Operator.Neq, rightInstance.column(selector))
  inline def lt(inline selector: D => T): OnChain3[A, B, C, D]  = complete(Operator.Lt, rightInstance.column(selector))
  inline def lte(inline selector: D => T): OnChain3[A, B, C, D] = complete(Operator.Lte, rightInstance.column(selector))
  inline def gt(inline selector: D => T): OnChain3[A, B, C, D]  = complete(Operator.Gt, rightInstance.column(selector))
  inline def gte(inline selector: D => T): OnChain3[A, B, C, D] = complete(Operator.Gte, rightInstance.column(selector))

end OnBuilder3

final case class OnChain3[A: Table, B: Table, C: Table, D: Table](
    query: Query3Builder[A, B, C],
    joinType: JoinType,
    rightAlias: Alias,
    rightInstance: Instance[D],
    conditions: Vector[Condition],
):
  def endJoin: Query4Builder[A, B, C, D] =
    val dTable        = summon[Table[D]]
    val conditionFrag = Condition.toSqlFragment(conditions)
    Query4Builder(
      query.gen,
      query.t1,
      query.t2,
      query.t3,
      rightInstance,
      query.joins :+ JoinClause(dTable.name, rightAlias, joinType, conditionFrag),
      query.sorts,
    )
  end endJoin

  def orderBy(sort: Sort[?]): Query4Builder[A, B, C, D]      = endJoin.orderBy(sort)
  def limit(n: Int): Query4Ready[A, B, C, D]                 = endJoin.limit(n)
  def offset(n: Long): Query4Builder[A, B, C, D]             = endJoin.offset(n)
  def where(predicate: SqlFragment): Query4Ready[A, B, C, D] = endJoin.where(predicate)
  def all: Query4Ready[A, B, C, D]                           = endJoin.all

end OnChain3

// ============================================================================
// Query4Builder - Four tables joined (not yet ready to execute)
// ============================================================================

final case class Query4Builder[A: Table, B: Table, C: Table, D: Table](
    private[saferis] val gen: AliasGenerator,
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    t4: Instance[D],
    joins: Vector[JoinClause],
    sorts: Vector[Sort[?]] = Vector.empty,
):
  def where(predicate: SqlFragment): Query4Ready[A, B, C, D] =
    Query4Ready(t1, t2, t3, t4, joins, Vector(predicate), sorts, Vector.empty, None, None)
  def orderBy(sort: Sort[?]): Query4Builder[A, B, C, D]                   = copy(sorts = sorts :+ sort)
  def offset(@scala.annotation.unused n: Long): Query4Builder[A, B, C, D] = copy()

  def limit(n: Int): Query4Ready[A, B, C, D] =
    Query4Ready(t1, t2, t3, t4, joins, Vector.empty, sorts, Vector.empty, Some(n), None)

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query4Ready[A, B, C, D] =
    Query4Ready(
      t1,
      t2,
      t3,
      t4,
      joins,
      Vector.empty,
      sorts,
      Vector(Seek(column, direction, value, sortOrder, nullOrder)),
      None,
      None,
    )

  def seekAfter[T: Encoder](
      column: Column[T],
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
  ): Query4Ready[A, B, C, D] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def all: Query4Ready[A, B, C, D] =
    Query4Ready(t1, t2, t3, t4, joins, Vector.empty, sorts, Vector.empty, None, None)

  def innerJoin[E: Table]: JoinBuilder4[A, B, C, D, E] = JoinBuilder4(this, JoinType.Inner)
  def leftJoin[E: Table]: JoinBuilder4[A, B, C, D, E]  = JoinBuilder4(this, JoinType.Left)
  def rightJoin[E: Table]: JoinBuilder4[A, B, C, D, E] = JoinBuilder4(this, JoinType.Right)
  def fullJoin[E: Table]: JoinBuilder4[A, B, C, D, E]  = JoinBuilder4(this, JoinType.Full)

end Query4Builder

// ============================================================================
// Query4Ready - Four tables joined, ready to execute
// ============================================================================

final case class Query4Ready[A: Table, B: Table, C: Table, D: Table](
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    t4: Instance[D],
    joins: Vector[JoinClause],
    wherePredicates: Vector[SqlFragment],
    sorts: Vector[Sort[?]],
    seeks: Vector[Seek[?]],
    limitValue: Option[Int],
    offsetValue: Option[Long],
) extends QueryBase:
  def where(predicate: SqlFragment): Query4Ready[A, B, C, D] = copy(wherePredicates = wherePredicates :+ predicate)
  def orderBy(sort: Sort[?]): Query4Ready[A, B, C, D]        = copy(sorts = sorts :+ sort)
  def limit(n: Int): Query4Ready[A, B, C, D]                 = copy(limitValue = Some(n))
  def offset(n: Long): Query4Ready[A, B, C, D]               = copy(offsetValue = Some(n))

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query4Ready[A, B, C, D] =
    copy(seeks = seeks :+ Seek(column, direction, value, sortOrder, nullOrder))

  def seekAfter[T: Encoder](
      column: Column[T],
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
  ): Query4Ready[A, B, C, D] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def build: SqlFragment =
    val t1SqlPart = t1.alias.fold(t1.tableName)(a => s"${t1.tableName} as ${a.value}")
    var result    = SqlFragment.text(s"select * from $t1SqlPart")

    for join <- joins do
      val joinSql = s" ${join.joinType.toSql} ${join.tableName} as ${join.alias.value}"
      result = result :+ SqlFragment.text(joinSql)
      if join.condition.sql.nonEmpty then result = result :+ SqlFragment.text(" on ") :+ join.condition

    val seekPredicates = seeks.map(_.toWherePredicate)
    val allPredicates  = (wherePredicates ++ seekPredicates).filter(_.sql.trim.nonEmpty)
    if allPredicates.nonEmpty then
      val joined = Placeholder.join(allPredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(joined)

    val seekSorts = seeks.map(_.toSort)
    val allSorts  = sorts ++ seekSorts
    if allSorts.nonEmpty then
      val sortFragments = allSorts.map(_.toSqlFragment)
      val joined        = Placeholder.join(sortFragments, ", ")
      result = result :+ SqlFragment.text(" order by ") :+ SqlFragment(joined)

    limitValue.foreach(n => result = result :+ SqlFragment.text(s" limit $n"))
    offsetValue.foreach(n => result = result :+ SqlFragment.text(s" offset $n"))

    result
  end build

  inline def query[R: Table](using Trace): ZIO[SqlSession, SaferisError, Chunk[R]]     = build.query[R]
  inline def queryOne[R: Table](using Trace): ZIO[SqlSession, SaferisError, Option[R]] = build.queryOne[R]
  inline def queryStream[R: Table](using Trace): ZStream[SqlSession, SaferisError, R]  =
    build.queryStream[R]

end Query4Ready

// ============================================================================
// JoinBuilder4 - Building ON clause for fourth join
// ============================================================================

final case class JoinBuilder4[
    A: Table,
    B: Table,
    C: Table,
    D: Table,
    E: Table,
](
    query: Query4Builder[A, B, C, D],
    joinType: JoinType,
):
  inline def on[T](inline selector: A => T): OnBuilder4[A, B, C, D, E, T] =
    val eInstance = query.gen.aliasedInstance[E]
    val leftCol   = query.t1.column(selector)
    val leftAlias = query.t1.alias.getOrElse(Alias.unsafe(query.t1.tableName))
    OnBuilder4(query, joinType, leftAlias, leftCol, eInstance.alias.get.asInstanceOf[Alias], eInstance)

  inline def onPrev[T](inline selector: D => T): OnBuilder4[A, B, C, D, E, T] =
    val eInstance = query.gen.aliasedInstance[E]
    val leftCol   = query.t4.column(selector)
    val leftAlias = query.t4.alias.getOrElse(Alias.unsafe(query.t4.tableName))
    OnBuilder4(query, joinType, leftAlias, leftCol, eInstance.alias.get.asInstanceOf[Alias], eInstance)

end JoinBuilder4

final case class OnBuilder4[
    A: Table,
    B: Table,
    C: Table,
    D: Table,
    E: Table,
    T,
](
    query: Query4Builder[A, B, C, D],
    joinType: JoinType,
    leftAlias: Alias,
    leftColumn: Column[T],
    rightAlias: Alias,
    rightInstance: Instance[E],
):
  private def complete(operator: Operator, rightCol: Column[?]): OnChain4[A, B, C, D, E] =
    val condition = BinaryCondition(leftAlias, leftColumn, operator, rightAlias, rightCol)
    OnChain4(query, joinType, rightAlias, rightInstance, Vector(condition))

  inline def eq(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Eq, rightInstance.column(selector))
  inline def neq(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Neq, rightInstance.column(selector))
  inline def lt(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Lt, rightInstance.column(selector))
  inline def lte(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Lte, rightInstance.column(selector))
  inline def gt(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Gt, rightInstance.column(selector))
  inline def gte(inline selector: E => T): OnChain4[A, B, C, D, E] =
    complete(Operator.Gte, rightInstance.column(selector))

end OnBuilder4

final case class OnChain4[
    A: Table,
    B: Table,
    C: Table,
    D: Table,
    E: Table,
](
    query: Query4Builder[A, B, C, D],
    joinType: JoinType,
    rightAlias: Alias,
    rightInstance: Instance[E],
    conditions: Vector[Condition],
):
  def endJoin: Query5Builder[A, B, C, D, E] =
    val eTable        = summon[Table[E]]
    val conditionFrag = Condition.toSqlFragment(conditions)
    Query5Builder(
      query.gen,
      query.t1,
      query.t2,
      query.t3,
      query.t4,
      rightInstance,
      query.joins :+ JoinClause(eTable.name, rightAlias, joinType, conditionFrag),
      query.sorts,
    )
  end endJoin

  def orderBy(sort: Sort[?]): Query5Builder[A, B, C, D, E]      = endJoin.orderBy(sort)
  def limit(n: Int): Query5Ready[A, B, C, D, E]                 = endJoin.limit(n)
  def offset(n: Long): Query5Builder[A, B, C, D, E]             = endJoin.offset(n)
  def where(predicate: SqlFragment): Query5Ready[A, B, C, D, E] = endJoin.where(predicate)
  def all: Query5Ready[A, B, C, D, E]                           = endJoin.all

end OnChain4

// ============================================================================
// Query5Builder - Five tables joined (not yet ready to execute)
// ============================================================================

final case class Query5Builder[
    A: Table,
    B: Table,
    C: Table,
    D: Table,
    E: Table,
](
    private[saferis] val gen: AliasGenerator,
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    t4: Instance[D],
    t5: Instance[E],
    joins: Vector[JoinClause],
    sorts: Vector[Sort[?]] = Vector.empty,
):
  def where(predicate: SqlFragment): Query5Ready[A, B, C, D, E] =
    Query5Ready(t1, t2, t3, t4, t5, joins, Vector(predicate), sorts, Vector.empty, None, None)
  def orderBy(sort: Sort[?]): Query5Builder[A, B, C, D, E]                   = copy(sorts = sorts :+ sort)
  def offset(@scala.annotation.unused n: Long): Query5Builder[A, B, C, D, E] = copy()

  def limit(n: Int): Query5Ready[A, B, C, D, E] =
    Query5Ready(t1, t2, t3, t4, t5, joins, Vector.empty, sorts, Vector.empty, Some(n), None)

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query5Ready[A, B, C, D, E] =
    Query5Ready(
      t1,
      t2,
      t3,
      t4,
      t5,
      joins,
      Vector.empty,
      sorts,
      Vector(Seek(column, direction, value, sortOrder, nullOrder)),
      None,
      None,
    )

  def seekAfter[T: Encoder](
      column: Column[T],
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
  ): Query5Ready[A, B, C, D, E] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def all: Query5Ready[A, B, C, D, E] =
    Query5Ready(t1, t2, t3, t4, t5, joins, Vector.empty, sorts, Vector.empty, None, None)

end Query5Builder

// ============================================================================
// Query5Ready - Five tables joined, ready to execute
// ============================================================================

final case class Query5Ready[
    A: Table,
    B: Table,
    C: Table,
    D: Table,
    E: Table,
](
    t1: Instance[A],
    t2: Instance[B],
    t3: Instance[C],
    t4: Instance[D],
    t5: Instance[E],
    joins: Vector[JoinClause],
    wherePredicates: Vector[SqlFragment],
    sorts: Vector[Sort[?]],
    seeks: Vector[Seek[?]],
    limitValue: Option[Int],
    offsetValue: Option[Long],
) extends QueryBase:
  def where(predicate: SqlFragment): Query5Ready[A, B, C, D, E] = copy(wherePredicates = wherePredicates :+ predicate)
  def orderBy(sort: Sort[?]): Query5Ready[A, B, C, D, E]        = copy(sorts = sorts :+ sort)
  def limit(n: Int): Query5Ready[A, B, C, D, E]                 = copy(limitValue = Some(n))
  def offset(n: Long): Query5Ready[A, B, C, D, E]               = copy(offsetValue = Some(n))

  def seek[T: Encoder](
      column: Column[T],
      direction: SeekDir,
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
      nullOrder: NullOrder = NullOrder.Default,
  ): Query5Ready[A, B, C, D, E] =
    copy(seeks = seeks :+ Seek(column, direction, value, sortOrder, nullOrder))

  def seekAfter[T: Encoder](
      column: Column[T],
      value: T,
      sortOrder: SortOrder = SortOrder.Asc,
  ): Query5Ready[A, B, C, D, E] =
    seek(column, SeekDir.Gt, value, sortOrder)

  def build: SqlFragment =
    val t1SqlPart = t1.alias.fold(t1.tableName)(a => s"${t1.tableName} as ${a.value}")
    var result    = SqlFragment.text(s"select * from $t1SqlPart")

    for join <- joins do
      val joinSql = s" ${join.joinType.toSql} ${join.tableName} as ${join.alias.value}"
      result = result :+ SqlFragment.text(joinSql)
      if join.condition.sql.nonEmpty then result = result :+ SqlFragment.text(" on ") :+ join.condition

    val seekPredicates = seeks.map(_.toWherePredicate)
    val allPredicates  = (wherePredicates ++ seekPredicates).filter(_.sql.trim.nonEmpty)
    if allPredicates.nonEmpty then
      val joined = Placeholder.join(allPredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(joined)

    val seekSorts = seeks.map(_.toSort)
    val allSorts  = sorts ++ seekSorts
    if allSorts.nonEmpty then
      val sortFragments = allSorts.map(_.toSqlFragment)
      val joined        = Placeholder.join(sortFragments, ", ")
      result = result :+ SqlFragment.text(" order by ") :+ SqlFragment(joined)

    limitValue.foreach(n => result = result :+ SqlFragment.text(s" limit $n"))
    offsetValue.foreach(n => result = result :+ SqlFragment.text(s" offset $n"))

    result
  end build

  inline def query[R: Table](using Trace): ZIO[SqlSession, SaferisError, Chunk[R]]     = build.query[R]
  inline def queryOne[R: Table](using Trace): ZIO[SqlSession, SaferisError, Option[R]] = build.queryOne[R]
  inline def queryStream[R: Table](using Trace): ZStream[SqlSession, SaferisError, R]  =
    build.queryStream[R]

end Query5Ready

// ============================================================================
// Query Companion - Entry Point
// ============================================================================

object Query:
  /** Create a Query starting with a single table.
    *
    * This returns a Query1Builder which does NOT have execution methods (.query, .queryOne). You must constrain the
    * query by calling .where(), .limit(), .seekAfter(), or .all to get a Query1Ready which can be executed.
    *
    * Usage:
    * {{{
    *   Query[User]
    *     .where(_.name).eq("Alice")  // Now Query1Ready
    *     .innerJoin[Order].on(_.id).eq(_.userId)
    *     .query[UserWithOrder]
    * }}}
    */
  inline def apply[A: Table]: Query1Builder[A] =
    val gen      = AliasGenerator.create()
    val instance = gen.aliasedInstance[A]
    Query1Builder(gen, instance)

  /** Create a Query from a derived table (subquery in FROM clause).
    *
    * The result type `A` of the SelectQuery must be a case class that `derives Table` and matches the shape of the
    * subquery result.
    *
    * Usage:
    * {{{
    *   // Define a virtual type for the subquery result
    *   case class PaidOrderSummary(userId: Int, total: BigDecimal) derives Table
    *
    *   // Create typed subquery
    *   val summary = Query[Order]
    *     .where(_.status).eq("paid")
    *     .selectAll[PaidOrderSummary]
    *
    *   // Use as table source with explicit alias
    *   Query.from(summary, "paid_summary")
    *     .innerJoin[User].on(_.userId).eq(_.id)
    *     .where(_.total).gt(100)
    *     .query[UserWithTotal]
    * }}}
    */
  inline def from[A: Table](subquery: SelectQuery[A], inline alias: String): Query1Builder[A] =
    val gen       = AliasGenerator.create()
    val userAlias = Alias(alias)
    val table     = summon[Table[A]]
    val columns   = table.columns.map(_.withTableAlias(Some(userAlias)))
    val instance  = Instance[A](table.name, columns, Some(userAlias), Vector.empty)
    Query1Builder(gen, instance, derivedSource = Some(DerivedSource(subquery, userAlias)))

end Query
