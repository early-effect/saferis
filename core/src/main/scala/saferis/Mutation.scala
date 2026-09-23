package saferis

import zio.Chunk
import zio.Trace
import zio.ZIO
import zio.stream.ZStream

// ============================================================================
// Clause types for INSERT and UPDATE
// ============================================================================

/** A column/value pair for INSERT statements */
final case class ValueClause(columnLabel: String, value: SqlValue)

/** A column/value pair for UPDATE SET clauses */
final case class SetClause(columnLabel: String, value: SqlValue)

// ============================================================================
// ReturningQuery - Type-safe wrapper for mutations with RETURNING
// ============================================================================

/** A mutation query with RETURNING clause, ready to execute with type safety.
  *
  * Created by calling `.returningAs` on UpdateReady or DeleteReady when a ReturningSupport dialect is in scope.
  *
  * Usage:
  * {{{
  *   given dialect: Dialect & ReturningSupport = PostgresDialect
  *
  *   Update[User]
  *     .set(_.status, "active")
  *     .where(_.id).eq(123)
  *     .returningAs
  *     .queryOne[User]  // Option[User]
  * }}}
  */
final case class ReturningQuery[A: Table](fragment: SqlFragment):
  /** Execute query and return all matching rows */
  inline def query(using Trace): ZIO[SqlSession, SaferisError, Chunk[A]] = fragment.query[A]

  /** Execute query and return the first row (if any) */
  inline def queryOne(using Trace): ZIO[SqlSession, SaferisError, Option[A]] = fragment.queryOne[A]

  /** Execute query and stream all matching rows lazily */
  inline def queryStream(using Trace): ZStream[SqlSession, SaferisError, A] = fragment.queryStream[A]

  /** Get the underlying SQL fragment */
  def build: SqlFragment = fragment
end ReturningQuery

// ============================================================================
// Insert Builder
// ============================================================================

/** Type-safe INSERT builder.
  *
  * Usage:
  * {{{
  *   Insert[User]
  *     .value(_.name, "Alice")
  *     .value(_.age, 30)
  *     .build
  *     .execute
  * }}}
  */
final case class Insert[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val values: Vector[ValueClause] = Vector.empty,
):
  /** Add a column/value pair to the INSERT */
  inline def value[T](inline selector: A => T, v: T)(using enc: Encoder[T]): Insert[A] =
    val fieldName   = Macros.extractFieldName[A, T](selector)
    val columnLabel = fieldNamesToColumns(fieldName).label
    val encoded     = enc.encode(v)
    copy(values = values :+ ValueClause(columnLabel, encoded))

  /** Build the INSERT SQL fragment */
  def build: SqlFragment =
    require(values.nonEmpty, "INSERT requires at least one value")
    val columns = values.map(_.columnLabel).mkString(", ")
    val params  = values.map(clause => SqlFragment.param(clause.value))
    val body    = params.reduce((left, right) => left.append(SqlFragment.text(", ")).append(right))
    SqlFragment.text(s"insert into $tableName ($columns) values (").append(body).append(SqlFragment.text(")"))

  /** Build INSERT with RETURNING clause (for dialects that support it) */
  def returning: SqlFragment =
    build :+ SqlFragment.text(" returning *")

end Insert

object Insert:
  /** Create an Insert builder for a table type */
  def apply[A: Table]: Insert[A] =
    val table = summon[Table[A]]
    Insert(table.name, table.columnMap)

// ============================================================================
// Update Builder (not yet ready to build)
// ============================================================================

/** Type-safe UPDATE builder - accumulates SET clauses.
  *
  * This builder does NOT have a `.build` method. You must call `.where()` or `.all` first to get an `UpdateReady` which
  * can be built.
  *
  * Usage:
  * {{{
  *   Update[User]
  *     .set(_.name, "Alice Updated")
  *     .set(_.age, 31)
  *     .where(_.id).eq(123)  // Returns UpdateReady
  *     .build
  *     .execute
  * }}}
  */
final case class UpdateBuilder[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val setClauses: Vector[SetClause] = Vector.empty,
):
  /** Add a SET clause */
  inline def set[T](inline selector: A => T, v: T)(using enc: Encoder[T]): UpdateBuilder[A] =
    val fieldName   = Macros.extractFieldName[A, T](selector)
    val columnLabel = fieldNamesToColumns(fieldName).label
    val encoded     = enc.encode(v)
    copy(setClauses = setClauses :+ SetClause(columnLabel, encoded))

  /** Start a type-safe WHERE condition by selecting a column */
  inline def where[T](inline selector: A => T): UpdateWhereBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    UpdateWhereBuilder(this, Alias.unsafe(tableName), col)

  /** Add a WHERE predicate using SqlFragment */
  def where(predicate: SqlFragment): UpdateReady[A] =
    UpdateReady(tableName, fieldNamesToColumns, setClauses, Vector(predicate))

  /** Explicitly mark this as updating all rows (no WHERE).
    *
    * This is required to prevent accidental updates of all rows.
    */
  def all: UpdateReady[A] =
    UpdateReady(tableName, fieldNamesToColumns, setClauses, Vector.empty)

end UpdateBuilder

// ============================================================================
// Update Ready (has WHERE or explicit .all - ready to build)
// ============================================================================

/** UPDATE that is ready to build - has WHERE clause or explicit `.all`.
  *
  * This type has `.build` and `.returning` methods.
  */
final case class UpdateReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val setClauses: Vector[SetClause],
    private[saferis] val wherePredicates: Vector[SqlFragment],
):
  /** Chain another type-safe WHERE condition */
  inline def where[T](inline selector: A => T): UpdateReadyWhereBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    UpdateReadyWhereBuilder(this, Alias.unsafe(tableName), col)

  /** Add a WHERE predicate using SqlFragment */
  def where(predicate: SqlFragment): UpdateReady[A] =
    copy(wherePredicates = wherePredicates :+ predicate)

  /** Add a grouped WHERE condition using lambda syntax for complex OR/AND expressions.
    *
    * Usage:
    * {{{
    *   Update[User]
    *     .set(_.status, "claimed")
    *     .where(_.id).eq(123)
    *     .andWhere(w => w(_.claimedBy).isNull.or(_.claimedUntil).lt(now))
    *     .build
    * }}}
    *
    * Generates: `UPDATE users SET status = ? WHERE id = ? AND (claimed_by IS NULL OR claimed_until < ?)`
    */
  def andWhere(builder: WhereGroupBuilder[A] => WhereGroupChain[A]): UpdateReady[A] =
    val groupBuilder = WhereGroupBuilder[A](fieldNamesToColumns, Alias.unsafe(tableName))
    val chain        = builder(groupBuilder)
    val whereFrag    = chain.toWhereGroup.toSqlFragment
    copy(wherePredicates = wherePredicates :+ whereFrag)

  /** Build the UPDATE SQL fragment */
  def build: SqlFragment =
    require(setClauses.nonEmpty, "UPDATE requires at least one SET clause")
    val sets = setClauses.map: clause =>
      SqlFragment.text(s"${clause.columnLabel} = ").append(SqlFragment.param(clause.value))
    val setFrag = sets.reduce((left, right) => left.append(SqlFragment.text(", ")).append(right))
    var result  = SqlFragment.text(s"update $tableName set ").append(setFrag)

    if wherePredicates.nonEmpty then
      val whereJoined = Placeholder.join(wherePredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(whereJoined)

    result
  end build

  /** Build UPDATE with RETURNING clause (for dialects that support it) */
  def returning: SqlFragment =
    build :+ SqlFragment.text(" returning *")

  /** Build UPDATE with RETURNING clause, with compile-time capability check.
    *
    * Only compiles if the dialect supports RETURNING (PostgreSQL, SQLite).
    *
    * Usage:
    * {{{
    *   given dialect: Dialect & ReturningSupport = PostgresDialect
    *
    *   Update[User]
    *     .set(_.status, "active")
    *     .where(_.id).eq(123)
    *     .returningAs
    *     .queryOne[User]  // Option[User]
    * }}}
    */
  def returningAs(using Dialect & ReturningSupport): ReturningQuery[A] =
    ReturningQuery[A](build :+ SqlFragment.text(" returning *"))

end UpdateReady

object Update:
  /** Create an Update builder for a table type */
  def apply[A: Table]: UpdateBuilder[A] =
    val table = summon[Table[A]]
    UpdateBuilder(table.name, table.columnMap)

// ============================================================================
// UpdateWhereBuilder (from UpdateBuilder -> UpdateReady)
// ============================================================================

/** Type-safe WHERE builder for Update.
  *
  * Extends WhereBuilderOps to inherit all comparison operators. Returns UpdateReady.
  */
final case class UpdateWhereBuilder[A: Table, T](
    builder: UpdateBuilder[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[UpdateReady[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): UpdateReady[A] =
    UpdateReady(builder.tableName, builder.fieldNamesToColumns, builder.setClauses, Vector(predicate))

end UpdateWhereBuilder

// ============================================================================
// UpdateReadyWhereBuilder (from UpdateReady -> UpdateReady, for chaining)
// ============================================================================

/** Type-safe WHERE builder for chaining additional conditions on UpdateReady.
  *
  * Extends WhereBuilderOps to inherit all comparison operators.
  */
final case class UpdateReadyWhereBuilder[A: Table, T](
    ready: UpdateReady[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[UpdateReady[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): UpdateReady[A] =
    ready.copy(wherePredicates = ready.wherePredicates :+ predicate)

end UpdateReadyWhereBuilder

// ============================================================================
// Delete Builder (not yet ready to build)
// ============================================================================

/** Type-safe DELETE builder.
  *
  * This builder does NOT have a `.build` method. You must call `.where()` or `.all` first to get a `DeleteReady` which
  * can be built.
  *
  * Usage:
  * {{{
  *   Delete[User]
  *     .where(_.status).eq("inactive")  // Returns DeleteReady
  *     .build
  *     .execute
  * }}}
  */
final case class DeleteBuilder[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
):
  /** Start a type-safe WHERE condition by selecting a column */
  inline def where[T](inline selector: A => T): DeleteWhereBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    DeleteWhereBuilder(this, Alias.unsafe(tableName), col)

  /** Add a WHERE predicate using SqlFragment */
  def where(predicate: SqlFragment): DeleteReady[A] =
    DeleteReady(tableName, fieldNamesToColumns, Vector(predicate))

  /** Explicitly mark this as deleting all rows (no WHERE).
    *
    * This is required to prevent accidental deletion of all rows.
    */
  def all: DeleteReady[A] =
    DeleteReady(tableName, fieldNamesToColumns, Vector.empty)

end DeleteBuilder

// ============================================================================
// Delete Ready (has WHERE or explicit .all - ready to build)
// ============================================================================

/** DELETE that is ready to build - has WHERE clause or explicit `.all`.
  *
  * This type has `.build` and `.returning` methods.
  */
final case class DeleteReady[A: Table](
    private[saferis] val tableName: String,
    private[saferis] val fieldNamesToColumns: Map[String, Column[?]],
    private[saferis] val wherePredicates: Vector[SqlFragment],
):
  /** Chain another type-safe WHERE condition */
  inline def where[T](inline selector: A => T): DeleteReadyWhereBuilder[A, T] =
    val fieldName = Macros.extractFieldName[A, T](selector)
    val col       = fieldNamesToColumns(fieldName).asInstanceOf[Column[T]]
    DeleteReadyWhereBuilder(this, Alias.unsafe(tableName), col)

  /** Add a WHERE predicate using SqlFragment */
  def where(predicate: SqlFragment): DeleteReady[A] =
    copy(wherePredicates = wherePredicates :+ predicate)

  /** Add a grouped WHERE condition using lambda syntax for complex OR/AND expressions.
    *
    * Usage:
    * {{{
    *   Delete[User]
    *     .where(_.status).eq("inactive")
    *     .andWhere(w => w(_.deletedAt).isNotNull.or(_.lastLogin).lt(cutoffDate))
    *     .build
    * }}}
    *
    * Generates: `DELETE FROM users WHERE status = ? AND (deleted_at IS NOT NULL OR last_login < ?)`
    */
  def andWhere(builder: WhereGroupBuilder[A] => WhereGroupChain[A]): DeleteReady[A] =
    val groupBuilder = WhereGroupBuilder[A](fieldNamesToColumns, Alias.unsafe(tableName))
    val chain        = builder(groupBuilder)
    val whereFrag    = chain.toWhereGroup.toSqlFragment
    copy(wherePredicates = wherePredicates :+ whereFrag)

  /** Build the DELETE SQL fragment */
  def build: SqlFragment =
    var result = SqlFragment.text(s"delete from $tableName")

    if wherePredicates.nonEmpty then
      val whereJoined = Placeholder.join(wherePredicates, " and ")
      result = result :+ SqlFragment.text(" where ") :+ SqlFragment(whereJoined)

    result
  end build

  /** Build DELETE with RETURNING clause (for dialects that support it) */
  def returning: SqlFragment =
    build :+ SqlFragment.text(" returning *")

  /** Build DELETE with RETURNING clause, with compile-time capability check.
    *
    * Only compiles if the dialect supports RETURNING (PostgreSQL, SQLite).
    *
    * Usage:
    * {{{
    *   given dialect: Dialect & ReturningSupport = PostgresDialect
    *
    *   Delete[User]
    *     .where(_.status).eq("deleted")
    *     .returningAs
    *     .queryOne[User]  // Option[User]
    * }}}
    */
  def returningAs(using Dialect & ReturningSupport): ReturningQuery[A] =
    ReturningQuery[A](build :+ SqlFragment.text(" returning *"))

end DeleteReady

object Delete:
  /** Create a Delete builder for a table type */
  def apply[A: Table]: DeleteBuilder[A] =
    val table = summon[Table[A]]
    DeleteBuilder(table.name, table.columnMap)

// ============================================================================
// DeleteWhereBuilder (from DeleteBuilder -> DeleteReady)
// ============================================================================

/** Type-safe WHERE builder for Delete.
  *
  * Extends WhereBuilderOps to inherit all comparison operators. Returns DeleteReady.
  */
final case class DeleteWhereBuilder[A: Table, T](
    builder: DeleteBuilder[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[DeleteReady[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): DeleteReady[A] =
    DeleteReady(builder.tableName, builder.fieldNamesToColumns, Vector(predicate))

end DeleteWhereBuilder

// ============================================================================
// DeleteReadyWhereBuilder (from DeleteReady -> DeleteReady, for chaining)
// ============================================================================

/** Type-safe WHERE builder for chaining additional conditions on DeleteReady.
  *
  * Extends WhereBuilderOps to inherit all comparison operators.
  */
final case class DeleteReadyWhereBuilder[A: Table, T](
    ready: DeleteReady[A],
    fromAlias: Alias,
    fromColumn: Column[T],
) extends WhereBuilderOps[DeleteReady[A], T]:
  protected def whereAlias: Alias                                    = fromAlias
  protected def whereColumn: Column[T]                               = fromColumn
  protected def addPredicate(predicate: SqlFragment): DeleteReady[A] =
    ready.copy(wherePredicates = ready.wherePredicates :+ predicate)

end DeleteReadyWhereBuilder
