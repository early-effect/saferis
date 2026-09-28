package saferis

import zio.*

/** Specialized DML operations that are only available when dialects support specific features */
object SpecializedDML:

  /** Insert with RETURNING clause - only available for dialects that support it */
  inline def insertReturning[A](entity: A)(using
      table: Table[A]
  )(using
      Dialect & ReturningSupport
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val sql =
      SqlFragment
        .text("insert into ")
        .append(SqlFragment.ident(table.name))
        .append(SqlFragment.text(" "))
        .append(table.insertColumnsSql)
        .append(SqlFragment.text(" values "))
        .append(table.insertPlaceholdersSql(entity))
        .append(SqlFragment.text(" returning "))
        .append(table.returningColumnsSql)
    sql.queryOne[A]
  end insertReturning

  /** Update with RETURNING clause - only available for dialects that support it */
  inline def updateReturning[A](entity: A)(using
      table: Table[A]
  )(using
      Dialect & ReturningSupport
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val sql =
      SqlFragment
        .text("update ")
        .append(SqlFragment.ident(table.name))
        .append(SqlFragment.text(" set "))
        .append(table.updateSetClause(entity))
        .append(table.updateWhereClause(entity))
        .append(SqlFragment.text(" returning "))
        .append(table.returningColumnsSql)
    sql.queryOne[A]
  end updateReturning

  /** Delete with RETURNING clause - only available for dialects that support it */
  inline def deleteReturning[A](entity: A)(using
      table: Table[A]
  )(using
      Dialect & ReturningSupport
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val sql =
      SqlFragment
        .text("delete from ")
        .append(SqlFragment.ident(table.name))
        .append(table.updateWhereClause(entity))
        .append(SqlFragment.text(" returning "))
        .append(table.returningColumnsSql)
    sql.queryOne[A]
  end deleteReturning

  /** UPSERT operation - only available for dialects that support it.
    *
    * Internal: `conflictColumns` are caller-supplied strings interpolated as raw identifiers, so this is not a safe
    * public API. The safe public path is the fluent `Upsert[A].values(...).onConflict(_.col)` DSL, which resolves
    * conflict columns from type-safe selectors. Kept `private[saferis]`.
    */
  private[saferis] inline def upsert[A](entity: A, conflictColumns: Seq[ColumnName])(using
      table: Table[A]
  )(using dialect: Dialect & UpsertSupport)(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val conflicts = conflictColumns.map(dialect.escapeIdentifier).mkString(", ")
    val sql       =
      SqlFragment
        .text("insert into ")
        .append(SqlFragment.ident(table.name))
        .append(SqlFragment.text(" "))
        .append(table.insertColumnsSql)
        .append(SqlFragment.text(" values "))
        .append(table.insertPlaceholdersSql(entity))
        .append(SqlFragment.text(s" on conflict ($conflicts) do update set "))
        .append(table.updateSetClause(entity))
    sql.dml
  end upsert

  /** Create index with IF NOT EXISTS - only available for dialects that support it */
  inline def createIndexIfNotExists[A](
      indexName: IndexName,
      columnNames: Seq[ColumnName],
      unique: Boolean = false,
  )(using
      table: Table[A],
      dialect: Dialect & IndexIfNotExistsSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val sql =
      SqlFragment.text(dialect.createIndexIfNotExistsSql(indexName, table.name, columnNames, unique))
    sql.dml
  end createIndexIfNotExists

  /** JSON field extraction - only available for dialects that support JSON.
    *
    * Internal: `columnName` is interpolated as a raw identifier, so this is not a safe public API for caller-supplied
    * strings. The safe public path is the schema DSL (`Query`/`Schema` `.where(_.col).json*`), which resolves column
    * labels from the schema. Kept `private[saferis]` for internal use and tests.
    */
  private[saferis] def jsonExtract(column: SqlText, fieldPath: String)(using
      dialect: Dialect & JsonSupport
  ): SqlFragment =
    SqlFragment.text(dialect.jsonExtractSql(column, fieldPath))

  /** Array containment check - only available for dialects that support arrays.
    *
    * Internal: `columnName` and `value` are interpolated raw, so this is not a safe public API for caller-supplied
    * strings. Use the schema DSL for safe queries. Kept `private[saferis]` for internal use.
    */
  private[saferis] def arrayContains(column: SqlText, value: SqlText)(using
      dialect: Dialect & ArraySupport
  ): SqlFragment =
    SqlFragment.text(dialect.arrayContainsSql(column, value))

  /** Get SQL for UPSERT operation - only available for dialects that support it.
    *
    * Internal: takes raw column-name/SQL strings, so this is not a safe public API. Use the fluent `Upsert` DSL. Kept
    * `private[saferis]`.
    */
  private[saferis] def upsertSql[A](
      insertColumns: SqlText,
      conflictColumns: Seq[ColumnName],
      updateColumns: SqlText,
  )(using table: Table[A], dialect: Dialect & UpsertSupport): SqlText =
    dialect.upsertSql(table.name, insertColumns, conflictColumns, updateColumns)

end SpecializedDML
