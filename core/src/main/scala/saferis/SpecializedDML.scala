package saferis

import zio.*

/** Specialized DML operations that are only available when dialects support specific features */
object SpecializedDML:

  /** Insert with RETURNING clause - only available for dialects that support it */
  inline def insertReturning[A](entity: A)(using
      table: Table[A],
      dialect: Dialect & ReturningSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val _   = dialect.insertReturningSql(table.name, "", "")
    val sql =
      SqlFragment
        .text(s"insert into ${table.name} ")
        .append(table.insertColumnsSql)
        .append(SqlFragment.text(" values "))
        .append(table.insertPlaceholdersSql(entity))
        .append(SqlFragment.text(" returning "))
        .append(table.returningColumnsSql)
    sql.queryOne[A]
  end insertReturning

  /** Update with RETURNING clause - only available for dialects that support it */
  inline def updateReturning[A](entity: A)(using
      table: Table[A],
      dialect: Dialect & ReturningSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val _   = dialect.updateReturningSql(table.name, "", "", "")
    val sql =
      SqlFragment
        .text(s"update ${table.name} set ")
        .append(table.updateSetClause(entity))
        .append(table.updateWhereClause(entity))
        .append(SqlFragment.text(" returning "))
        .append(table.returningColumnsSql)
    sql.queryOne[A]
  end updateReturning

  /** Delete with RETURNING clause - only available for dialects that support it */
  inline def deleteReturning[A](entity: A)(using
      table: Table[A],
      dialect: Dialect & ReturningSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val _   = dialect.deleteReturningSql(table.name, "", "")
    val sql =
      SqlFragment
        .text(s"delete from ${table.name}")
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
  private[saferis] inline def upsert[A](entity: A, conflictColumns: Seq[String])(using
      table: Table[A],
      dialect: Dialect & UpsertSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val _   = dialect.upsertSql(table.name, "", conflictColumns, "")
    val sql =
      SqlFragment
        .text(s"insert into ${table.name} ")
        .append(table.insertColumnsSql)
        .append(SqlFragment.text(" values "))
        .append(table.insertPlaceholdersSql(entity))
        .append(SqlFragment.text(s" on conflict (${conflictColumns.mkString(", ")}) do update set "))
        .append(table.updateSetClause(entity))
    sql.dml
  end upsert

  /** Create index with IF NOT EXISTS - only available for dialects that support it */
  inline def createIndexIfNotExists[A](
      indexName: String,
      columnNames: Seq[String],
      unique: Boolean = false,
  )(using
      table: Table[A],
      dialect: Dialect & IndexIfNotExistsSupport,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName = table.name
    // indexName and columnNames are caller-supplied strings, so escape them at this public trust boundary to
    // prevent SQL injection. tableName comes from the compile-time @tableName/type name (not user input) and is
    // left idiomatic. The underlying builder is also called internally with schema-derived labels, which must
    // stay unquoted, so escaping belongs here rather than in the builder.
    val safeIndexName   = dialect.escapeIdentifier(indexName)
    val safeColumnNames = columnNames.map(dialect.escapeIdentifier)
    val sql             =
      SqlFragment.text(dialect.createIndexIfNotExistsSql(safeIndexName, tableName, safeColumnNames, unique))
    sql.dml
  end createIndexIfNotExists

  /** JSON field extraction - only available for dialects that support JSON.
    *
    * Internal: `columnName` is interpolated as a raw identifier, so this is not a safe public API for caller-supplied
    * strings. The safe public path is the schema DSL (`Query`/`Schema` `.where(_.col).json*`), which resolves column
    * labels from the schema. Kept `private[saferis]` for internal use and tests.
    */
  private[saferis] def jsonExtract(columnName: String, fieldPath: String)(using
      dialect: Dialect & JsonSupport
  ): SqlFragment =
    SqlFragment.text(dialect.jsonExtractSql(columnName, fieldPath))

  /** Array containment check - only available for dialects that support arrays.
    *
    * Internal: `columnName` and `value` are interpolated raw, so this is not a safe public API for caller-supplied
    * strings. Use the schema DSL for safe queries. Kept `private[saferis]` for internal use.
    */
  private[saferis] def arrayContains(columnName: String, value: String)(using
      dialect: Dialect & ArraySupport
  ): SqlFragment =
    SqlFragment.text(dialect.arrayContainsSql(columnName, value))

  /** Get SQL for UPSERT operation - only available for dialects that support it.
    *
    * Internal: takes raw column-name/SQL strings, so this is not a safe public API. Use the fluent `Upsert` DSL. Kept
    * `private[saferis]`.
    */
  private[saferis] def upsertSql[A](
      insertColumns: String,
      conflictColumns: Seq[String],
      updateColumns: String,
  )(using table: Table[A], dialect: Dialect & UpsertSupport): String =
    val tableName = summon[Table[A]].name
    dialect.upsertSql(tableName, insertColumns, conflictColumns, updateColumns)

end SpecializedDML
