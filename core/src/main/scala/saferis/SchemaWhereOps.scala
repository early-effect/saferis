package saferis

/** Shared trait for Schema WHERE clause builders.
  *
  * Provides all WHERE operator implementations for DDL purposes (partial indexes). Unlike `WhereBuilderOps` which
  * generates parameterized SQL with `?` placeholders for DML, this trait generates literal SQL strings suitable for DDL
  * statements like CREATE INDEX ... WHERE.
  *
  * Used by `InstanceWhereColumnBuilder` and `InstanceWhereGroupColumnBuilder`.
  *
  * @tparam Result
  *   The type returned after completing a condition
  * @tparam T
  *   The column type being filtered
  */
trait SchemaWhereOps[Result, T]:
  /** The column name for the condition */
  protected def schemaColumnName: ColumnName

  /** Create the result with the given condition string */
  protected def completeCondition(condition: SqlText): Result

  /** Equals comparison */
  def eql(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName = ${encoder.literal(value)}"))

  /** Not equals comparison */
  def neql(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName <> ${encoder.literal(value)}"))

  /** Greater than comparison */
  def gt(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName > ${encoder.literal(value)}"))

  /** Greater than or equal comparison */
  def gte(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName >= ${encoder.literal(value)}"))

  /** Less than comparison */
  def lt(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName < ${encoder.literal(value)}"))

  /** Less than or equal comparison */
  def lte(value: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName <= ${encoder.literal(value)}"))

  /** IS NULL check */
  def isNull: Result =
    completeCondition(SqlText(s"$schemaColumnName is null"))

  /** IS NOT NULL check */
  def isNotNull: Result =
    completeCondition(SqlText(s"$schemaColumnName is not null"))

  /** LIKE pattern matching (for String columns) */
  def like(pattern: String)(using ev: T =:= String): Result =
    val escaped = pattern.replace("'", "''")
    completeCondition(SqlText(s"$schemaColumnName like '$escaped'"))

  /** NOT LIKE pattern matching (for String columns) */
  def notLike(pattern: String)(using ev: T =:= String): Result =
    val escaped = pattern.replace("'", "''")
    completeCondition(SqlText(s"$schemaColumnName not like '$escaped'"))

  /** IN clause */
  def in(values: Seq[T])(using encoder: Encoder[T]): Result =
    val literals = values.map(encoder.literal).mkString(", ")
    completeCondition(SqlText(s"$schemaColumnName in ($literals)"))

  /** NOT IN clause */
  def notIn(values: Seq[T])(using encoder: Encoder[T]): Result =
    val literals = values.map(encoder.literal).mkString(", ")
    completeCondition(SqlText(s"$schemaColumnName not in ($literals)"))

  /** BETWEEN clause */
  def between(low: T, high: T)(using encoder: Encoder[T]): Result =
    completeCondition(SqlText(s"$schemaColumnName between ${encoder.literal(low)} and ${encoder.literal(high)}"))

end SchemaWhereOps
