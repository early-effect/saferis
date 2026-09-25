package saferis

/** Actions that can be taken when a referenced row is deleted or updated */
enum ForeignKeyAction:
  case NoAction, Cascade, SetNull, SetDefault, Restrict

  /** Returns the SQL clause for this action */
  def toSql: SqlText = SqlText:
    this match
      case NoAction   => "no action"
      case Cascade    => "cascade"
      case SetNull    => "set null"
      case SetDefault => "set default"
      case Restrict   => "restrict"
end ForeignKeyAction

object ForeignKeyAction:
  /** The SQL spelling as a catalog reports it (`CASCADE`, `NO ACTION`, `set null`), case-insensitively. */
  def parse(text: String): Option[ForeignKeyAction] =
    val normalized = text.trim.toLowerCase(java.util.Locale.ROOT)
    values.find(_.toSql == normalized)

/** Represents a complete foreign key specification
  *
  * @tparam From
  *   The source table type containing the foreign key column(s)
  * @tparam To
  *   The referenced table type
  * @param fromColumns
  *   Column field names in the source table (will be converted to labels at SQL generation time)
  * @param toTable
  *   Name of the referenced table
  * @param toColumns
  *   Column field names in the referenced table (will be converted to labels at SQL generation time)
  * @param toColumnMap
  *   Column map from target table for field name to label conversion
  * @param onDelete
  *   Action to take when referenced row is deleted
  * @param onUpdate
  *   Action to take when referenced row is updated
  * @param constraintName
  *   Optional custom constraint name
  */
final case class ForeignKeySpec[From, To](
    fromColumns: Seq[FieldName],
    toTable: TableName,
    toColumns: Seq[FieldName],
    toColumnMap: Map[String, Column[?]] = Map.empty,
    onDelete: ForeignKeyAction = ForeignKeyAction.NoAction,
    onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
    constraintName: Option[ConstraintName] = None,
):
  /** Generates the SQL FOREIGN KEY constraint clause
    *
    * @param fromFieldToLabel
    *   Function to convert source table field names to column labels (respects @label annotations)
    */
  def toConstraintSql(fromFieldToLabel: String => ColumnName): SqlText =
    val constraintClause = constraintName.fold("")(n => s"constraint $n ")
    val fromColsSql      = fromColumns.map(fromFieldToLabel).mkString(", ")
    // Use toColumnMap if available, otherwise use field name directly
    val toColsSql      = toColumns.map(fn => toColumnMap.get(fn).fold(fn: String)(_.label)).mkString(", ")
    val onDeleteClause = if onDelete == ForeignKeyAction.NoAction then "" else s" on delete ${onDelete.toSql}"
    val onUpdateClause = if onUpdate == ForeignKeyAction.NoAction then "" else s" on update ${onUpdate.toSql}"
    SqlText(
      s"${constraintClause}foreign key ($fromColsSql) references $toTable ($toColsSql)$onDeleteClause$onUpdateClause"
    )
  end toConstraintSql
end ForeignKeySpec
