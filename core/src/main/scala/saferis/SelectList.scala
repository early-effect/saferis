package saferis

/** The select list of a query builder statement. An unannotated query stays `*`. An annotated table contributes its
  * columns, qualified by the instance alias, in case-class order. A joined table that is not annotated contributes
  * `alias.*` once any table in the query is annotated, so its columns are neither expanded nor dropped.
  */
private[saferis] object SelectList:
  def starOrNamed(tables: Seq[Instance[?]]): SqlFragment =
    if tables.forall(!_.tableEvidence.projectColumns) then SqlFragment.text("*")
    else comma(tables.map(one))

  def selectFrom(tables: Seq[Instance[?]]): SqlFragment =
    SqlFragment.text("select ").append(starOrNamed(tables)).append(SqlFragment.text(" from "))

  /** Columns of one instance, already qualified when the instance has an alias. */
  def namedColumns(instance: Instance[?]): SqlFragment =
    comma(instance.columns.map(column => SqlFragment(column)))

  private def one(instance: Instance[?]): SqlFragment =
    if instance.tableEvidence.projectColumns then namedColumns(instance)
    else
      val alias = instance.alias.getOrElse(Alias.unsafe(instance.tableName))
      SqlFragment.ident(alias).append(SqlFragment.text(".*"))

  private def comma(parts: Seq[SqlFragment]): SqlFragment =
    parts.toList match
      case Nil          => SqlFragment.empty
      case head :: tail =>
        tail.foldLeft(head)((acc, next) => acc.append(SqlFragment.text(", ")).append(next))
end SelectList
