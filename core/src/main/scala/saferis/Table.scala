package saferis

import scala.annotation.StaticAnnotation

final case class tableName(name: String) extends StaticAnnotation

/** Select this table's named columns instead of `*`. A table without it still renders `select *`. The flag is a
  * select-list policy: insert, update, and schema verify still use [[Table.columns]].
  */
class projectColumns extends StaticAnnotation

sealed trait Table[A]:
  private[saferis] def name: TableName
  def columns: Seq[Column[?]]

  /** True when the case class carries [[projectColumns]]. */
  def projectColumns: Boolean
  private[saferis] def columnMap: Map[String, Column[?]]       = columns.map(c => (c.name: String) -> c).toMap
  transparent inline def instance                              = Macros.instanceOf[A](alias = None)
  transparent inline def aliasedInstance(inline alias: String) =
    val _ = Alias(alias) // Compile-time validation that alias is a string literal
    Macros.instanceOf[A](alias = Some(alias))
  private[saferis] def insertColumnsSql: SqlFragment =
    parenthesize(columns.filterNot(_.isGenerated).map(column => SqlFragment(column)))
  private[saferis] def returningColumnsSql: SqlFragment =
    joinFragments(columns.map(column => SqlFragment(column)))
  private[saferis] inline def insertPlaceholders(a: A): Seq[Placeholder] =
    Macros
      .columnPlaceholders(a)
      .filterNot: (name, _) =>
        columnMap(name).isGenerated
      .map: (_, p) =>
        p
  private[saferis] inline def insertPlaceholdersSql(a: A): SqlFragment =
    val placeholders = insertPlaceholders(a)
    if placeholders.isEmpty then SqlFragment.text("()")
    else
      SqlFragment
        .text("(")
        .append(SqlFragment(Placeholder.join(placeholders, ", ")))
        .append(SqlFragment.text(")"))

  private[saferis] inline def updateSetClause(a: A): SqlFragment =
    val placeholders = Macros
      .columnPlaceholders(a)
      .filterNot: (name, _) =>
        val col = columnMap(name)
        col.isGenerated || col.isKey
    val setClauses = placeholders.map: (name, placeholder) =>
      val column = columnMap(name)
      SqlFragment(column).append(SqlFragment.text(" = ")).append(SqlFragment(placeholder))
    if setClauses.isEmpty then SqlFragment.empty
    else setClauses.reduce((left, right) => left.append(SqlFragment.text(", ")).append(right))
  end updateSetClause

  private[saferis] inline def updateWhereClause(a: A): SqlFragment =
    val keyPlaceholders = Macros
      .columnPlaceholders(a)
      .filter: (name, _) =>
        columnMap(name).isKey
    val whereClauses = keyPlaceholders.map: (name, placeholder) =>
      val column = columnMap(name)
      SqlFragment(column).append(SqlFragment.text(" = ")).append(SqlFragment(placeholder))
    if whereClauses.nonEmpty then
      SqlFragment
        .text(" where ")
        .append(
          whereClauses.reduce((left, right) => left.append(SqlFragment.text(" and ")).append(right))
        )
    else SqlFragment.empty
  end updateWhereClause

  private def parenthesize(columns: Seq[SqlFragment]): SqlFragment =
    SqlFragment.text("(").append(joinFragments(columns)).append(SqlFragment.text(")"))

  private def joinFragments(columns: Seq[SqlFragment]): SqlFragment =
    columns.toList match
      case Nil           => SqlFragment.empty
      case first :: rest =>
        rest.foldLeft(first)((acc, next) => acc.append(SqlFragment.text(", ")).append(next))

end Table

object Table:
  transparent inline def apply[A](using table: Table[A])                       = table.instance
  transparent inline def apply[A](inline alias: String)(using table: Table[A]) =
    table.aliasedInstance(alias)

  final case class Derived[A](
      name: TableName,
      columns: Seq[Column[?]],
      projectColumns: Boolean = false,
  ) extends Table[A]

  inline def derived[A]: Table[A] =
    Derived[A](Macros.nameOf[A], Macros.columnsOf[A], Macros.projectsColumns[A])

end Table
