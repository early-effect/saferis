package saferis

import scala.annotation.StaticAnnotation

final case class tableName(name: String) extends StaticAnnotation

sealed trait Table[A]:
  private[saferis] def name: String
  def columns: Seq[Column[?]]
  private[saferis] def columnMap                               = columns.map(c => c.name -> c).toMap
  transparent inline def instance                              = Macros.instanceOf[A](alias = None)
  transparent inline def aliasedInstance(inline alias: String) =
    val _ = Alias(alias) // Compile-time validation that alias is a string literal
    Macros.instanceOf[A](alias = Some(alias))
  private[saferis] def insertColumnsSql: SqlFragment =
    SqlFragment.text(columns.filterNot(_.isGenerated).map(_.sql).mkString("(", ", ", ")"))
  private[saferis] def returningColumnsSql: SqlFragment =
    SqlFragment.text(columns.map(_.sql).mkString(", "))
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
      SqlFragment.text(s"${column.sql} = ").append(SqlFragment(placeholder))
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
      SqlFragment.text(s"${column.sql} = ").append(SqlFragment(placeholder))
    if whereClauses.nonEmpty then
      SqlFragment
        .text(" where ")
        .append(
          whereClauses.reduce((left, right) => left.append(SqlFragment.text(" and ")).append(right))
        )
    else SqlFragment.empty
  end updateWhereClause

end Table

object Table:
  transparent inline def apply[A](using table: Table[A])                       = table.instance
  transparent inline def apply[A](inline alias: String)(using table: Table[A]) =
    table.aliasedInstance(alias)

  final case class Derived[A](name: String, columns: Seq[Column[?]]) extends Table[A]

  inline def derived[A]: Table[A] =
    Derived[A](Macros.nameOf[A], Macros.columnsOf[A])

end Table
