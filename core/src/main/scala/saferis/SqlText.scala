package saferis

/** SQL as text: a statement, a clause, or a rendered fragment, with parameters as placeholders and no bound values. It
  * reads as a `String`, but a plain `String` is not SQL until [[SqlText.apply]] says so. Everything Saferis renders
  * (`SqlFragment.sql`, `Dialect` clauses, the SQL on errors and on `SqlExecuted`) is one.
  */
opaque type SqlText <: String = String

object SqlText:
  def apply(text: String): SqlText = text

  val empty: SqlText = ""

/** A column's type as DDL spells it (`varchar(255)`, `timestamptz`), or as a catalog reports it (`int4`). */
opaque type ColumnType <: String = String

object ColumnType:
  def apply(spelling: String): ColumnType = spelling

/** JSON document text, as bound to or read from a JSON column. Saferis does not parse it; `Json[A]` does. */
opaque type JsonText <: String = String

object JsonText:
  def apply(json: String): JsonText = json
