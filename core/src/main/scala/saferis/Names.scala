package saferis

/** Names Saferis puts into SQL or reads back from a catalog. Each is a distinct type, so a column name cannot be passed
  * where a table name is expected, and a plain `String` cannot be passed for either. Each still reads as a `String`
  * (interpolation, comparison, logging) without unwrapping. Construct one with its companion's `apply`.
  */

/** Evidence that `A` is a SQL name. A plain `String` has none, so it cannot be quoted into a statement until a
  * companion (`TableName`, `ColumnName`, `IndexName`, `ConstraintName`, or `Alias`) says so.
  */
trait SqlName[A]:
  def text(name: A): String

  /** A dot in a table name separates schema and table. Other names are one identifier. */
  def qualify: Boolean = false

/** A table as SQL names it: `users`, or `schema.table` when qualified. */
opaque type TableName <: String = String

object TableName:
  def apply(name: String): TableName = name
  given SqlName[TableName] with
    def text(name: TableName): String = name
    override def qualify: Boolean     = true
  extension (name: TableName) def folded: TableName = name.toLowerCase(java.util.Locale.ROOT)

/** A column as SQL names it: the `@label`, or the field name when there is none. */
opaque type ColumnName <: String = String

object ColumnName:
  def apply(name: String): ColumnName = name
  given SqlName[ColumnName] with
    def text(name: ColumnName): String = name
  extension (name: ColumnName) def folded: ColumnName = name.toLowerCase(java.util.Locale.ROOT)

/** A field of a Scala case class. Not a column name: `@label` can make those differ. */
opaque type FieldName <: String = String

object FieldName:
  def apply(name: String): FieldName = name

opaque type IndexName <: String = String

object IndexName:
  def apply(name: String): IndexName = name
  given SqlName[IndexName] with
    def text(name: IndexName): String = name

  /** `idx_<table>_<columns>`, the name Saferis gives an index the schema did not name. */
  def default(tableName: TableName, columns: Seq[ColumnName]): IndexName = s"idx_${tableName}_${columns.mkString("_")}"

  /** `idx_<table>_compound_key`, the index Saferis adds over a compound primary key. */
  def compoundKey(tableName: TableName): IndexName = s"idx_${tableName}_compound_key"

end IndexName

/** A named constraint: a primary key, unique constraint, foreign key, or check. */
opaque type ConstraintName <: String = String

object ConstraintName:
  def apply(name: String): ConstraintName = name
  given SqlName[ConstraintName] with
    def text(name: ConstraintName): String = name

/** A server type as a driver or the catalog spells it: `int4`, `varchar`, `mood`. */
opaque type TypeName <: String = String

object TypeName:
  def apply(name: String): TypeName = name

/** A dialect's display name, used in error messages: `PostgreSQL`, `MySQL`. */
opaque type DialectName <: String = String

object DialectName:
  def apply(name: String): DialectName = name
