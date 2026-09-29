package saferis

/** Represents a difference between expected schema and actual database schema. */
sealed trait SchemaIssue:
  def description: String
  def tableName: TableName

object SchemaIssue:

  // === Column Issues ===

  /** Table does not exist in the database */
  final case class TableNotFound(tableName: TableName) extends SchemaIssue:
    def description: String = s"Table '$tableName' does not exist in the database"

  /** Expected column is missing from the table */
  final case class MissingColumn(
      tableName: TableName,
      columnName: ColumnName,
      expectedType: ColumnType,
  ) extends SchemaIssue:
    def description: String = s"Column '$columnName' ($expectedType) is missing from table '$tableName'"

  /** Column exists but has wrong type */
  final case class TypeMismatch(
      tableName: TableName,
      columnName: ColumnName,
      expectedType: ColumnType,
      actualType: ColumnType,
  ) extends SchemaIssue:
    def description: String =
      s"Column '$columnName' in '$tableName': expected type '$expectedType', found '$actualType'"

  /** Column has wrong nullability */
  final case class NullabilityMismatch(
      tableName: TableName,
      columnName: ColumnName,
      expectedNullable: Boolean,
      actualNullable: Boolean,
  ) extends SchemaIssue:
    def description: String =
      val expected = if expectedNullable then "nullable" else "NOT NULL"
      val actual   = if actualNullable then "nullable" else "NOT NULL"
      s"Column '$columnName' in '$tableName': expected $expected, found $actual"
  end NullabilityMismatch

  /** Primary key columns do not match */
  final case class PrimaryKeyMismatch(
      tableName: TableName,
      expectedColumns: Seq[ColumnName],
      actualColumns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      s"Primary key mismatch in '$tableName': expected (${expectedColumns.mkString(", ")}), found (${actualColumns.mkString(", ")})"

  /** Extra column exists in database but not in schema definition */
  final case class ExtraColumn(
      tableName: TableName,
      columnName: ColumnName,
      columnType: ColumnType,
  ) extends SchemaIssue:
    def description: String =
      s"Extra column '$columnName' ($columnType) found in table '$tableName' not defined in schema"

  // === Index Issues ===

  /** Index on expected columns not found */
  final case class MissingIndex(
      tableName: TableName,
      expectedName: Option[IndexName],
      columns: Seq[ColumnName],
      isUnique: Boolean,
  ) extends SchemaIssue:
    def description: String =
      val uniqueStr = if isUnique then "unique " else ""
      val nameStr   = expectedName.fold("")(n => s"'$n' ")
      s"Missing ${uniqueStr}index ${nameStr}on (${columns.mkString(", ")}) in table '$tableName'"
  end MissingIndex

  /** Index exists on correct columns but with different name */
  final case class IndexNameMismatch(
      tableName: TableName,
      expectedName: IndexName,
      actualName: IndexName,
      columns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      s"Index on (${columns.mkString(", ")}) in '$tableName': expected name '$expectedName', found '$actualName'"

  // === Unique Constraint Issues ===

  /** Unique constraint on expected columns not found */
  final case class MissingUniqueConstraint(
      tableName: TableName,
      expectedName: Option[ConstraintName],
      columns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      val nameStr = expectedName.fold("")(n => s"'$n' ")
      s"Missing unique constraint ${nameStr}on (${columns.mkString(", ")}) in table '$tableName'"

  /** Unique constraint exists but with different name */
  final case class UniqueConstraintNameMismatch(
      tableName: TableName,
      expectedName: ConstraintName,
      actualName: ConstraintName,
      columns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      s"Unique constraint on (${columns.mkString(", ")}) in '$tableName': expected name '$expectedName', found '$actualName'"

  // === Foreign Key Issues ===

  /** Foreign key constraint not found */
  final case class MissingForeignKey(
      tableName: TableName,
      expectedName: Option[ConstraintName],
      fromColumns: Seq[ColumnName],
      toTable: TableName,
      toColumns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      val nameStr = expectedName.fold("")(n => s"'$n' ")
      s"Missing foreign key ${nameStr}(${fromColumns.mkString(", ")}) -> $toTable(${toColumns.mkString(", ")}) in table '$tableName'"
  end MissingForeignKey

  /** Foreign key exists but with different name */
  final case class ForeignKeyNameMismatch(
      tableName: TableName,
      expectedName: ConstraintName,
      actualName: ConstraintName,
      fromColumns: Seq[ColumnName],
      toTable: TableName,
      toColumns: Seq[ColumnName],
  ) extends SchemaIssue:
    def description: String =
      s"Foreign key (${fromColumns.mkString(", ")}) -> $toTable in '$tableName': expected name '$expectedName', found '$actualName'"
  end ForeignKeyNameMismatch
end SchemaIssue
