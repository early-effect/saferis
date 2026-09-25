package saferis

/** Represents actual column metadata from database introspection.
  *
  * @param name
  *   Column name in the database
  * @param dataType
  *   Database type (e.g., "integer", "varchar(255)", "jsonb")
  * @param isNullable
  *   Whether the column accepts NULL values
  * @param isPrimaryKey
  *   Whether the column is part of the primary key
  * @param defaultValue
  *   Default value expression (if any)
  * @param ordinalPosition
  *   Column position in the table (1-based)
  */
final case class DatabaseColumn(
    name: ColumnName,
    dataType: ColumnType,
    isNullable: Boolean,
    isPrimaryKey: Boolean,
    defaultValue: Option[SqlText],
    ordinalPosition: Int,
)

/** Represents an index from database introspection.
  *
  * @param indexName
  *   Name of the index
  * @param columns
  *   Columns included in the index (in order)
  * @param isUnique
  *   Whether this is a unique index
  * @param whereClause
  *   WHERE clause for partial indexes (if any)
  */
final case class DatabaseIndex(
    indexName: IndexName,
    columns: Seq[ColumnName],
    isUnique: Boolean,
    whereClause: Option[SqlText],
)

/** Represents a unique constraint from database introspection.
  *
  * @param constraintName
  *   Name of the constraint
  * @param columns
  *   Columns included in the constraint (in order)
  */
final case class DatabaseUniqueConstraint(
    constraintName: ConstraintName,
    columns: Seq[ColumnName],
)

/** Represents a foreign key constraint from database introspection.
  *
  * @param constraintName
  *   Name of the constraint
  * @param fromColumns
  *   Columns in this table
  * @param toTable
  *   Referenced table name
  * @param toColumns
  *   Columns in the referenced table
  * @param onDelete
  *   Action on delete
  * @param onUpdate
  *   Action on update
  */
final case class DatabaseForeignKey(
    constraintName: ConstraintName,
    fromColumns: Seq[ColumnName],
    toTable: TableName,
    toColumns: Seq[ColumnName],
    onDelete: ForeignKeyAction,
    onUpdate: ForeignKeyAction,
)

/** Represents actual table metadata from database introspection.
  *
  * @param tableName
  *   Name of the table
  * @param columns
  *   All columns in the table
  * @param primaryKeyColumns
  *   Columns that make up the primary key (in order)
  * @param indexes
  *   Non-primary-key indexes on the table
  * @param uniqueConstraints
  *   Unique constraints (table constraints, not indexes)
  * @param foreignKeys
  *   Foreign key constraints
  */
final case class DatabaseTable(
    tableName: TableName,
    columns: Seq[DatabaseColumn],
    primaryKeyColumns: Seq[ColumnName],
    indexes: Seq[DatabaseIndex],
    uniqueConstraints: Seq[DatabaseUniqueConstraint],
    foreignKeys: Seq[DatabaseForeignKey],
)
