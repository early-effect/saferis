package saferis

import zio.Chunk
import zio.Trace
import zio.ZIO

/** What every catalog introspection reads, whatever SQL produced it. A catalog query selects these labels:
  * `table_name`; `column_name`, `data_type`, `is_nullable`, `column_default`, `ordinal_position` for columns;
  * `constraint_name` for unique constraints; `from_column`, `to_table`, `to_column`, `update_rule`, `delete_rule` for
  * foreign keys; `index_name`, `is_unique`, `where_clause` for indexes.
  */
private[saferis] object CatalogRows:
  final case class ColumnRow(
      name: ColumnName,
      dataType: ColumnType,
      nullable: Boolean,
      defaultValue: Option[SqlText],
      ordinal: Int,
  )

  final case class OrdinalName(name: ColumnName, ordinal: Int)

  final case class ConstraintColumn(constraint: ConstraintName, name: ColumnName, ordinal: Int)

  final case class ForeignKeyRow(
      constraint: ConstraintName,
      fromColumn: ColumnName,
      toTable: TableName,
      toColumn: ColumnName,
      onUpdate: ForeignKeyAction,
      onDelete: ForeignKeyAction,
      ordinal: Int,
  )

  final case class IndexRow(
      indexName: IndexName,
      column: ColumnName,
      isUnique: Boolean,
      whereClause: Option[SqlText],
      ordinal: Int,
  )

  def run[A](fragment: SqlFragment)(read: SqlRow => Either[SaferisError, A])(using
      Trace
  ): ZIO[SqlSession, SaferisError, Chunk[A]] =
    fragment.toCommand.flatMap: command =>
      ZIO.serviceWithZIO[SqlSession](_.query(command)(read))

  private def cell[A: Decoder](row: SqlRow, label: String, expected: String): Either[SaferisError, A] =
    row
      .get(ColumnName(label))
      .flatMap(summon[Decoder[A]].decode)
      .left
      .map(err => SaferisError.DecodingError(ColumnName(label), TypeName(expected), err.detail))

  /** A boolean column, or an integer where the catalog has no boolean type (MySQL). Non-zero is true. */
  private def flag(row: SqlRow, label: String): Either[SaferisError, Boolean] =
    cell[Boolean](row, label, "bool").orElse(cell[Long](row, label, "integer").map(_ != 0L))

  private def text(row: SqlRow, label: String): Either[SaferisError, String] =
    cell[String](row, label, "text")

  private def action(row: SqlRow, label: String): Either[SaferisError, ForeignKeyAction] =
    text(row, label).flatMap: spelling =>
      ForeignKeyAction
        .parse(spelling)
        .toRight(
          SaferisError
            .DecodingError(ColumnName(label), TypeName("foreign key action"), s"unknown action '$spelling'")
        )

  def readTable(row: SqlRow): Either[SaferisError, TableName] =
    text(row, "table_name").map(TableName(_))

  def readColumn(row: SqlRow): Either[SaferisError, ColumnRow] =
    for
      name     <- text(row, "column_name")
      dataType <- text(row, "data_type")
      nullable <- text(row, "is_nullable")
      default  <- cell[Option[String]](row, "column_default", "text")
      ordinal  <- cell[Int](row, "ordinal_position", "integer")
    yield ColumnRow(
      ColumnName(name),
      ColumnType(dataType),
      nullable.equalsIgnoreCase("YES"),
      default.map(SqlText(_)),
      ordinal,
    )

  def readOrdinal(row: SqlRow): Either[SaferisError, OrdinalName] =
    for
      name    <- text(row, "column_name")
      ordinal <- cell[Int](row, "ordinal_position", "integer")
    yield OrdinalName(ColumnName(name), ordinal)

  def readConstraint(row: SqlRow): Either[SaferisError, ConstraintColumn] =
    for
      constraint <- text(row, "constraint_name")
      name       <- text(row, "column_name")
      ordinal    <- cell[Int](row, "ordinal_position", "integer")
    yield ConstraintColumn(ConstraintName(constraint), ColumnName(name), ordinal)

  def readForeignKey(row: SqlRow): Either[SaferisError, ForeignKeyRow] =
    for
      constraint <- text(row, "constraint_name")
      fromColumn <- text(row, "from_column")
      toTable    <- text(row, "to_table")
      toColumn   <- text(row, "to_column")
      onUpdate   <- action(row, "update_rule")
      onDelete   <- action(row, "delete_rule")
      ordinal    <- cell[Int](row, "ordinal_position", "integer")
    yield ForeignKeyRow(
      ConstraintName(constraint),
      ColumnName(fromColumn),
      TableName(toTable),
      ColumnName(toColumn),
      onUpdate,
      onDelete,
      ordinal,
    )

  def readIndex(row: SqlRow): Either[SaferisError, IndexRow] =
    for
      indexName   <- text(row, "index_name")
      column      <- text(row, "column_name")
      isUnique    <- flag(row, "is_unique")
      whereClause <- cell[Option[String]](row, "where_clause", "text")
      ordinal     <- cell[Int](row, "ordinal_position", "integer")
    yield IndexRow(IndexName(indexName), ColumnName(column), isUnique, whereClause.map(SqlText(_)), ordinal)

  def table(
      stored: TableName,
      columnRows: Chunk[ColumnRow],
      keyRows: Chunk[OrdinalName],
      uniqueRows: Chunk[ConstraintColumn],
      fkRows: Chunk[ForeignKeyRow],
      indexRows: Chunk[IndexRow],
  ): DatabaseTable =
    val keys = keyRows.toSeq.sortBy(_.ordinal).map(_.name)
    DatabaseTable(
      tableName = stored,
      columns = columnModels(columnRows, keys),
      primaryKeyColumns = keys,
      indexes = indexModels(indexRows),
      uniqueConstraints = uniqueModels(uniqueRows),
      foreignKeys = foreignKeyModels(fkRows),
    )
  end table

  private def columnModels(rows: Chunk[ColumnRow], keys: Seq[ColumnName]): Seq[DatabaseColumn] =
    val keyNames = keys.toSet
    rows.toSeq
      .sortBy(_.ordinal)
      .map: row =>
        DatabaseColumn(
          name = row.name,
          dataType = row.dataType,
          isNullable = row.nullable,
          isPrimaryKey = keyNames.contains(row.name),
          defaultValue = row.defaultValue,
          ordinalPosition = row.ordinal,
        )
  end columnModels

  private def grouped[A](rows: Chunk[A])(name: A => String, ordinal: A => Int): Seq[Seq[A]] =
    rows.groupBy(name).values.map(_.toSeq.sortBy(ordinal)).toSeq

  private def uniqueModels(rows: Chunk[ConstraintColumn]): Seq[DatabaseUniqueConstraint] =
    grouped(rows)(_.constraint, _.ordinal).flatMap: ordered =>
      ordered.headOption.map(head => DatabaseUniqueConstraint(head.constraint, ordered.map(_.name)))

  private def foreignKeyModels(rows: Chunk[ForeignKeyRow]): Seq[DatabaseForeignKey] =
    grouped(rows)(_.constraint, _.ordinal).flatMap: ordered =>
      ordered.headOption.map: head =>
        DatabaseForeignKey(
          constraintName = head.constraint,
          fromColumns = ordered.map(_.fromColumn),
          toTable = head.toTable,
          toColumns = ordered.map(_.toColumn),
          onDelete = head.onDelete,
          onUpdate = head.onUpdate,
        )

  private def indexModels(rows: Chunk[IndexRow]): Seq[DatabaseIndex] =
    grouped(rows)(_.indexName, _.ordinal).flatMap: ordered =>
      ordered.headOption.map: head =>
        DatabaseIndex(
          indexName = head.indexName,
          columns = ordered.map(_.column),
          isUnique = head.isUnique,
          whereClause = head.whereClause,
        )
end CatalogRows
