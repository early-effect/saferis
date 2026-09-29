package saferis

import zio.Chunk

/** One materialized row. Cells are already `SqlValue`. The callback may retain the row. */
final case class SqlRow(labels: Chunk[ColumnName], cells: Chunk[SqlValue]):
  def width: Int = cells.length

  def at(index: Int): Either[DecodeError, SqlValue] =
    if index < 0 || index >= cells.length then Left(DecodeError(s"column index $index out of range (width $width)"))
    else Right(cells(index))

  /** Case-insensitive first match, which is what `ResultSet.getString(label)` does. */
  def get(label: ColumnName): Either[DecodeError, SqlValue] =
    val index = labels.indexWhere(_.equalsIgnoreCase(label))
    if index < 0 then Left(DecodeError(s"missing column $label"))
    else at(index)
end SqlRow
