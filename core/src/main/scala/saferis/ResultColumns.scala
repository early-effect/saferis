package saferis

import zio.Chunk

/** Which result columns a read will use. A driver materializes these and nothing else, while the result is still open.
  * The row type decodes the `SqlRow` after that result is closed.
  */
enum ResultColumns:
  /** Every column, in result order. Tuples, `queryValue`, and catalog reads. */
  case All

  /** These labels. Matching is the same first-hit, case-insensitive match as [[SqlRow.get]]. */
  case Labels(names: Chunk[ColumnName])

object ResultColumns:
  /** Indexes to materialize, in result order. A later duplicate of a label is not included: [[SqlRow.get]] would not
    * return it. A wanted label that is absent is left out, and the row decode reports [[DecodeError.MissingColumn]].
    */
  def indexes(labels: Chunk[ColumnName], columns: ResultColumns): Chunk[Int] =
    columns match
      case ResultColumns.All =>
        Chunk.fromIterable(labels.indices)
      case ResultColumns.Labels(wanted) =>
        val (kept, _) =
          labels.zipWithIndex.foldLeft((Chunk.empty[Int], Chunk.empty[ColumnName])):
            case ((taken, seen), (label, index)) =>
              val wantedHit = wanted.exists(_.matchesLabel(label))
              val already   = seen.exists(_.matchesLabel(label))
              if wantedHit && !already then (taken :+ index, seen :+ label)
              else (taken, seen)
        kept
end ResultColumns

/** A decode and the columns it will ask for. The two travel together so a driver can skip the rest. */
final case class RowRead[A](columns: ResultColumns, decode: SqlRow => Either[SaferisError, A])

object RowRead:
  def all[A](decode: SqlRow => Either[SaferisError, A]): RowRead[A] =
    RowRead(ResultColumns.All, decode)
