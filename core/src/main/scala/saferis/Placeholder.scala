package saferis

import zio.Chunk
import zio.Duration

/** A hole in a SQL statement: text, parameters, or both, plus any construction issues. */
trait Placeholder:
  def pieces: Chunk[SqlPiece]
  def issues: List[FragmentIssue]
  def timeout: Option[Duration] = None
  def sql: String               = SqlPieces.postgres(pieces)

  final def ++(other: Placeholder): Placeholder =
    Placeholder.Derived(
      SqlPieces.merge(pieces ++ other.pieces),
      issues ++ other.issues,
      timeout.orElse(other.timeout),
    )
end Placeholder

object Placeholder:
  def apply[A](a: A)(using encoder: Encoder[A]): Placeholder =
    param(encoder.encode(a))

  def apply(p: Placeholder): Placeholder = p

  def raw(sql: String): Placeholder =
    if sql.isEmpty then Empty else Derived(Chunk(SqlPiece.Text(sql)), Nil, None)

  def param(value: SqlValue): Placeholder =
    Derived(Chunk(SqlPiece.Param(value)), Nil, None)

  def identifier(identifier: String)(using dialect: Dialect): Placeholder =
    raw(dialect.escapeIdentifier(identifier))

  def concat(placeholders: Placeholder*): Placeholder =
    if placeholders.isEmpty then Empty
    else placeholders.tail.foldLeft(placeholders.head)(_ ++ _)

  def join(placeholders: Seq[Placeholder], separator: String = ", "): Placeholder =
    if placeholders.isEmpty then Empty
    else if placeholders.sizeIs == 1 then placeholders.head
    else
      val sep = raw(separator)
      placeholders.tail.foldLeft(placeholders.head)((acc, next) => acc ++ sep ++ next)

  def commaList(placeholders: Placeholder*): Placeholder =
    join(placeholders, ", ")

  def list[A](values: Iterable[A])(using Encoder[A]): Placeholder =
    array(values)

  def list[A](first: A, rest: A*)(using Encoder[A]): Placeholder =
    array(first +: rest)

  /** One array parameter. An empty collection is `'{}'`: `= ANY('{}')` is false and `<> ALL('{}')` is true. */
  def array[A](values: Iterable[A])(using encoder: Encoder[A]): Placeholder =
    param(members(values, encoder))

  def array[A](first: A, rest: A*)(using encoder: Encoder[A]): Placeholder =
    array(first +: rest)

  private def members[A](values: Iterable[A], encoder: Encoder[A]): SqlValue =
    val encoded = Chunk.fromIterable(values).distinct.map(encoder.encode)
    SqlValue.array(encoder.sqlType, encoded).getOrElse(SqlValue.Array(encoder.sqlType, encoded))

  private[saferis] def allIssues(ps: Seq[Placeholder]): List[FragmentIssue] =
    ps.toList.flatMap(_.issues)

  final private case class Derived(
      pieces: Chunk[SqlPiece],
      issues: List[FragmentIssue],
      override val timeout: Option[Duration],
  ) extends Placeholder

  given convertToPlaceholder[A](using encoder: Encoder[A]): Conversion[A, Placeholder] with
    def apply(a: A): Placeholder = param(encoder.encode(a))

  given convertSeqOfPlaceholdersToPlaceholder: Conversion[Seq[Placeholder], Placeholder] with
    def apply(as: Seq[Placeholder]): Placeholder = concat(as*)

  final private[saferis] class RawSql(sqlText: String) extends Placeholder:
    val pieces: Chunk[SqlPiece]     = if sqlText.isEmpty then Chunk.empty else Chunk(SqlPiece.Text(sqlText))
    val issues: List[FragmentIssue] = Nil
    override def toString: String   = s"RawSql($sqlText)"

  object Empty extends Placeholder:
    val pieces: Chunk[SqlPiece]     = Chunk.empty
    val issues: List[FragmentIssue] = Nil
    override def toString: String   = "Placeholder.Empty"

    def isEmpty(p: Placeholder): Boolean = p match
      case Empty => true
      case other =>
        other.pieces.forall:
          case SqlPiece.Text(t)  => t.trim.isEmpty
          case _: SqlPiece.Param => false
  end Empty
end Placeholder
