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

  /** Splice a collection as comma-separated parameters, `$1, $2, $3`, one per distinct value.
    *
    * Accepts any `Iterable[A]` (`Seq`, `List`, `Set`, `LinkedHashSet`, etc.). Duplicates are removed (set semantics,
    * matching typical IN-clause use), and order follows the input iterator. Useful for VALUES rows and other
    * comma-separated list contexts. For IN clauses prefer the top-level `in` helper, which adds the parentheses. Works
    * on every dialect. For a Postgres array value use [[array]].
    *
    * On empty (or degenerate-empty-after-dedupe) input, returns a placeholder carrying a
    * [[FragmentIssue.EmptyCollection]], which surfaces as [[SaferisError.InvalidStatement]] when the fragment is run.
    */
  def list[A](values: Iterable[A])(using Encoder[A]): Placeholder =
    listTagged(values, helper = "Placeholder.list", origin = captureOrigin())

  /** Varargs form. At least one element by construction. */
  def list[A](first: A, rest: A*)(using Encoder[A]): Placeholder =
    listTagged(first +: rest, helper = "Placeholder.list", origin = captureOrigin())

  /** Shared by `list` and `Interpolator.in`, so each tags the issue with the helper the user actually called. */
  private[saferis] def listTagged[A](values: Iterable[A], helper: String, origin: Option[StackTraceElement])(using
      encoder: Encoder[A]
  ): Placeholder =
    val deduped = values.iterator.distinct.toVector
    if deduped.isEmpty then Derived(Chunk.empty, List(FragmentIssue.EmptyCollection(helper, origin)), None)
    else join(deduped.map(a => param(encoder.encode(a))), ", ")

  /** The first stack frame outside the `saferis` package: the user's call site. */
  private[saferis] def captureOrigin(): Option[StackTraceElement] =
    val st = new Throwable().getStackTrace
    st.find(f => !f.getClassName.startsWith("saferis."))

  /** One array parameter holding the values as given, duplicates included. An empty collection is `'{}'`: `= ANY('{}')`
    * is false and `<> ALL('{}')` is true. A member that is not the element type fails at `toCommand`.
    */
  def array[A](values: Iterable[A])(using encoder: Encoder[A]): Placeholder =
    param(SqlValue.Array(encoder.sqlType, Chunk.fromIterable(values).map(encoder.encode)))

  def array[A](first: A, rest: A*)(using encoder: Encoder[A]): Placeholder =
    array(first +: rest)

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
