package saferis

/** A problem detected in a statement before a connection is checked out.
  *
  * Issues surface as [[SaferisError.InvalidStatement]] when the fragment is run. This keeps fragment construction pure
  * (no thrown exceptions) while still surfacing failures through the library's typed-error channel.
  */
sealed trait FragmentIssue:
  def description: String

object FragmentIssue:

  /** `in` or `Placeholder.list` was called with an empty (or degenerate-empty-after-dedupe) collection. The resulting
    * SQL would be invalid (`IN ()`).
    *
    * @param helper
    *   Name of the helper that produced the issue (e.g. "in", "Placeholder.list"). Used in the error message so the
    *   user can locate the offending call.
    * @param origin
    *   Stack frame captured at construction, pointing at the user's call site. Surfaces in the error message even
    *   though the failure is reported at execution time.
    */
  final case class EmptyCollection(
      helper: String,
      origin: Option[StackTraceElement],
  ) extends FragmentIssue:
    def description: String =
      val site = origin.fold("")(o => s" at $o")
      s"$helper requires a non-empty collection$site; guard the call site with `if coll.isEmpty then ...`"

  /** An array parameter whose members are not all its element type. That is a bug in the element's `Encoder`: its
    * `sqlType` disagrees with what `encode` returns.
    *
    * @param parameter
    *   One-based parameter position, as in `$n`.
    */
  final case class MalformedArray(parameter: Int, detail: String) extends FragmentIssue:
    def description: String = s"array parameter $$$parameter is malformed: $detail"
end FragmentIssue
