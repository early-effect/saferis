package saferis

/** Represents a problem detected at SQL-fragment construction time that should fail the statement before any JDBC call
  * is made.
  *
  * Issues are accumulated on `Placeholder` and `SqlFragment` and surface as [[SaferisError.InvalidStatement]] when the
  * fragment is run. This keeps fragment construction pure (no thrown exceptions) while still surfacing failures through
  * the library's typed-error channel.
  */
sealed trait FragmentIssue:
  def description: String

object FragmentIssue:

  /** An IN/NOT-IN/list helper was called with an empty (or degenerate-empty-after-dedupe) collection. The resulting SQL
    * would be invalid (`= ANY()` / an empty array parameter).
    *
    * @param helper
    *   Name of the helper that produced the issue (e.g. "in", "Placeholder.list", "WhereBuilder.in"). Used in the error
    *   message so the user can locate the offending call.
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
end FragmentIssue
