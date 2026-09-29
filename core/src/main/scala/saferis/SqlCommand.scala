package saferis

import zio.Chunk
import zio.Duration

/** One piece of a statement. `Text` is caller SQL. `Ident` is a name, quoted when the statement is rendered. `Param` is
  * one bound value. Nothing scans the text for placeholders.
  */
enum SqlPiece:
  case Text(text: SqlText)
  case Ident(name: String, qualify: Boolean)
  case Param(value: SqlValue)

/** A validated statement. No issue list: invalid fragments never become a command.
  *
  * `identifierQuote` is the dialect's quote character, captured when the fragment became a command.
  */
final class SqlCommand private (
    val pieces: Chunk[SqlPiece],
    val timeout: Option[Duration],
    identifierQuote: String,
):
  /** The statement a driver sends. `placeholder` spells parameter `n` (one-based) of type `SqlType`. */
  def render(placeholder: (Int, SqlType) => String): SqlText =
    SqlPieces.render(pieces, placeholder, identifierQuote)

  /** `$n` text with no casts and no bound values. Identifiers use the quote captured with the command. */
  def inspection: SqlText = SqlPieces.postgres(pieces, identifierQuote)

  def withTimeout(timeout: Option[Duration]): SqlCommand =
    new SqlCommand(pieces, timeout, identifierQuote)
end SqlCommand

object SqlCommand:
  private[saferis] def apply(
      pieces: Chunk[SqlPiece],
      timeout: Option[Duration],
      identifierQuote: String = "\"",
  ): SqlCommand =
    new SqlCommand(pieces, timeout, identifierQuote)

private[saferis] object SqlPieces:
  def merge(pieces: Iterable[SqlPiece]): Chunk[SqlPiece] =
    val b             = Chunk.newBuilder[SqlPiece]
    val text          = new StringBuilder
    def flush(): Unit =
      if text.nonEmpty then
        b += SqlPiece.Text(SqlText(text.toString))
        text.clear()
    pieces.foreach:
      case SqlPiece.Text(t) =>
        text.append(t)
      case ident: SqlPiece.Ident =>
        flush()
        b += ident
      case param: SqlPiece.Param =>
        flush()
        b += param
    flush()
    b.result()
  end merge

  /** Replace every `Ident` with its quoted text. Already-quoted `Text` is left alone. */
  def quote(pieces: Chunk[SqlPiece], identifierQuote: String): Chunk[SqlPiece] =
    merge:
      pieces.map:
        case SqlPiece.Ident(name, qualify) => SqlPiece.Text(Dialect.quote(identifierQuote, name, qualify))
        case other                         => other

  def render(pieces: Chunk[SqlPiece], placeholder: (Int, SqlType) => String, identifierQuote: String): SqlText =
    val sb = new StringBuilder
    quote(pieces, identifierQuote).foldLeft(1): (n, piece) =>
      piece match
        case SqlPiece.Text(t) =>
          sb.append(t)
          n
        case SqlPiece.Ident(_, _) =>
          n
        case SqlPiece.Param(value) =>
          sb.append(placeholder(n, value.sqlType))
          n + 1
    SqlText(sb.toString)
  end render

  def show(pieces: Chunk[SqlPiece], identifierQuote: String): SqlText =
    val sb = new StringBuilder
    quote(pieces, identifierQuote).foreach:
      case SqlPiece.Text(t)      => sb.append(t)
      case SqlPiece.Ident(_, _)  => ()
      case SqlPiece.Param(value) => sb.append(SqlValue.literal(value))
    SqlText(sb.toString)

  /** One issue per array parameter that breaks the member rule. Every construction path meets here, in `toCommand`. */
  def arrayIssues(pieces: Chunk[SqlPiece]): List[FragmentIssue] =
    pieces
      .collect { case SqlPiece.Param(value) => value }
      .zipWithIndex
      .toList
      .flatMap((value, index) => SqlValue.malformed(value).map(FragmentIssue.MalformedArray(index + 1, _)))

  /** Postgres inspection form. The first parameter is `$1`. */
  def postgres(pieces: Chunk[SqlPiece], identifierQuote: String): SqlText =
    render(pieces, (n, _) => s"$$$n", identifierQuote)
end SqlPieces
