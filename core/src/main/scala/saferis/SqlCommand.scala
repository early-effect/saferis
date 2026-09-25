package saferis

import zio.Chunk
import zio.Duration

/** One piece of a statement. Text is caller SQL. Param is one bound value. Nothing scans the text for placeholders. */
enum SqlPiece:
  case Text(text: SqlText)
  case Param(value: SqlValue)

/** A validated statement. No issue list: invalid fragments never become a command. */
final class SqlCommand private (
    val pieces: Chunk[SqlPiece],
    val timeout: Option[Duration],
):
  /** The statement a driver sends. `placeholder` spells parameter `n` (one-based) of type `SqlType`. */
  def render(placeholder: (Int, SqlType) => String): SqlText =
    SqlPieces.render(pieces, placeholder)

  /** `$n` text with no casts and no bound values. The same string for every driver. */
  def inspection: SqlText = SqlPieces.postgres(pieces)

  def withTimeout(timeout: Option[Duration]): SqlCommand =
    new SqlCommand(pieces, timeout)
end SqlCommand

object SqlCommand:
  private[saferis] def apply(pieces: Chunk[SqlPiece], timeout: Option[Duration]): SqlCommand =
    new SqlCommand(pieces, timeout)

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
      case param: SqlPiece.Param =>
        flush()
        b += param
    flush()
    b.result()
  end merge

  def render(pieces: Chunk[SqlPiece], placeholder: (Int, SqlType) => String): SqlText =
    val sb = new StringBuilder
    pieces.foldLeft(1): (n, piece) =>
      piece match
        case SqlPiece.Text(t) =>
          sb.append(t)
          n
        case SqlPiece.Param(value) =>
          sb.append(placeholder(n, value.sqlType))
          n + 1
    SqlText(sb.toString)
  end render

  def show(pieces: Chunk[SqlPiece]): SqlText =
    val sb = new StringBuilder
    pieces.foreach:
      case SqlPiece.Text(t)      => sb.append(t)
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
  def postgres(pieces: Chunk[SqlPiece]): SqlText =
    render(pieces, (n, _) => s"$$$n")
end SqlPieces
