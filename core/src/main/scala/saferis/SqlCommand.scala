package saferis

import zio.Chunk
import zio.Duration

/** One piece of a statement. Text is caller SQL. Param is one bound value. Nothing scans the text for placeholders. */
enum SqlPiece:
  case Text(text: String)
  case Param(value: SqlValue)

/** A validated statement. No issue list: invalid fragments never become a command. */
final class SqlCommand private (
    val pieces: Chunk[SqlPiece],
    val timeout: Option[Duration],
):
  def render(placeholder: (Int, SqlType) => String): String =
    SqlPieces.render(pieces, placeholder)

  /** `$n` text with no casts and no bound values. The same string for every driver. */
  def inspection: String = SqlPieces.postgres(pieces)

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
        b += SqlPiece.Text(text.toString)
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

  def render(pieces: Chunk[SqlPiece], placeholder: (Int, SqlType) => String): String =
    val sb = new StringBuilder
    pieces.foldLeft(1): (n, piece) =>
      piece match
        case SqlPiece.Text(t) =>
          sb.append(t)
          n
        case SqlPiece.Param(value) =>
          sb.append(placeholder(n, value.sqlType))
          n + 1
    sb.toString
  end render

  def show(pieces: Chunk[SqlPiece]): String =
    val sb = new StringBuilder
    pieces.foreach:
      case SqlPiece.Text(t)      => sb.append(t)
      case SqlPiece.Param(value) => sb.append(SqlValue.literal(value))
    sb.toString

  /** Postgres inspection form. The first parameter is `$1`. */
  def postgres(pieces: Chunk[SqlPiece]): String =
    render(pieces, (n, _) => s"$$$n")
end SqlPieces
