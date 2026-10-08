package saferis

import zio.Chunk
import zio.Duration
import zio.IO
import zio.Trace
import zio.ZIO
import zio.stream.ZStream

/** A statement as text pieces and parameters, plus construction issues and an optional per-statement timeout. */
final class SqlFragment private (
    private val stored: Chunk[SqlPiece],
    val issues: List[FragmentIssue],
    override val timeout: Option[Duration],
) extends Placeholder:

  private[saferis] def rawPieces: Chunk[SqlPiece] = stored

  /** `$1`, `$2`, with identifiers quoted for the dialect in scope. Not the text a JDBC driver sends. */
  override def sql(using Dialect): SqlText = SqlPieces.postgres(stored, summon[Dialect].identifierQuote)

  def show(using dialect: Dialect): SqlText = SqlPieces.show(stored, dialect.identifierQuote)

  def withTimeout(d: Duration): SqlFragment =
    new SqlFragment(stored, issues, Some(d))

  def stripMargin: SqlFragment = stripMargin('|')

  def stripMargin(marginChar: Char): SqlFragment =
    new SqlFragment(SqlFragment.stripPieces(stored, marginChar), issues, timeout)

  def append(other: SqlFragment): SqlFragment =
    new SqlFragment(
      SqlPieces.merge(stored ++ other.stored),
      issues ++ other.issues,
      timeout.orElse(other.timeout),
    )

  def :+(other: SqlFragment): SqlFragment = append(other)

  def validate(using Trace): IO[SaferisError, SqlFragment] =
    val all = allIssues
    if all.isEmpty then ZIO.succeed(this)
    else ZIO.fail(SaferisError.InvalidStatement(all))

  /** Fails with `InvalidStatement` before a connection is checked out. Timeout is this fragment, else the fiber ref.
    * The dialect's quote character is stored on the command so a driver quotes `Ident` the same way `sql` does.
    */
  def toCommand(using dialect: Dialect)(using Trace): IO[SaferisError, SqlCommand] =
    val all = allIssues
    if all.nonEmpty then ZIO.fail(SaferisError.InvalidStatement(all))
    else
      Saferis.timeoutFiberRef.get.map: aspect =>
        SqlCommand(stored, timeout.orElse(aspect), dialect.identifierQuote)

  private def allIssues: List[FragmentIssue] =
    issues ++ SqlPieces.arrayIssues(stored)

  inline def query[E](using table: Table[E])(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Chunk[E]] =
    val read = SqlFragment.readTable[E]
    for
      command <- toCommand
      rows    <- ZIO.serviceWithZIO[SqlSession](_.query(command)(read))
    yield rows

  inline def queryOne[E](using table: Table[E])(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Option[E]] =
    val read = SqlFragment.readTable[E]
    for
      command <- toCommand
      row     <- ZIO.serviceWithZIO[SqlSession](_.queryAtMostOne(command)(read))
    yield row

  inline def queryValue[A](using
      decoder: RowDecoder[A]
  )(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Option[A]] =
    val read: SqlRow => Either[SaferisError, A] = row =>
      decoder
        .decode(row)
        .left
        .map: err =>
          val column = if row.width == 1 then row.labels.headOption.getOrElse("0") else "0"
          SaferisError.DecodingError(ColumnName(column), TypeName("value"), err)
    for
      command <- toCommand
      row     <- ZIO.serviceWithZIO[SqlSession](_.queryAtMostOne(command)(read))
    yield row
  end queryValue

  inline def queryStream[E](using table: Table[E])(using Dialect)(using Trace): ZStream[SqlSession, SaferisError, E] =
    val read = SqlFragment.readTable[E]
    ZStream.unwrap:
      toCommand.map: command =>
        ZStream.serviceWithStream[SqlSession](_.stream(command)(read))

  def update(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Long]  = dml
  def delete(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Long]  = dml
  def insert(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Long]  = dml
  def execute(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Long] = dml

  def dml(using Dialect)(using Trace): ZIO[SqlSession, SaferisError, Long] =
    for
      command <- toCommand
      count   <- ZIO.serviceWithZIO[SqlSession](_.exec(command))
    yield count

end SqlFragment

object SqlFragment:
  def apply(
      pieces: Chunk[SqlPiece],
      issues: List[FragmentIssue] = Nil,
      timeout: Option[Duration] = None,
  ): SqlFragment =
    new SqlFragment(SqlPieces.merge(pieces), issues, timeout)

  def apply(placeholder: Placeholder): SqlFragment =
    new SqlFragment(SqlPieces.merge(placeholder.rawPieces), placeholder.issues, placeholder.timeout)

  def ident[A](name: A)(using sqlName: SqlName[A]): SqlFragment =
    one(SqlPiece.Ident(sqlName.text(name), sqlName.qualify))

  /** `alias.column`, or just the column when there is no alias. */
  def columnRef(alias: Option[Alias], column: ColumnName): SqlFragment =
    alias match
      case None    => ident(column)
      case Some(a) => ident(a).append(text(".")).append(ident(column))

  /** `table` or `table as alias`. */
  def tableRef(name: TableName, alias: Option[Alias]): SqlFragment =
    alias match
      case None    => ident(name)
      case Some(a) => ident(name).append(text(" as ")).append(ident(a))

  private def one(piece: SqlPiece): SqlFragment =
    new SqlFragment(Chunk(piece), Nil, None)

  val empty: SqlFragment = new SqlFragment(Chunk.empty, Nil, None)

  /** Caller-trusted SQL text, never user data. This is where a `String` becomes SQL. */
  def text(sql: String): SqlFragment =
    if sql.isEmpty then empty else new SqlFragment(Chunk(SqlPiece.Text(SqlText(sql))), Nil, None)

  def param(value: SqlValue): SqlFragment =
    new SqlFragment(Chunk(SqlPiece.Param(value)), Nil, None)

  private[saferis] def interpolate(parts: Seq[String], holders: Seq[Placeholder]): SqlFragment =
    // `StringContext.parts` still contains Scala escapes. `StringContext.s` used to decode them.
    val decoded                   = parts.map(StringContext.processEscapes)
    val b                         = Chunk.newBuilder[SqlPiece]
    val issues                    = List.newBuilder[FragmentIssue]
    var timeout: Option[Duration] = None
    var i                         = 0
    while i < holders.length do
      if i < decoded.length then b += SqlPiece.Text(SqlText(decoded(i)))
      b ++= holders(i).rawPieces
      issues ++= holders(i).issues
      timeout = timeout.orElse(holders(i).timeout)
      i += 1
    if i < decoded.length then b += SqlPiece.Text(SqlText(decoded(i)))
    new SqlFragment(SqlPieces.merge(b.result()), issues.result(), timeout)
  end interpolate

  inline def readTable[E](using table: Table[E]): SqlRow => Either[SaferisError, E] = row =>
    val decoded = table.columns.foldLeft[Either[SaferisError, List[(String, Any)]]](Right(Nil)): (acc, col) =>
      acc.flatMap(pairs => col.read(row).map(pair => pair :: pairs))
    decoded.flatMap: pairs =>
      Macros
        .make[E](pairs.reverse)
        .left
        .map: err =>
          SaferisError.DecodingError(ColumnName(table.name), TypeName("row"), err)

  /** `String.stripMargin`, walking text only. A parameter is content, so it ends the margin scan for that line. */
  private def stripPieces(pieces: Chunk[SqlPiece], marginChar: Char): Chunk[SqlPiece] =
    val out         = Chunk.newBuilder[SqlPiece]
    val pending     = new StringBuilder
    val leading     = new StringBuilder
    var atLineStart = true

    def emit(text: String): Unit =
      if text.nonEmpty then pending.append(text)

    def flush(): Unit =
      if pending.nonEmpty then
        out += SqlPiece.Text(SqlText(pending.toString))
        pending.clear()

    def commitLeading(): Unit =
      if leading.nonEmpty then
        emit(leading.toString)
        leading.clear()

    def onChar(c: Char): Unit =
      if c == '\n' then
        if atLineStart then commitLeading()
        emit("\n")
        atLineStart = true
      else if atLineStart then
        if c <= ' ' then leading.append(c)
        else if c == marginChar then
          leading.clear()
          atLineStart = false
        else
          commitLeading()
          emit(c.toString)
          atLineStart = false
      else emit(c.toString)

    pieces.foreach:
      case SqlPiece.Text(text) =>
        text.foreach(onChar)
      case ident: SqlPiece.Ident =>
        if atLineStart then
          commitLeading()
          atLineStart = false
        flush()
        out += ident
      case param: SqlPiece.Param =>
        if atLineStart then
          commitLeading()
          atLineStart = false
        flush()
        out += param
    if atLineStart then commitLeading()
    flush()
    SqlPieces.merge(out.result())
  end stripPieces
end SqlFragment
