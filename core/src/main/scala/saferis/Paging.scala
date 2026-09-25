package saferis

import zio.*
import zio.stream.ZStream

/** A page of results with cursor information for checkpointing.
  *
  * Used by `pagedStream` to provide cursor tracking for resumable processing. Each page contains items fetched in a
  * single query, with the cursor pointing to the next page.
  *
  * @param items
  *   The rows in this page
  * @param cursor
  *   Cursor value to fetch next page (None if this is the last page)
  * @param pageNumber
  *   0-indexed page number
  */
final case class Page[A, K](
    items: Chunk[A],
    cursor: Option[K],
    pageNumber: Int,
)

/** Extensions for Page streams.
  *
  * Note: These extensions emit items lazily to downstream consumers, but each page is buffered internally (required to
  * release the connection between pages). For truly unbuffered row-by-row streaming, use `queryStream` instead.
  */
extension [R, A, K](stream: ZStream[R, SaferisError, Page[A, K]])
  /** Flatten pages to individual items.
    *
    * Items are emitted lazily to downstream consumers. Use this when you want connection-release-between-pages behavior
    * but don't need cursor information for checkpointing.
    */
  def items: ZStream[R, SaferisError, A] =
    stream.flatMap(page => ZStream.fromChunk(page.items))

  /** Flatten pages to individual items with checkpoint callback.
    *
    * The callback is invoked after all items from each page are emitted, allowing you to track cursor position for
    * resumable processing.
    */
  def itemsWithCheckpoint(onPageComplete: Option[K] => UIO[Unit]): ZStream[R, SaferisError, A] =
    stream.flatMap: page =>
      ZStream.fromChunk(page.items) ++ ZStream.fromZIO(onPageComplete(page.cursor)).drain
end extension

/** Helper object for paged stream operations.
  *
  * These methods use inline to ensure proper type information is available for macros.
  */
object PagedStreamOps:
  /** Create a paged stream from a Query1Ready
    *
    * State: (cursor, pageNum, done) - done flag prevents infinite loop when last page is partial
    */
  inline def pagedStream[A: Table, K](
      ready: Query1Ready[A],
      column: Column[K],
      extractCursor: A => K,
      pageSize: Int,
      startAfter: Option[K],
  )(using enc: Encoder[K], trace: Trace): ZStream[SqlSession, SaferisError, Page[A, K]] =
    ZStream.unfoldZIO((startAfter, 0, false)) { case (cursor, pageNum, done) =>
      if done then ZIO.succeed(None) // We've already emitted the last page
      else
        val query = cursor match
          case Some(c) => ready.seekAfter(column, c)(using enc).limit(pageSize)
          case None    => ready.orderBy(column.asc).limit(pageSize)
        query.build.query[A].map { items =>
          if items.isEmpty then None
          else
            val nextCursor = items.lastOption.map(extractCursor)
            val page       = Page(items, nextCursor, pageNum)
            val hasMore    = items.size == pageSize
            if hasMore then Some((page, (nextCursor, pageNum + 1, false)))
            else Some((page, (None, pageNum + 1, true))) // Mark done after emitting partial page
        }
    }

  /** Create a seeking stream from a Query1Ready
    *
    * State: (cursor, done) - done flag prevents infinite loop when last batch is partial
    */
  inline def seekingStream[A: Table, K](
      ready: Query1Ready[A],
      column: Column[K],
      extractCursor: A => K,
      batchSize: Int,
      startAfter: Option[K],
  )(using enc: Encoder[K], trace: Trace): ZStream[SqlSession, SaferisError, A] =
    ZStream.unfoldChunkZIO((startAfter, false)) { case (cursor, done) =>
      if done then ZIO.succeed(None) // We've already emitted the last batch
      else
        val query = cursor match
          case Some(c) => ready.seekAfter(column, c)(using enc).limit(batchSize)
          case None    => ready.orderBy(column.asc).limit(batchSize)
        query.build.query[A].map { items =>
          if items.isEmpty then None
          else
            val nextCursor = items.lastOption.map(extractCursor)
            val hasMore    = items.size == batchSize
            if hasMore then Some((items, (nextCursor, false)))
            else Some((items, (None, true))) // Mark done after emitting partial batch
        }
    }
end PagedStreamOps

/** Sort direction for ORDER BY clauses */
enum SortOrder:
  case Asc, Desc

/** Null ordering preference in sorted results */
enum NullOrder:
  case First, Last, Default

/** Seek direction for cursor-based pagination */
enum SeekDir:
  /** Greater than (>) - forward pagination, get items AFTER cursor */
  case Gt

  /** Less than (<) - backward pagination, get items BEFORE cursor */
  case Lt

/** Type-safe sort specification using Column[T]
  *
  * @param column
  *   The column to sort by
  * @param order
  *   Sort direction (Asc or Desc)
  * @param nullOrder
  *   Where to place NULL values in results
  */
final case class Sort[T](
    column: Column[T],
    order: SortOrder = SortOrder.Asc,
    nullOrder: NullOrder = NullOrder.Default,
):
  /** Generate the SQL fragment for this sort specification */
  def toSqlFragment: SqlFragment =
    val orderStr = order match
      case SortOrder.Asc  => " asc"
      case SortOrder.Desc => " desc"
    val nullStr = nullOrder match
      case NullOrder.First   => " nulls first"
      case NullOrder.Last    => " nulls last"
      case NullOrder.Default => ""
    SqlFragment.text(s"${column.sql}$orderStr$nullStr")
end Sort

/** Type-safe seek specification for cursor-based pagination
  *
  * @param column
  *   The column to seek on (usually the primary key or indexed column)
  * @param direction
  *   Forward (Gt) or backward (Lt)
  * @param value
  *   The cursor value to seek from
  * @param sortOrder
  *   How to sort the seek column
  * @param nullOrder
  *   Where to place NULL values
  */
final case class Seek[T: Encoder](
    column: Column[T],
    direction: SeekDir,
    value: T,
    sortOrder: SortOrder = SortOrder.Asc,
    nullOrder: NullOrder = NullOrder.Default,
):
  /** Generate the WHERE predicate for this seek specification */
  def toWherePredicate: SqlFragment =
    val op = direction match
      case SeekDir.Gt => ">"
      case SeekDir.Lt => "<"
    val encoder = summon[Encoder[T]]
    SqlFragment.text(s"${column.sql} $op ").append(SqlFragment.param(encoder.encode(value)))

  /** Generate a Sort from this seek specification */
  def toSort: Sort[T] = Sort(column, sortOrder, nullOrder)
end Seek

// Column extensions for fluent sorting and seeking
extension [T](column: Column[T])
  // Sort extensions (combined style)
  def asc: Sort[T]            = Sort(column, SortOrder.Asc)
  def desc: Sort[T]           = Sort(column, SortOrder.Desc)
  def ascNullsFirst: Sort[T]  = Sort(column, SortOrder.Asc, NullOrder.First)
  def ascNullsLast: Sort[T]   = Sort(column, SortOrder.Asc, NullOrder.Last)
  def descNullsFirst: Sort[T] = Sort(column, SortOrder.Desc, NullOrder.First)
  def descNullsLast: Sort[T]  = Sort(column, SortOrder.Desc, NullOrder.Last)

  // Seek extensions
  def gt(value: T)(using Encoder[T]): Seek[T] = Seek(column, SeekDir.Gt, value)
  def lt(value: T)(using Encoder[T]): Seek[T] = Seek(column, SeekDir.Lt, value)
end extension
