package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.stream.*
import zio.test.*

object PagedStreaming extends SaferisDocSpecSuite:

  @tableName("paged_streaming_events")
  case class PagedEvent(@generated @key id: Int, name: String, processed: Boolean) derives Table

  val events = Table[PagedEvent]

  // Create the table and insert five rows. Returns a ZIO so each example can
  // reuse it before running its own paged query.
  def seedEvents =
    for
      _ <- ddl.createTable[PagedEvent](ifNotExists = true)
      _ <- ddl.truncateTable[PagedEvent]()
      _ <- dml.insert(PagedEvent(-1, "event1", false))
      _ <- dml.insert(PagedEvent(-1, "event2", false))
      _ <- dml.insert(PagedEvent(-1, "event3", false))
      _ <- dml.insert(PagedEvent(-1, "event4", false))
      _ <- dml.insert(PagedEvent(-1, "event5", false))
    yield ()

  @tableName("paged_streaming_large_dataset")
  case class LargeRow(@generated @key id: Int, data: String) derives Table

  def processRow(row: LargeRow): UIO[Unit] =
    ZIO.unit // your per-row work

  def doc = page("Paged Streaming")(
    md"""Paged streaming provides **cursor-based pagination with automatic connection release between pages**. Unlike `queryStream` which holds a single connection open for the entire stream, paged streaming fetches data in batches and releases the connection after each batch. This makes it ideal for:

- **Long-running processing** - Process millions of rows without holding database connections
- **Resumable operations** - Checkpoint your position and resume after failures
- **Resource efficiency** - Free connections for other operations between page fetches
- **Backpressure-aware batching** - Control memory usage with configurable page sizes

These examples share a `seedEvents` helper that creates the table and inserts five
rows, so each example below can populate its own data before running its paged
query.""",
    section("pagedStream - Cursor-Based Pagination")(
      md"""Use `pagedStream` to get a stream of `Page[A, K]` objects, each containing a batch of items and a cursor for checkpointing:""",
      exampleZIO {
        xa.run(for
          _ <- seedEvents
          // Fetch in pages of 2
          pages <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2)
            .runCollect
        yield pages.map(p => s"Page ${p.pageNumber}: ${p.items.size} items, cursor=${p.cursor}"))
          .either
      }.assert {
        case Right(pages) =>
          assertTrue(
            pages.size == 3,
            pages(0).startsWith("Page 0: 2 items, cursor=Some("),
            pages(1).startsWith("Page 1: 2 items, cursor=Some("),
            pages(2).startsWith("Page 2: 1 items, cursor=Some("),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
      md"""Each `Page[A, K]` contains:
- `items: Chunk[A]` - The rows in this page
- `cursor: Option[K]` - Last key on this page (use with `startAfter` to resume)
- `pageNumber: Int` - 0-indexed page number

The stream ends after a partial page (fewer than `pageSize` items) or an empty fetch.""",
    ),
    section("Checkpointing for Resumable Processing")(
      md"""The cursor in each page enables resumable processing - save the cursor, and if processing fails, resume from where you left off:""",
      exampleZIO {
        xa.run(for
          _ <- seedEvents
          // Simulate processing with checkpoint storage
          checkpoint <- Ref.make(Option.empty[Int])

          // Process pages and save cursor after each
          _ <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2)
            .tap(page => checkpoint.set(page.cursor))
            .runDrain

          // Get final checkpoint
          finalCursor <- checkpoint.get
        yield s"Final cursor: $finalCursor")
          .either
      }.assert {
        case Right(msg) => assertTrue(msg.startsWith("Final cursor: Some("))
        case Left(err)  => assertTrue(false).label(err.message)
      },
    ),
    section("Resuming from a Checkpoint")(
      md"""Use `startAfter` to resume processing from a saved cursor:""",
      exampleZIO {
        xa.run(for
          _ <- seedEvents
          // First, process first page only
          firstPage <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2)
            .runHead

          // Save the cursor
          savedCursor = firstPage.flatMap(_.cursor)

          // Later, resume from the checkpoint
          remaining <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2, startAfter = savedCursor)
            .runCollect
        yield (s"Saved cursor: $savedCursor", s"Remaining pages: ${remaining.size}"))
          .either
      }.assert {
        case Right((saved, remaining)) =>
          assertTrue(saved.startsWith("Saved cursor: Some("), remaining == "Remaining pages: 2")
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("seekingStream - Row-by-Row with Batched Fetching")(
      md"""If you don't need page metadata, use `seekingStream` to get individual items while still releasing connections between batches:""",
      exampleZIO {
        xa.run(for
          _      <- seedEvents
          result <- Query[PagedEvent].all
            .seekingStream(_.id, batchSize = 2)
            .runCollect
            .map(items => s"Got ${items.size} items")
        yield result)
          .either
      }.assert {
        case Right(result) => assertTrue(result == "Got 5 items")
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Flattening Pages to Items")(
      md"""Use the `.items` extension to flatten a paged stream to individual items:""",
      exampleZIO {
        xa.run(for
          _      <- seedEvents
          result <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2)
            .items // Flatten to individual items
            .runCollect
            .map(items => s"Got ${items.size} items")
        yield result)
          .either
      }.assert {
        case Right(result) => assertTrue(result == "Got 5 items")
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Items with Checkpoint Callback")(
      md"""Use `.itemsWithCheckpoint` to process items individually while receiving a callback after each page completes:""",
      exampleZIO {
        xa.run(for
          _           <- seedEvents
          checkpoints <- Ref.make(Chunk.empty[Option[Int]])

          items <- Query[PagedEvent].all
            .pagedStream(_.id, pageSize = 2)
            .itemsWithCheckpoint(cursor => checkpoints.update(_ :+ cursor))
            .runCollect

          recorded <- checkpoints.get
        yield (s"Processed ${items.size} items", s"Checkpoints: ${recorded.toList}"))
          .either
      }.assert {
        case Right((processed, checkpoints)) =>
          assertTrue(
            processed == "Processed 5 items",
            checkpoints.startsWith("Checkpoints: List("),
            checkpoints.contains("Some("),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("pagedStream vs queryStream")(
      md"""| Feature | `queryStream` | `pagedStream` / `seekingStream` |
|---------|--------------|--------------------------------|
| Connection usage | Held for entire stream | Released between pages |
| Memory per page | Single row buffer | Full page buffered |
| Checkpoint support | No | Yes (cursor in each page) |
| Best for | Real-time, low-latency | Long-running, resumable jobs |
| Early termination | Connection released | Connection already released |"""
    ),
    section("Combining with WHERE Clauses")(
      md"""Paged streaming works with all query builder methods:""",
      exampleZIO {
        xa.run(for
          _      <- seedEvents
          result <- Query[PagedEvent]
            .where(_.processed)
            .eq(false)
            .pagedStream(_.id, pageSize = 10)
            .items
            .runCollect
            .map(items => s"Unprocessed: ${items.size}")
        yield result)
          .either
      }.assert {
        case Right(result) => assertTrue(result == "Unprocessed: 5")
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Resource Safety")(
      md"""Paged streams release the database connection after fetching each page. This prevents connection pool exhaustion during long-running operations. The program below runs a full checkpointed pipeline against the database:""",
      exampleZIO {
        // The xa.run workflow fails with SaferisError; the final console print is a
        // separate effect, so we sequence it after the run with *>.
        (xa.run(for
          _ <- ddl.createTable[LargeRow](ifNotExists = true)
          _ <- ddl.truncateTable[LargeRow]()
          _ <- ZIO.foreachDiscard(1 to 5)(i => dml.insert(LargeRow(-1, s"row-$i")))
          // Process rows in pages of 2, checkpointing after each page; the connection
          // is released between pages so a long job never exhausts the pool.
          _ <- Query[LargeRow].all
            .pagedStream(_.id, pageSize = 2)
            .itemsWithCheckpoint(cursor => Console.printLine(s"Checkpoint: $cursor").orDie)
            .foreach(processRow)
        yield ()) *> Console.printLine("Processed all rows")).either
      }.assert {
        case Right(_)  => assertTrue(true)
        case Left(err) => assertTrue(false).label(err.toString)
      },
    ),
  )
end PagedStreaming
