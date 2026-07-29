package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.stream.*
import zio.test.*

object Streaming extends SaferisDocSpecSuite:

  @tableName("streaming_stream_events")
  case class StreamEvent(@generated @key id: Int, name: String, payload: String) derives Table

  val events = Table[StreamEvent]

  @tableName("streaming_zip_users")
  case class ZipUser(@generated @key id: Int, name: String) derives Table

  @tableName("streaming_zip_items")
  case class ZipItem(@generated @key id: Int, value: String) derives Table

  val users = Table[ZipUser]
  val items = Table[ZipItem]

  def doc = page("Streaming with ZStream")(
    md"""For large result sets, Saferis provides `queryStream` which returns a `ZStream` that lazily iterates through results. This is ideal when you need to process rows one at a time without loading the entire result set into memory.""",
    section("Basic Streaming")(
      md"""Use `queryStream` instead of `query` to get a stream:""",
      exampleZIO {
        xa.run(for
          _ <- ddl.createTable[StreamEvent](ifNotExists = true)
          _ <- ddl.truncateTable[StreamEvent]()
          _ <- dml.insert(StreamEvent(-1, "event1", "data1"))
          _ <- dml.insert(StreamEvent(-1, "event2", "data2"))
          _ <- dml.insert(StreamEvent(-1, "event3", "data3"))
          // Stream returns a ZStream, use runCollect to materialize
          result <- Query[StreamEvent].all.queryStream[StreamEvent].runCollect
        yield result)
          .either
      }.assert {
        case Right(result) => assertTrue(result.size == 3 && result.exists(_.name == "event1"))
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Stream vs Eager Query")(
      md"""Both `query` and `queryStream` return the same data, but with different memory characteristics:

| Method | Return Type | Memory Usage | Best For |
|--------|-------------|--------------|----------|
| `.query[T]` | `Chunk[T]` | All rows loaded at once | Small to medium result sets |
| `.queryStream[T]` | `ZStream[..., T]` | One row at a time | Large result sets, real-time processing |"""
    ),
    section("Lazy Evaluation")(
      md"""Streams are evaluated lazily - rows are only fetched as they're consumed:""",
      exampleZIO {
        xa.run(
          for
            _ <- ddl.createTable[StreamEvent](ifNotExists = true)
            _ <- ddl.truncateTable[StreamEvent]()
            _ <- dml.insert(StreamEvent(-1, "lazy1", "data"))
            _ <- dml.insert(StreamEvent(-1, "lazy2", "data"))
            _ <- dml.insert(StreamEvent(-1, "lazy3", "data"))
            // Only fetches 2 rows from the database, even though more exist
            first2 <- Query[StreamEvent].all.queryStream[StreamEvent].take(2).runCollect
          yield first2
        ).either
      }.assert {
        case Right(first2) => assertTrue(first2.size == 2)
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Stream Composition")(
      md"""ZStream provides powerful composition operators:""",
      exampleZIO {
        xa.run(
          for
            _ <- ddl.createTable[StreamEvent](ifNotExists = true)
            _ <- ddl.truncateTable[StreamEvent]()
            _ <- dml.insert(StreamEvent(-1, "event1", "data1"))
            _ <- dml.insert(StreamEvent(-1, "event2", "data2"))
            _ <- dml.insert(StreamEvent(-1, "other", "data3"))
            // Map, filter, and transform streams
            names <- Query[StreamEvent].all
              .queryStream[StreamEvent]
              .map(_.name)
              .filter(_.startsWith("event"))
              .runCollect

            // Batch processing with grouped
            batches <- Query[StreamEvent].all
              .queryStream[StreamEvent]
              .grouped(2)
              .runCollect
          yield (names, batches.map(_.size))
        ).either
      }.assert {
        case Right((names, batchSizes)) =>
          assertTrue(
            names.toSet == Set("event1", "event2"),
            batchSizes == Chunk(2, 1),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("Resource Safety")(
      md"""The database connection is automatically released when the stream completes, errors, or is interrupted:

```scala
import saferis.*
import zio.*
import zio.stream.*

@tableName("streaming_resource_events")
case class ResourceEvent(@generated @key id: Int, data: String) derives Table

// Connection released after stream fully consumed
Query[ResourceEvent].all.queryStream[ResourceEvent].runDrain

// Connection released after take(n) partial consumption
Query[ResourceEvent].all.queryStream[ResourceEvent].take(10).runDrain

// Connection released on stream interruption
val fiber = Query[ResourceEvent].all
  .queryStream[ResourceEvent]
  .tap(_ => ZIO.sleep(10.millis))
  .runDrain
  .fork
// fiber.interrupt releases the connection
```"""
    ),
    section("Streaming with Query Builder")(
      md"""All query builder methods support streaming:""",
      exampleZIO {
        xa.run(for
          _      <- ddl.createTable[StreamEvent](ifNotExists = true)
          _      <- ddl.truncateTable[StreamEvent]()
          _      <- dml.insert(StreamEvent(-1, "event1", "data1"))
          _      <- dml.insert(StreamEvent(-1, "event2", "data2"))
          result <- Query[StreamEvent]
            .where(_.name)
            .eq("event1")
            .orderBy(events.id.asc)
            .queryStream[StreamEvent]
            .runCollect
        yield result)
          .either
      }.assert {
        case Right(result) => assertTrue(result.size == 1 && result.head.name == "event1")
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("Streaming with Mutations (RETURNING)")(
      md"""For dialects that support RETURNING (PostgreSQL, SQLite), you can stream returned rows:""",
      exampleZIO {
        xa.run(for
          _       <- ddl.createTable[StreamEvent](ifNotExists = true)
          _       <- ddl.truncateTable[StreamEvent]()
          _       <- dml.insert(StreamEvent(-1, "event1", "data1"))
          _       <- dml.insert(StreamEvent(-1, "keep", "data2"))
          deleted <- Delete[StreamEvent]
            .where(_.name)
            .eq("event1")
            .returningAs
            .queryStream
            .runCollect
        yield deleted)
          .either
      }.assert {
        case Right(deleted) => assertTrue(deleted.size == 1 && deleted.head.name == "event1")
        case Left(err)      => assertTrue(false).label(err.message)
      },
    ),
    section("Combining Streams")(
      md"""You can compose streams from different queries:""",
      exampleZIO {
        xa.run(
          for
            _ <- ddl.createTable[ZipUser](ifNotExists = true)
            _ <- ddl.createTable[ZipItem](ifNotExists = true)
            _ <- ddl.truncateTable[ZipUser]()
            _ <- ddl.truncateTable[ZipItem]()
            _ <- dml.insert(ZipUser(-1, "Alice"))
            _ <- dml.insert(ZipUser(-1, "Bob"))
            _ <- dml.insert(ZipItem(-1, "Item A"))
            _ <- dml.insert(ZipItem(-1, "Item B"))
            // Zip two streams together
            zipped <-
              val userStream = Query[ZipUser].all.orderBy(users.name.asc).queryStream[ZipUser]
              val itemStream = Query[ZipItem].all.orderBy(items.value.asc).queryStream[ZipItem]
              userStream.zip(itemStream).runCollect
          yield zipped.map((u, i) => s"${u.name} -> ${i.value}")
        ).either
      }.assert {
        case Right(zipped) =>
          assertTrue(
            zipped.size == 2,
            zipped.contains("Alice -> Item A"),
            zipped.contains("Bob -> Item B"),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
  )
end Streaming
