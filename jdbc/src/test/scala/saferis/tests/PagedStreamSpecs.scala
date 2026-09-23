package saferis.tests

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.postgres.given
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*

object PagedStreamSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  @tableName("paged_stream_users")
  final case class PagedUser(@generated @key id: Int, name: String, age: Int) derives Table

  val userTable = Table[PagedUser]

  val spec = suite("Paged Stream Specs")(
    suite("pagedStream")(
      test("returns pages with correct size") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 10)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          pages <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 3).runCollect
        yield assertTrue(pages.size == 4) &&
          assertTrue(pages(0).items.size == 3) &&
          assertTrue(pages(1).items.size == 3) &&
          assertTrue(pages(2).items.size == 3) &&
          assertTrue(pages(3).items.size == 1)
      },
      test("provides correct cursor for checkpointing") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 15)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          pages <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 5).runCollect
          // Get cursor from first page
          firstPageCursor = pages(0).cursor
          // Resume from cursor
          remainingPages <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 5, startAfter = firstPageCursor).runCollect
        yield assertTrue(pages.size == 3) &&
          assertTrue(firstPageCursor.isDefined) &&
          assertTrue(remainingPages.size == 2) &&                // Should skip first page
          assertTrue(remainingPages.flatMap(_.items).size == 10) // 15 - 5 = 10 remaining
      },
      test("page numbers are correct") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 25)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          pages <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 10).runCollect
        yield assertTrue(pages(0).pageNumber == 0) &&
          assertTrue(pages(1).pageNumber == 1) &&
          assertTrue(pages(2).pageNumber == 2)
      },
      test("works with WHERE clauses") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 30)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          pages <-
            Query[PagedUser]
              .where(_.age)
              .gt(35)
              .pagedStream(_.id, pageSize = 5)
              .runCollect
        yield assertTrue(pages.flatMap(_.items).forall(_.age > 35))
      },
      test("empty result returns empty stream") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          pages <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 10).runCollect
        yield assertTrue(pages.isEmpty)
      },
    ),
    suite("pagedStream.items extension")(
      test("flattens pages to individual items") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 15)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          items <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 5).items.runCollect
        yield assertTrue(items.size == 15)
      },
      test("items are emitted in order") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 10)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          items <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 3).items.runCollect
          ids = items.map(_.id)
        yield assertTrue(ids == ids.sorted)
      },
    ),
    suite("pagedStream.itemsWithCheckpoint extension")(
      test("calls checkpoint callback after each page") {
        for
          _           <- (dropTable[PagedUser](ifExists = true))
          _           <- (createTable[PagedUser]())
          _           <- ZIO.foreachDiscard(1 to 15)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          checkpoints <- Ref.make(Chunk.empty[Option[Int]])
          _           <-
            Query[PagedUser].all
              .pagedStream(_.id, pageSize = 5)
              .itemsWithCheckpoint(cursor => checkpoints.update(_ :+ cursor))
              .runDrain
          recorded <- checkpoints.get
        yield assertTrue(recorded.size == 3) // 3 pages
      }
    ),
    suite("seekingStream")(
      test("streams individual rows") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 25)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          items <-
            Query[PagedUser].all.seekingStream(_.id, batchSize = 10).runCollect
        yield assertTrue(items.size == 25)
      },
      test("respects startAfter cursor") {
        for
          _ <- (dropTable[PagedUser](ifExists = true))
          _ <- (createTable[PagedUser]())
          _ <- ZIO.foreachDiscard(1 to 20)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          // Get first 5 items to find a cursor
          firstBatch <-
            Query[PagedUser].all.orderBy(userTable.id.asc).limit(5).query[PagedUser]
          cursor = firstBatch.lastOption.map(_.id)
          // Resume from cursor
          remaining <-
            Query[PagedUser].all.seekingStream(_.id, batchSize = 10, startAfter = cursor).runCollect
        yield assertTrue(remaining.size == 15) && // 20 - 5 = 15
          assertTrue(remaining.forall(_.id > cursor.get))
      },
      test("works with take() for partial consumption") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 100)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          items <-
            Query[PagedUser].all.seekingStream(_.id, batchSize = 10).take(15).runCollect
        yield assertTrue(items.size == 15)
      },
      test("works with WHERE clauses") {
        for
          _     <- (dropTable[PagedUser](ifExists = true))
          _     <- (createTable[PagedUser]())
          _     <- ZIO.foreachDiscard(1 to 30)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          items <-
            Query[PagedUser]
              .where(_.age)
              .lte(35)
              .seekingStream(_.id, batchSize = 5)
              .runCollect
        yield assertTrue(items.forall(_.age <= 35))
      },
    ),
    suite("Resource Safety")(
      test("connection released between pages") {
        for
          _ <- (dropTable[PagedUser](ifExists = true))
          _ <- (createTable[PagedUser]())
          _ <- ZIO.foreachDiscard(1 to 30)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          // Run paged stream
          _ <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 10).runDrain
          // If connection wasn't released, this would fail
          count <-
            Query[PagedUser].all.query[PagedUser].map(_.size)
        yield assertTrue(count == 30)
      },
      test("connection released on early termination") {
        for
          _ <- (dropTable[PagedUser](ifExists = true))
          _ <- (createTable[PagedUser]())
          _ <- ZIO.foreachDiscard(1 to 100)(i => (insert(PagedUser(0, s"User$i", 20 + i))))
          // Take only first page
          _ <-
            Query[PagedUser].all.pagedStream(_.id, pageSize = 10).take(1).runDrain
          // If connection wasn't released, this would fail
          count <-
            Query[PagedUser].all.query[PagedUser].map(_.size)
        yield assertTrue(count == 100)
      },
    ),
  ).provideShared(xaLayer) @@ TestAspect.sequential

end PagedStreamSpecs
