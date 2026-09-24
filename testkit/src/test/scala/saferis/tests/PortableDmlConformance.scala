package saferis.tests

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.tests.SqlSessionConformance.withDialect

import zio.{test as _, *}
import zio.test.*

import java.time.Instant
import java.time.LocalDate

/** DDL and DML every database has to agree on, rendered by the target's dialect. Tables come from `createTable`, so the
  * dialect's column types are part of what is checked.
  */
object PortableDmlConformance:
  @tableName("portable_people")
  final case class Person(@key id: Int, name: String, age: Option[Int], active: Boolean) derives Table

  @tableName("portable_serial")
  final case class Serial(@generated @key id: Int, label: String) derives Table

  @tableName("portable_values")
  final case class Values(
      @key id: Int,
      big: Long,
      amount: BigDecimal,
      birthday: LocalDate,
      stamp: Instant,
      note: String,
  ) derives Table

  private val people = List(
    Person(1, "Ada", Some(36), true),
    Person(2, "Grace", None, false),
    Person(3, "Edsger", Some(72), true),
  )

  private def fresh[A: Table](using Dialect): ZIO[SqlSession, SaferisError, Unit] =
    dropTable[A](ifExists = true) *> createTable[A]().unit

  private def seeded(using Dialect): ZIO[SqlSession, SaferisError, Unit] =
    fresh[Person] *> ZIO.foreachDiscard(people)(p => insert(p))

  def conformance =
    suite("portable DML")(
      test("insert, read back, update, and delete a row"):
        withDialect:
          for
            _       <- fresh[Person]
            _       <- insert(Person(1, "Ada", Some(36), true))
            read    <- sql"select * from portable_people where id = ${1}".queryOne[Person]
            _       <- update(Person(1, "Ada Lovelace", None, false))
            updated <- sql"select * from portable_people where id = ${1}".queryOne[Person]
            _       <- delete(Person(1, "Ada Lovelace", None, false))
            gone    <- sql"select * from portable_people where id = ${1}".queryOne[Person]
          yield assertTrue(
            read.contains(Person(1, "Ada", Some(36), true)),
            updated.contains(Person(1, "Ada Lovelace", None, false)),
            gone.isEmpty,
          )
      ,
      test("a generated key is assigned by the database"):
        withDialect:
          for
            _    <- fresh[Serial]
            _    <- insert(Serial(0, "a"))
            _    <- insert(Serial(0, "b"))
            rows <- sql"select * from portable_serial order by id".query[Serial]
          yield assertTrue(rows.map(_.label) == Chunk("a", "b"), rows.map(_.id).distinct.size == 2)
      ,
      test("values round-trip through the dialect's column types"):
        val row = Values(
          1,
          Long.MaxValue,
          BigDecimal("12.34"),
          LocalDate.parse("2024-02-29"),
          Instant.parse("2024-01-02T03:04:05.123456Z"),
          "it's quoted",
        )
        withDialect:
          for
            _    <- fresh[Values]
            _    <- insert(row)
            read <- sql"select * from portable_values where id = ${1}".queryOne[Values]
          yield assertTrue(read.contains(row))
      ,
      test("the builder's inList, notInList, and empty lists agree with the data"):
        withDialect:
          for
            _       <- seeded
            some    <- Query[Person].where(_.id).inList(List(1, 3, 3)).query[Person]
            others  <- Query[Person].where(_.id).notInList(List(1, 3)).query[Person]
            none    <- Query[Person].where(_.id).inList(List.empty[Int]).query[Person]
            allRows <- Query[Person].where(_.id).notInList(List.empty[Int]).query[Person]
          yield assertTrue(
            some.map(_.id).toSet == Set(1, 3),
            others.map(_.id) == Chunk(2),
            none.isEmpty,
            allRows.size == 3,
          )
      ,
      test("a raw in(...) matches, and an empty in(...) fails before a connection"):
        withDialect:
          for
            _     <- seeded
            found <- sql"select * from portable_people where id in ${in(List(2, 3))}".query[Person]
            empty <- sql"select * from portable_people where id in ${in(List.empty[Int])}".query[Person].exit
          yield assertTrue(
            found.map(_.id).toSet == Set(2, 3),
            empty match
              case Exit.Failure(cause) =>
                cause.failureOption match
                  case Some(SaferisError.InvalidStatement(_)) => true
                  case _                                      => false
              case _ => false,
          )
      ,
      test("queryStream reads every row"):
        withDialect:
          for
            _    <- seeded
            rows <- sql"select * from portable_people order by id".queryStream[Person].runCollect
          yield assertTrue(rows.map(_.id) == Chunk(1, 2, 3))
      ,
      test("pagedStream pages through every row"):
        withDialect:
          for
            _     <- seeded
            pages <- Query[Person].all.pagedStream(_.id, pageSize = 2).runCollect
          yield assertTrue(
            pages.map(_.items.size) == Chunk(2, 1),
            pages.flatMap(_.items).map(_.id) == Chunk(1, 2, 3),
          )
      ,
      test("count(*) decodes as Int and as Long"):
        withDialect:
          for
            _     <- seeded
            asInt <- sql"select count(*) from portable_people".queryValue[Int]
            asLng <- sql"select count(*) from portable_people".queryValue[Long]
          yield assertTrue(asInt.contains(3), asLng.contains(3L)),
    ) @@ TestAspect.sequential
end PortableDmlConformance
