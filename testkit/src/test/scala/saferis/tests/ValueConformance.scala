package saferis.tests

import saferis.*

import zio.{test as _, *}
import zio.test.*

import java.time.Instant
import java.time.LocalDate
import java.util.UUID

/** Values both drivers have to bind and read the same way. */
object ValueConformance:
  enum Mood:
    case sad, ok

  given mood: Codec[Mood]                             = Codec.pgEnum[Mood]("mood")
  given ints: Codec[Chunk[Int]]                       = Codec.array[Int]
  given intOptions: Codec[Chunk[Option[Int]]]         = Codec.array[Option[Int]]
  given Decoder[Mood]                                 = mood
  given chunkInts: Decoder[Chunk[Int]]                = ints
  given chunkOpts: Decoder[Chunk[Option[Int]]]        = intOptions
  given Encoder[Mood]                                 = mood
  given Encoder[Chunk[Int]]                           = ints
  given chunkOptsEncoder: Encoder[Chunk[Option[Int]]] = intOptions

  def conformance =
    suite("values")(
      test("a Postgres enum round-trips through Other"):
        for
          _    <- sql"drop table if exists conformance_mood".dml
          _    <- sql"drop type if exists mood cascade".dml
          _    <- sql"create type mood as enum ('sad', 'ok')".dml
          _    <- sql"create table conformance_mood (id integer primary key, mood mood)".dml
          _    <- sql"insert into conformance_mood (id, mood) values (1, ${Mood.ok})".dml
          read <- sql"select mood from conformance_mood where id = 1".queryValue[Mood]
        yield assertTrue(read.contains(Mood.ok))
      ,
      test("an int4 array round-trips"):
        for
          _    <- sql"drop table if exists conformance_ints".dml
          _    <- sql"create table conformance_ints (id integer primary key, xs int4[])".dml
          _    <- sql"insert into conformance_ints (id, xs) values (1, ${Chunk(1, 2, 3)})".dml
          read <- sql"select xs from conformance_ints where id = 1".queryValue[Chunk[Int]]
        yield assertTrue(read.contains(Chunk(1, 2, 3)))
      ,
      test("a null array member round-trips"):
        for
          _    <- sql"drop table if exists conformance_ints_null".dml
          _    <- sql"create table conformance_ints_null (id integer primary key, xs int4[])".dml
          _    <- sql"insert into conformance_ints_null (id, xs) values (1, ${Chunk(Some(1), None)})".dml
          read <- sql"select xs from conformance_ints_null where id = 1".queryValue[Chunk[Option[Int]]]
        yield assertTrue(read.contains(Chunk(Some(1), None)))
      ,
      test("array membership matches dates, instants, uuids, and bytes"):
        val day    = LocalDate.parse("2024-01-01")
        val moment = Instant.parse("2024-01-01T00:00:00Z")
        val id     = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
        val bytes  = Chunk[Byte](1, 2, 3)
        for
          _ <- sql"drop table if exists conformance_members".dml
          _ <-
            sql"""create table conformance_members (
                    id integer primary key,
                    d date,
                    t timestamptz,
                    u uuid,
                    b bytea
                  )""".dml
          _ <-
            sql"""insert into conformance_members (id, d, t, u, b)
                  values (1, $day, $moment, $id, $bytes)""".dml
          dateHit <- sql"select id from conformance_members where d = any(${array(List(day))})".queryValue[Int]
          timeHit <- sql"select id from conformance_members where t = any(${array(List(moment))})".queryValue[Int]
          uuidHit <- sql"select id from conformance_members where u = any(${array(List(id))})".queryValue[Int]
          byteHit <- sql"select id from conformance_members where b = any(${array(List(bytes))})".queryValue[Int]
          missed  <-
            sql"select id from conformance_members where d = any(${array(List.empty[LocalDate])})".queryValue[Int]
        yield assertTrue(
          dateHit.contains(1),
          timeHit.contains(1),
          uuidHit.contains(1),
          byteHit.contains(1),
          missed.isEmpty,
        )
        end for
      ,
      test("a listener sees user statements and not transaction control"):
        for
          events <- Ref.make(Chunk.empty[SqlExecuted])
          raw    <- ZIO.service[SqlSession]
          listener = new SqlListener:
            def executed(event: SqlExecuted): UIO[Unit] = events.update(_ :+ event)
          observed = SqlListener.observe(listener)(raw)
          _     <- observed.transact(sql"select 1".queryValue[Int])
          seen  <- events.get
          fiber <- sql"select pg_sleep(30)".execute.provide(ZLayer.succeed(observed)).fork
          _     <- ZIO.sleep(1.second)
          _     <- fiber.interrupt
          after <- events.get
        yield
          val sqls = seen.map(_.sql.toLowerCase)
          assertTrue(
            sqls.exists(_.contains("select 1")),
            sqls.forall(sql => !sql.contains("begin") && !sql.contains("commit") && !sql.contains("set local")),
            after.exists(_.outcome == StatementOutcome.Interrupted),
          ),
    )
end ValueConformance
