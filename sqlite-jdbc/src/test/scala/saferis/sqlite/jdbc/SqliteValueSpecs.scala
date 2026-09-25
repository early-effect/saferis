package saferis.sqlite.jdbc

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.sqlite.given

import zio.{test as _, *}
import zio.json.*
import zio.test.*

import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

/** What SQLite does differently from the portable suite: values without a native storage class, 64-bit integers,
  * foreign keys that are on, and result codes mapped to the shared SQLSTATE vocabulary.
  */
object SqliteValueSpecs:
  final case class Meta(tags: List[String], version: Int) derives JsonCodec

  @tableName("lite_values")
  final case class Row(
      @key id: Int,
      small: Short,
      meta: Json[Meta],
      ref: UUID,
      local: LocalDateTime,
      clock: LocalTime,
      bytes: Chunk[Byte],
      ratio: Double,
  ) derives Table

  private def failure(exit: Exit[SaferisError, Any]): Option[SaferisError] = exit match
    case Exit.Failure(cause) => cause.failureOption
    case _                   => None

  def spec =
    suite("sqlite values")(
      test("json, uuid, datetime, time, bytes, a short, and a double round-trip through declared types"):
        val row = Row(
          1,
          7,
          Json(Meta(List("a", "b"), 2)),
          UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
          LocalDateTime.parse("2024-01-02T03:04:05.123456"),
          LocalTime.parse("13:14:15.654321"),
          Chunk[Byte](1, 2, 3),
          0.125,
        )
        for
          _    <- dropTable[Row](ifExists = true)
          _    <- createTable[Row]()
          _    <- insert(row)
          read <- sql"select * from lite_values where id = ${1}".queryOne[Row]
        yield assertTrue(read.contains(row))
      ,
      test("foreign keys are enforced, and a violation is a 23503 constraint violation"):
        for
          _ <- sql"drop table if exists lite_child".dml
          _ <- sql"drop table if exists lite_parent".dml
          _ <- sql"create table lite_parent (id integer primary key)".dml
          _ <-
            sql"create table lite_child (id integer primary key, parent_id integer references lite_parent (id))".dml
          exit <- sql"insert into lite_child (id, parent_id) values (1, 99)".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(SaferisError.ConstraintViolation("23503", _, _, _)) => true
            case _                                                        => false
      ,
      test("a not-null violation is 23502"):
        for
          _    <- sql"drop table if exists lite_not_null".dml
          _    <- sql"create table lite_not_null (id integer primary key, name text not null)".dml
          exit <- sql"insert into lite_not_null (id, name) values (1, null)".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(SaferisError.ConstraintViolation("23502", _, _, _)) => true
            case _                                                        => false
      ,
      test("a real column keeps a Double's precision and a Float's value"):
        for
          _ <- sql"drop table if exists lite_reals".dml
          // `real` for both, as tables created by Saferis 0.19 declared them.
          _      <- sql"create table lite_reals (id integer primary key, d real, f real)".dml
          _      <- sql"insert into lite_reals (id, d, f) values (1, ${0.1}, ${1.1f})".dml
          double <- sql"select d from lite_reals where id = 1".queryValue[Double]
          float  <- sql"select f from lite_reals where id = 1".queryValue[Float]
        yield assertTrue(double.contains(0.1), float.contains(1.1f))
      ,
      test("a 64-bit integer reads as Long, and as Int when it fits"):
        for
          big   <- sql"select 9223372036854775807".queryValue[Long]
          small <- sql"select 42".queryValue[Int]
        yield assertTrue(big.contains(Long.MaxValue), small.contains(42))
      ,
      test("an array parameter is Unsupported on SQLite"):
        for exit <- sql"select ${Chunk(1, 2)}".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(_: SaferisError.Unsupported) => true
            case _                                 => false
      ,
      test("a dropped column is gone"):
        for
          _    <- sql"drop table if exists lite_drop".dml
          _    <- sql"create table lite_drop (id integer primary key, gone text, kept text)".dml
          _    <- SqlFragment.text(summon[Dialect].dropColumnSql("lite_drop", "gone")).dml
          _    <- sql"insert into lite_drop (id, kept) values (1, ${"k"})".dml
          read <- sql"select kept from lite_drop where id = 1".queryValue[String]
        yield assertTrue(read.contains("k")),
    ) @@ TestAspect.sequential
end SqliteValueSpecs
