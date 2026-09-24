package saferis.mysql.jdbc

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.mysql.given

import zio.{test as _, *}
import zio.json.*
import zio.test.*

import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

/** What MySQL does differently from the portable suite: JSON, `char(36)` UUIDs, `datetime`, unsigned integers, and
  * vendor error codes mapped to the shared SQLSTATE vocabulary.
  */
object MySqlValueSpecs:
  final case class Meta(tags: List[String], version: Int) derives JsonCodec

  @tableName("mysql_values")
  final case class Row(@key id: Int, meta: Json[Meta], ref: UUID, local: LocalDateTime, clock: LocalTime) derives Table

  private def failure(exit: Exit[SaferisError, Any]): Option[SaferisError] = exit match
    case Exit.Failure(cause) => cause.failureOption
    case _                   => None

  def spec =
    suite("mysql values")(
      test("json, a char(36) uuid, datetime(6), and time(6) round-trip"):
        val row = Row(
          1,
          Json(Meta(List("a", "b"), 2)),
          UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
          LocalDateTime.parse("2024-01-02T03:04:05.123456"),
          LocalTime.parse("13:14:15.654321"),
        )
        for
          _    <- dropTable[Row](ifExists = true)
          _    <- createTable[Row]()
          _    <- insert(row)
          read <- sql"select * from mysql_values where id = ${1}".queryOne[Row]
        yield assertTrue(read.contains(row))
      ,
      test("an enum column reads as text and binds from text"):
        for
          _    <- sql"drop table if exists mysql_mood".dml
          _    <- sql"create table mysql_mood (id int primary key, mood enum('sad', 'ok'))".dml
          _    <- sql"insert into mysql_mood (id, mood) values (1, ${"ok"})".dml
          read <- sql"select mood from mysql_mood where id = 1".queryValue[String]
        yield assertTrue(read.contains("ok"))
      ,
      test("an unsigned int past Int.MaxValue reads as a Long"):
        for
          _    <- sql"drop table if exists mysql_unsigned".dml
          _    <- sql"create table mysql_unsigned (n int unsigned)".dml
          _    <- sql"insert into mysql_unsigned (n) values (4294967295)".dml
          read <- sql"select n from mysql_unsigned".queryValue[Long]
        yield assertTrue(read.contains(4294967295L))
      ,
      test("a duplicate key is UniqueViolation, named by the key"):
        for
          _ <- sql"drop table if exists mysql_uniq".dml
          _ <-
            sql"create table mysql_uniq (id int primary key, email varchar(255), unique key uq_email (email))".dml
          _    <- sql"insert into mysql_uniq (id, email) values (1, ${"a@b.c"})".dml
          exit <- sql"insert into mysql_uniq (id, email) values (2, ${"a@b.c"})".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(SaferisError.UniqueViolation(Some("uq_email"), "unique violation", _)) => true
            case _                                                                           => false
      ,
      test("a missing parent is a 23503 constraint violation, named by the constraint"):
        for
          _ <- sql"drop table if exists mysql_child".dml
          _ <- sql"drop table if exists mysql_parent".dml
          _ <- sql"create table mysql_parent (id int primary key)".dml
          _ <- sql"""create table mysql_child (
                      id int primary key,
                      parent_id int,
                      constraint fk_child_parent foreign key (parent_id) references mysql_parent (id)
                    )""".dml
          exit <- sql"insert into mysql_child (id, parent_id) values (1, 99)".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(SaferisError.ConstraintViolation("23503", Some("fk_child_parent"), _, _)) => true
            case _                                                                              => false
      ,
      test("an array parameter is Unsupported on MySQL"):
        for exit <- sql"select ${Chunk(1, 2)}".dml.exit
        yield assertTrue:
          failure(exit) match
            case Some(_: SaferisError.Unsupported) => true
            case _                                 => false,
    ) @@ TestAspect.sequential
end MySqlValueSpecs
