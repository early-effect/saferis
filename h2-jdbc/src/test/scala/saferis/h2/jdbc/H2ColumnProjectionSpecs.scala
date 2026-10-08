package saferis.h2.jdbc

import saferis.*
import saferis.h2.given
import saferis.jdbc.JdbcColumn
import saferis.jdbc.JdbcSession
import saferis.jdbc.StandardJdbcAdapter

import zio.test.*
import zio.{test as _, *}

import java.sql.ResultSet

/** `wide_row` has a `noise` column the row types do not name. The adapter fails if that column is read, so a green
  * `select *` into `Narrow` means the driver skipped it.
  */
object H2ColumnProjectionSpecs extends ZIOSpecDefault:
  private val noise = ColumnName("noise")

  private object FailOnNoise extends StandardJdbcAdapter:
    override def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
      if column.label.matchesLabel(noise) then Left(SaferisError.Unexpected("read noise"))
      else super.read(rs, column)

  @tableName("wide_row")
  final case class Narrow(@key id: Int, name: String) derives Table

  @tableName("wide_row")
  @projectColumns
  final case class Projected(@key id: Int, name: String) derives Table

  def spec =
    suite("unused result columns")(
      test("select star into a narrower type does not read the extra column"):
        for
          _ <- sql"""drop table if exists "wide_row"""".execute
          _ <-
            sql"""create table "wide_row" ("id" integer primary key, "noise" varchar(32), "name" varchar(32))""".execute
          _    <- sql"""insert into "wide_row" ("id", "noise", "name") values (${1}, ${"boom"}, ${"ada"})""".execute
          read <- sql"""select * from "wide_row"""".queryOne[Narrow]
        yield assertTrue(read.contains(Narrow(1, "ada")))
      ,
      test("projectColumns asks only for the named columns"):
        val rendered = Query[Projected].all.build.sql
        for read <- Query[Projected].all.queryOne[Projected]
        yield assertTrue(
          read.contains(Projected(1, "ada")),
          !rendered.contains("*"),
          rendered.contains("\"wide_row_ref_1\".\"id\""),
          rendered.contains("\"wide_row_ref_1\".\"name\""),
        ),
    ).provideShared(H2Jdbc.memory("saferis_h2_columns") >>> JdbcSession.layer(FailOnNoise))
      @@ TestAspect.sequential
end H2ColumnProjectionSpecs
