package saferis.postgres.jdbc

import saferis.*

import java.time.OffsetTime
import java.time.ZoneOffset

import zio.test.*

/** Postgres `timetz` keeps the offset. It is not stored as a `LocalTime`. */
object TimeTzSpecs extends ZIOSpecDefault:
  def spec = suite("timetz")(
    test("a non-zero offset comes back from Postgres"):
      val when = OffsetTime.of(15, 4, 5, 123456000, ZoneOffset.ofHours(2))
      for
        _   <- sql"drop table if exists timetz_round".dml
        _   <- sql"create table timetz_round (id int primary key, clock timetz)".dml
        _   <- sql"insert into timetz_round (id, clock) values (1, ${when})".dml
        got <- sql"select clock from timetz_round where id = 1".queryValue[OffsetTime]
      yield assertTrue(got.contains(when))
  ).provideShared(DataSourceProvider.default) @@ TestAspect.sequential
end TimeTzSpecs
