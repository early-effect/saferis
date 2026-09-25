package saferis.h2.jdbc

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.h2.H2Dialect
import saferis.h2.given
import saferis.tests.DatabaseTarget
import saferis.tests.SqlSessionConformance

import zio.{test as _, *}
import zio.json.*
import zio.test.*

import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

object H2ConformanceSpecs extends ZIOSpecDefault:
  final case class Meta(tags: List[String], version: Int) derives JsonCodec

  @tableName("h2_values")
  final case class Row(
      @key id: Int,
      meta: Json[Meta],
      ref: UUID,
      local: LocalDateTime,
      clock: LocalTime,
      bytes: Chunk[Byte],
      precise: BigDecimal,
  ) derives Table

  private val values =
    suite("h2 values")(
      test("json, uuid, timestamp(6), time(6), bytes, and a 20-digit decimal round-trip"):
        val row = Row(
          1,
          Json(Meta(List("a", "b"), 2)),
          UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
          LocalDateTime.parse("2024-01-02T03:04:05.123456"),
          LocalTime.parse("13:14:15.654321"),
          Chunk[Byte](1, 2, 3),
          BigDecimal("1234567890.12345678901234567890"),
        )
        for
          _    <- dropTable[Row](ifExists = true)
          _    <- createTable[Row]()
          _    <- insert(row)
          read <- sql"select * from h2_values where id = ${1}".queryOne[Row]
        yield assertTrue(read.contains(row))
    )

  /** H2 proves itself by providing its session and target to the common suite, with no Docker. */
  def spec =
    suite("h2")(SqlSessionConformance.suite, values)
      .provideShared(H2Jdbc.memory("saferis_h2") >>> H2Jdbc.layer(), ZLayer.succeed(DatabaseTarget(H2Dialect)))
      @@ TestAspect.sequential
end H2ConformanceSpecs
