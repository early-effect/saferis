package saferis.postgres

import saferis.ServerType
import saferis.SqlType
import saferis.SqlValue

import zio.Chunk
import zio.test.*

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

object PgTextSpecs extends ZIOSpecDefault:
  private val known: List[SqlValue] = List(
    SqlValue.Bool(true),
    SqlValue.Bool(false),
    SqlValue.Int2(Short.MinValue),
    SqlValue.Int2(7),
    SqlValue.Int4(0),
    SqlValue.Int4(-42),
    SqlValue.Int8(9223372036854775807L),
    SqlValue.Float4(1.5f),
    SqlValue.Float4(0.0f),
    SqlValue.Float8(-2.25),
    SqlValue.Numeric(BigDecimal("9223372036854775808.25")),
    SqlValue.VarChar("a b"),
    SqlValue.Text("line"),
    SqlValue.Bytea(Chunk[Byte](0, 1, -1, 32)),
    SqlValue.Date(LocalDate.parse("2024-09-23")),
    SqlValue.Time(LocalTime.of(15, 4, 5, 123456000)),
    SqlValue.Timestamp(LocalDateTime.parse("2024-09-23T15:04:05.123456")),
    SqlValue.Timestamptz(Instant.parse("2024-09-23T15:04:05.123456Z")),
    SqlValue.Jsonb("""{"a":null}"""),
    SqlValue.Uuid(UUID.fromString("123e4567-e89b-12d3-a456-426614174000")),
  )

  private def roundTrip(value: SqlValue): Boolean =
    PgText.oid(value.sqlType).exists { id =>
      PgText.decode(id, PgText.encode(value)) == Right(value)
    }

  def spec = suite("PgText")(
    test("known values round-trip through DateStyle=ISO text"):
      assertTrue(known.forall(roundTrip))
    ,
    test("generated integers, longs, and bools round-trip"):
      check(Gen.int, Gen.long, Gen.boolean, Gen.listOf(Gen.byte)): (i, n, bit, bytes) =>
        val values = List(
          SqlValue.Int4(i),
          SqlValue.Int8(n),
          SqlValue.Int2(i.toShort),
          SqlValue.Bool(bit),
          SqlValue.Bytea(Chunk.fromIterable(bytes)),
        )
        assertTrue(values.forall(roundTrip))
    ,
    test("non-finite floats encode as Postgres text"):
      val nan = PgText.decode(701, PgText.encode(SqlValue.Float8(Double.NaN)))
      val inf = PgText.decode(700, PgText.encode(SqlValue.Float4(Float.PositiveInfinity)))
      assertTrue(
        PgText.encode(SqlValue.Float8(Double.NaN)) == "NaN",
        nan match
          case Right(SqlValue.Float8(v)) => v.isNaN
          case _                         => false
        ,
        inf match
          case Right(SqlValue.Float4(v)) => v.isInfinite
          case _                         => false,
      )
    ,
    test("an unknown oid is Other"):
      assertTrue(PgText.decode(99999, "custom") == Right(SqlValue.Other(ServerType.Oid(99999), "custom")))
    ,
    test("casts use postgres spellings"):
      assertTrue(
        PgText.cast(SqlType.Int8) == "int8",
        PgText.cast(SqlType.Float8) == "float8",
        PgText.cast(SqlType.Bytea) == "bytea",
        PgText.cast(SqlType.Jsonb) == "jsonb",
        PgText.cast(SqlType.Timestamptz) == "timestamptz",
      ),
  )
end PgTextSpecs
