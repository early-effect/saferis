package saferis.postgres

import saferis.JsonText
import saferis.ServerType
import saferis.SqlType
import saferis.SqlValue
import saferis.TypeName

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
    SqlValue.SmallInt(Short.MinValue),
    SqlValue.SmallInt(7),
    SqlValue.Integer(0),
    SqlValue.Integer(-42),
    SqlValue.BigInt(9223372036854775807L),
    SqlValue.Real(1.5f),
    SqlValue.Real(0.0f),
    SqlValue.DoublePrecision(-2.25),
    SqlValue.Numeric(BigDecimal("9223372036854775808.25")),
    SqlValue.VarChar("a b"),
    SqlValue.Text("line"),
    SqlValue.Binary(Chunk[Byte](0, 1, -1, 32)),
    SqlValue.Date(LocalDate.parse("2024-09-23")),
    SqlValue.Time(LocalTime.of(15, 4, 5, 123456000)),
    SqlValue.Timestamp(LocalDateTime.parse("2024-09-23T15:04:05.123456")),
    SqlValue.TimestampTz(Instant.parse("2024-09-23T15:04:05.123456Z")),
    SqlValue.Json(JsonText("""{"a":null}""")),
    SqlValue.Uuid(UUID.fromString("123e4567-e89b-12d3-a456-426614174000")),
  )

  private def roundTrip(value: SqlValue): Boolean =
    PgText.oid(value.sqlType).exists { id =>
      PgText.encode(value).exists(text => PgText.decode(id, text) == Right(value))
    }

  def spec = suite("PgText")(
    test("known values round-trip through DateStyle=ISO text"):
      assertTrue(known.forall(roundTrip))
    ,
    test("generated integers, longs, and bools round-trip"):
      check(Gen.int, Gen.long, Gen.boolean, Gen.listOf(Gen.byte)): (i, n, bit, bytes) =>
        val values = List(
          SqlValue.Integer(i),
          SqlValue.BigInt(n),
          SqlValue.SmallInt(i.toShort),
          SqlValue.Bool(bit),
          SqlValue.Binary(Chunk.fromIterable(bytes)),
        )
        assertTrue(values.forall(roundTrip))
    ,
    test("non-finite floats encode as Postgres text"):
      val nan = PgText.encode(SqlValue.DoublePrecision(Double.NaN)).flatMap(text => PgText.decode(701, text).toOption)
      val inf =
        PgText.encode(SqlValue.Real(Float.PositiveInfinity)).flatMap(text => PgText.decode(700, text).toOption)
      assertTrue(
        PgText.encode(SqlValue.DoublePrecision(Double.NaN)) == Some("NaN"),
        nan match
          case Some(SqlValue.DoublePrecision(v)) => v.isNaN
          case _                                 => false
        ,
        inf match
          case Some(SqlValue.Real(v)) => v.isInfinite
          case _                      => false,
      )
    ,
    test("an unknown oid is Other"):
      assertTrue(PgText.decode(99999, "custom") == Right(SqlValue.Other(ServerType.Oid(99999), "custom")))
    ,
    test("casts use postgres spellings"):
      assertTrue(
        PgText.cast(SqlType.BigInt) == Some("int8"),
        PgText.cast(SqlType.DoublePrecision) == Some("float8"),
        PgText.cast(SqlType.Binary) == Some("bytea"),
        PgText.cast(SqlType.Json) == Some("jsonb"),
        PgText.cast(SqlType.TimestampTz) == Some("timestamptz"),
        PgText.cast(SqlType.Array(SqlType.Integer)) == Some("int4[]"),
        PgText.cast(SqlType.Other(ServerType.Named(TypeName("mood")))) == None,
        PgText.encode(SqlValue.Null(SqlType.Text)) == None,
      )
    ,
    test("array text quotes, escapes, and round-trips"):
      check(Gen.listOf(Gen.string), Gen.listOf(Gen.int), Gen.boolean): (texts, ints, includeNull) =>
        val textValues =
          Chunk.fromIterable(texts.map(SqlValue.Text(_))) ++
            (if includeNull then Chunk(SqlValue.Null(SqlType.Text)) else Chunk.empty)
        val intValues             = Chunk.fromIterable(ints.map(SqlValue.Integer(_)))
        val textArray             = SqlValue.array(SqlType.Text, textValues)
        val intArray              = SqlValue.array(SqlType.Integer, intValues)
        def back(value: SqlValue) =
          PgText.encode(value).flatMap(text => PgText.decode(1009, text).toOption)
        def backInt(value: SqlValue) =
          PgText.encode(value).flatMap(text => PgText.decode(1007, text).toOption)
        assertTrue(
          textArray.exists(value => back(value).contains(value)),
          intArray.exists(value => backInt(value).contains(value)),
          SqlValue.array(SqlType.Integer, Chunk(SqlValue.Text("x"))).isLeft,
        ),
  )
end PgTextSpecs
