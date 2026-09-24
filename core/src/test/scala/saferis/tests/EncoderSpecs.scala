package saferis.tests

import saferis.*
import zio.test.*

import java.time.LocalDate
import java.time.LocalTime

object EncoderSpecs extends ZIOSpecDefault:
  val spec = suite("Encoder should encode all supported types")(
    test("literal method") {
      val stringLit     = summon[Encoder[String]].literal("O'Reilly")
      val someStringLit = summon[Encoder[Option[String]]].literal(Some("O'Reilly"))
      val intLit        = summon[Encoder[Int]].literal(42)
      val boolLit       = summon[Encoder[Boolean]].literal(true)
      val dateLit       = summon[Encoder[LocalDate]].literal(LocalDate.of(2024, 9, 23))
      val floatLit      = summon[Encoder[Float]].literal(3.14f)
      val noneLitInt    = summon[Encoder[Option[Int]]].literal(None)
      val noneLitStr    = summon[Encoder[Option[String]]].literal(None)
      val someLitInt    = summon[Encoder[Option[Int]]].literal(Some(99))
      val timeLit       = summon[Encoder[LocalTime]].literal(LocalTime.of(15, 4, 5))
      assertTrue(
        stringLit == "'O''Reilly'",
        someStringLit == "'O''Reilly'",
        intLit == "42",
        boolLit == "true",
        dateLit == "DATE '2024-09-23'",
        floatLit == "3.14",
        noneLitInt == "null",
        noneLitStr == "null",
        someLitInt == "99",
        timeLit == "TIME '15:04:05'",
      )
    },
    test("optional none is a typed null") {
      val encoded = summon[Encoder[Option[Int]]].encode(None)
      assertTrue(encoded == SqlValue.Null(SqlType.Int4))
    },
    test("NaN and infinities are quoted float and double literals") {
      val floats  = summon[Encoder[Float]]
      val doubles = summon[Encoder[Double]]
      assertTrue(
        floats.literal(Float.NaN) == "'NaN'",
        floats.literal(Float.PositiveInfinity) == "'Infinity'",
        floats.literal(Float.NegativeInfinity) == "'-Infinity'",
        doubles.literal(Double.NaN) == "'NaN'",
        doubles.literal(Double.PositiveInfinity) == "'Infinity'",
        doubles.literal(Double.NegativeInfinity) == "'-Infinity'",
      )
    },
    test("finite float literals round-trip") {
      val floats                        = summon[Encoder[Float]]
      def roundTrips(v: Float): Boolean =
        val text = floats.literal(v)
        text != "3.140000104904175" && text.toFloat == v
      assertTrue(
        floats.literal(3.14f) == "3.14",
        roundTrips(3.14f),
        roundTrips(0.1f),
        roundTrips(1.0f),
        roundTrips(-0.0f),
        roundTrips(Float.MaxValue),
        roundTrips(Float.MinPositiveValue),
      )
    },
  )
end EncoderSpecs
