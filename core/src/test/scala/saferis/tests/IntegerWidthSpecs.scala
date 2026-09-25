package saferis.tests

import saferis.*
import zio.test.*

object IntegerWidthSpecs extends ZIOSpecDefault:
  private def decode[A](value: SqlValue)(using decoder: Decoder[A]): Either[DecodeError, A] =
    decoder.decode(value)

  def spec = suite("integer decode by value, not column width")(
    test("an int8 that fits in int4 decodes as Int"):
      check(Gen.int)(n => assertTrue(decode[Int](SqlValue.Int8(n.toLong)) == Right(n)))
    ,
    test("an int8 outside int4 fails instead of wrapping"):
      check(Gen.long.filter(n => n > Int.MaxValue || n < Int.MinValue)): n =>
        assertTrue(decode[Int](SqlValue.Int8(n)).isLeft)
    ,
    test("any narrower integer decodes as Long"):
      check(Gen.short, Gen.int): (s, i) =>
        assertTrue(
          decode[Long](SqlValue.Int2(s)) == Right(s.toLong),
          decode[Long](SqlValue.Int4(i)) == Right(i.toLong),
        )
    ,
    test("an int4 that fits in int2 decodes as Short"):
      check(Gen.short)(s => assertTrue(decode[Short](SqlValue.Int4(s.toInt)) == Right(s)))
    ,
    test("a float4 widens to Double exactly"):
      check(Gen.float.filterNot(_.isNaN))(f =>
        assertTrue(decode[Double](SqlValue.Float4(f)).map(_.toFloat) == Right(f))
      )
    ,
    test("a float8 written from a Float decodes back to that Float, NaN and infinities included"):
      check(Gen.float): f =>
        val back = decode[Float](SqlValue.Float8(f.toDouble))
        assertTrue(back.exists(b => b == f || (b.isNaN && f.isNaN)))
    ,
    test("a float8 that is not exactly a Float fails instead of rounding"):
      assertTrue(decode[Float](SqlValue.Float8(0.1)).isLeft)
    ,
    test("text is still not an integer"):
      assertTrue(decode[Int](SqlValue.Text("1")).isLeft),
  )
end IntegerWidthSpecs
