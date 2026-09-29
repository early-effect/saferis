package saferis.tests

import saferis.*

import java.time.OffsetTime
import java.time.ZoneOffset

import zio.test.*

object IntegerWidthSpecs extends ZIOSpecDefault:
  private def decode[A](value: SqlValue)(using decoder: Decoder[A]): Either[DecodeError, A] =
    decoder.decode(value)

  def spec = suite("integer decode by value, not column width")(
    test("an int8 that fits in int4 decodes as Int"):
      check(Gen.int)(n => assertTrue(decode[Int](SqlValue.BigInt(n.toLong)) == Right(n)))
    ,
    test("an int8 outside int4 fails instead of wrapping"):
      check(Gen.long.filter(n => n > Int.MaxValue || n < Int.MinValue)): n =>
        assertTrue(decode[Int](SqlValue.BigInt(n)).isLeft)
    ,
    test("any narrower integer decodes as Long"):
      check(Gen.short, Gen.int): (s, i) =>
        assertTrue(
          decode[Long](SqlValue.SmallInt(s)) == Right(s.toLong),
          decode[Long](SqlValue.Integer(i)) == Right(i.toLong),
        )
    ,
    test("an int4 that fits in int2 decodes as Short"):
      check(Gen.short)(s => assertTrue(decode[Short](SqlValue.Integer(s.toInt)) == Right(s)))
    ,
    test("a float4 widens to Double exactly"):
      check(Gen.float.filterNot(_.isNaN))(f => assertTrue(decode[Double](SqlValue.Real(f)).map(_.toFloat) == Right(f)))
    ,
    test("a float8 written from a Float decodes back to that Float, NaN and infinities included"):
      check(Gen.float): f =>
        val back = decode[Float](SqlValue.DoublePrecision(f.toDouble))
        assertTrue(back.exists(b => b == f || (b.isNaN && f.isNaN)))
    ,
    test("a float8 that is not exactly a Float fails instead of rounding"):
      assertTrue(decode[Float](SqlValue.DoublePrecision(0.1)).isLeft)
    ,
    test("text is still not an integer"):
      assertTrue(decode[Int](SqlValue.Text("1")).isLeft)
    ,
    test("an integer decoder succeeds exactly when the value fits, at any width"):
      check(Gen.long): n =>
        val samples = List(
          Option.when(n.isValidShort)(SqlValue.SmallInt(n.toShort)),
          Option.when(n.isValidInt)(SqlValue.Integer(n.toInt)),
          Some(SqlValue.BigInt(n)),
        ).flatten
        assertTrue(
          samples.forall(value => decode[Short](value).isRight == n.isValidShort),
          samples.forall(value => decode[Int](value).isRight == n.isValidInt),
          samples.forall(value => decode[Long](value).isRight),
        )
    ,
    test("the float decoder succeeds exactly when toFloat.toDouble is the same value, NaN included"):
      def same(left: Double, right: Double) = left == right || (left.isNaN && right.isNaN)
      check(Gen.double): n =>
        val exact = same(n.toFloat.toDouble, n)
        val back  = decode[Float](SqlValue.DoublePrecision(n))
        assertTrue(back.isRight == exact)
    ,
    test("TimeTz encode then decode keeps a non-zero offset"):
      val times =
        for
          hour   <- Gen.int(0, 23)
          minute <- Gen.int(0, 59)
          second <- Gen.int(0, 59)
          nano   <- Gen.int(0, 999999999)
          offset <- Gen.int(-18 * 60, 18 * 60)
        yield OffsetTime.of(hour, minute, second, nano, ZoneOffset.ofTotalSeconds(offset * 60))
      check(times): value =>
        val encoded = summon[Encoder[OffsetTime]].encode(value)
        assertTrue(decode[OffsetTime](encoded) == Right(value)),
  )
end IntegerWidthSpecs
