package saferis.tests

import saferis.*

import java.time.LocalDate
import java.time.LocalDateTime
import java.util.UUID

import zio.Chunk
import zio.test.*

object LosslessDecodeSpecs extends ZIOSpecDefault:
  private def decode[A](value: SqlValue)(using decoder: Decoder[A]): Either[DecodeError, A] =
    decoder.decode(value)

  private val int8    = TypeName("int8")
  private val bigint  = TypeName("bigint")
  private val float4  = TypeName("float4")
  private val float8  = TypeName("float8")
  private val numeric = TypeName("numeric")
  private val bool    = TypeName("bool")
  private val date    = TypeName("date")
  private val uuid    = TypeName("uuid")
  private val varchar = TypeName("varchar")
  private val text    = TypeName("text")

  def spec = suite("a cell decodes when the conversion loses nothing")(
    test("a fraction is lossy and a past-int8 magnitude is out of range"):
      val fraction = SqlValue.Numeric(BigDecimal("1.5"))
      val past     = SqlValue.Text("9223372036854775808")
      val before   = SqlValue.Text("-9223372036854775809")
      assertTrue(
        decode[BigInt](fraction) == Left(DecodeError.Lossy(bigint, fraction)),
        decode[Long](fraction) == Left(DecodeError.Lossy(int8, fraction)),
        decode[Long](past) == Left(DecodeError.OutOfRange(int8, past)),
        decode[Long](before) == Left(DecodeError.OutOfRange(int8, before)),
      )
    ,
    test("fractional or padded text is not an integer spelling"):
      val fraction = SqlValue.Text("123.5")
      val padded   = SqlValue.VarChar(" 123")
      assertTrue(
        decode[Long](fraction) == Left(DecodeError.InvalidText(int8, "123.5")),
        decode[Long](padded) == Left(DecodeError.InvalidText(int8, " 123")),
        decode[Long](SqlValue.Text("1e2")) == Right(100L),
        decode[Long](SqlValue.Numeric(BigDecimal("5.0"))) == Right(5L),
      )
    ,
    test("null stays a failure for a non-option field"):
      val cell = SqlValue.Null(SqlType.BigInt)
      assertTrue(
        decode[Long](cell) == Left(DecodeError.Null),
        decode[Option[Long]](cell) == Right(None),
      )
    ,
    test("2^53 is a double and the next long is not"):
      val exact   = 1L << 53
      val inexact = exact + 1
      val loss    = SqlValue.BigInt(inexact)
      assertTrue(
        decode[Double](SqlValue.BigInt(exact)) == Right(exact.toDouble),
        decode[Double](loss) == Left(DecodeError.Lossy(float8, loss)),
      )
    ,
    test("a numeric becomes the nearest finite double and overflows as lossy"):
      val tenth = SqlValue.Numeric(BigDecimal("0.1"))
      val huge  = SqlValue.Numeric(BigDecimal("1e309"))
      assertTrue(
        decode[Double](tenth) == Right(BigDecimal("0.1").bigDecimal.doubleValue),
        decode[Double](huge) == Left(DecodeError.Lossy(float8, huge)),
      )
    ,
    test("an integer is a float only when it is that float"):
      val exact   = SqlValue.BigInt(16777216L)
      val inexact = SqlValue.BigInt(16777217L)
      assertTrue(
        decode[Float](exact) == Right(16777216f),
        decode[Float](inexact) == Left(DecodeError.Lossy(float4, inexact)),
      )
    ,
    test("a non-finite float is not a decimal"):
      val cell = SqlValue.DoublePrecision(Double.PositiveInfinity)
      assertTrue(
        decode[BigDecimal](SqlValue.DoublePrecision(0.1)) == Right(BigDecimal.exact(0.1)),
        decode[BigDecimal](cell) == Left(DecodeError.Lossy(numeric, cell)),
        decode[BigDecimal](SqlValue.BigInt(12L)) == Right(BigDecimal(12)),
        decode[BigDecimal](SqlValue.Text("1.50")) == Right(BigDecimal("1.50")),
      )
    ,
    test("a date reads ISO text and refuses a timestamp"):
      val day   = LocalDate.parse("2020-01-02")
      val stamp = SqlValue.Timestamp(LocalDateTime.parse("2020-01-02T00:00:00"))
      val text  = SqlValue.Text("2020-01-02T00:00:00")
      assertTrue(
        decode[LocalDate](SqlValue.Date(day)) == Right(day),
        decode[LocalDate](SqlValue.Text("2020-01-02")) == Right(day),
        decode[LocalDate](text) == Left(DecodeError.InvalidText(date, "2020-01-02T00:00:00")),
        decode[LocalDate](stamp) == Left(DecodeError.Mismatch(date, stamp)),
      )
    ,
    test("a boolean reads true and false text only"):
      val letter = SqlValue.Text("t")
      val number = SqlValue.Integer(1)
      assertTrue(
        decode[Boolean](SqlValue.Text("true")) == Right(true),
        decode[Boolean](SqlValue.VarChar("false")) == Right(false),
        decode[Boolean](letter) == Left(DecodeError.InvalidText(bool, "t")),
        decode[Boolean](number) == Left(DecodeError.Mismatch(bool, number)),
      )
    ,
    test("string renders a number, a boolean, a date, and a uuid"):
      val id     = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      val day    = LocalDate.parse("2020-01-02")
      val binary = SqlValue.Binary(Chunk.empty)
      val json   = SqlValue.Json(JsonText("{}"))
      assertTrue(
        decode[String](SqlValue.BigInt(12L)) == Right("12"),
        decode[String](SqlValue.Numeric(BigDecimal("1.50"))) == Right("1.50"),
        decode[String](SqlValue.Bool(false)) == Right("false"),
        decode[String](SqlValue.Date(day)) == Right("2020-01-02"),
        decode[String](SqlValue.Uuid(id)) == Right(id.toString),
        decode[String](binary) == Left(DecodeError.Mismatch(varchar, binary)),
        decode[String](json) == Left(DecodeError.Mismatch(varchar, json)),
      )
    ,
    test("Text stays a text column"):
      val cell = SqlValue.BigInt(1L)
      assertTrue(
        decode[Text](SqlValue.Text("1")) == Right(Text("1")),
        decode[Text](cell) == Left(DecodeError.Mismatch(text, cell)),
      )
    ,
    test("a uuid reads the native value and canonical text"):
      val id  = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      val bad = SqlValue.Text("nope")
      assertTrue(
        decode[UUID](SqlValue.Uuid(id)) == Right(id),
        decode[UUID](SqlValue.VarChar(id.toString.toUpperCase)) == Right(id),
        decode[UUID](bad) == Left(DecodeError.InvalidText(uuid, "nope")),
      )
    ,
    test("an array uses the element decoder"):
      val cell = SqlValue.array(SqlType.Text, Chunk(SqlValue.Text("4"), SqlValue.Text("5")))
      assertTrue(decode[Chunk[Long]](cell) == Right(Chunk(4L, 5L)))
    ,
    test("a tuple column keeps the cell reason"):
      val row = SqlRow(
        Chunk(ColumnName("a"), ColumnName("b")),
        Chunk(SqlValue.BigInt(1L), SqlValue.Text("no")),
      )
      val decoded = summon[RowDecoder[(Long, Long)]].decode(row)
      assertTrue(decoded == Left(DecodeError.At(2, DecodeError.InvalidText(int8, "no"))))
    ,
    test("DecodingError still names the column and the expected type"):
      val reason = DecodeError.Lossy(int8, SqlValue.Numeric(BigDecimal("1.5")))
      val error  = SaferisError.DecodingError(ColumnName("project_id"), TypeName("BigInt"), reason)
      assertTrue(error.message.startsWith("Failed to decode column 'project_id' as BigInt:")),
  )
end LosslessDecodeSpecs
