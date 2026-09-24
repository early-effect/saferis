package saferis.tests

import saferis.*
import zio.test.*

import java.time.*

object JavaTimeCodecSpecs extends ZIOSpecDefault:

  val spec = suite("Java Time Codec Support")(
    test("Instant codec is available and uses TIMESTAMP_WITH_TIMEZONE") {
      val encoder = summon[Encoder[Instant]]
      val decoder = summon[Decoder[Instant]]
      val codec   = summon[Codec[Instant]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Timestamptz)
    },
    test("LocalDateTime codec is available") {
      val encoder = summon[Encoder[LocalDateTime]]
      val decoder = summon[Decoder[LocalDateTime]]
      val codec   = summon[Codec[LocalDateTime]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Timestamp)
    },
    test("LocalDate codec is available") {
      val encoder = summon[Encoder[LocalDate]]
      val decoder = summon[Decoder[LocalDate]]
      val codec   = summon[Codec[LocalDate]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Date)
    },
    test("LocalTime codec is available") {
      val encoder = summon[Encoder[LocalTime]]
      val decoder = summon[Decoder[LocalTime]]
      val codec   = summon[Codec[LocalTime]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Time)
    },
    test("ZonedDateTime codec is available and uses TIMESTAMP_WITH_TIMEZONE") {
      val encoder = summon[Encoder[ZonedDateTime]]
      val decoder = summon[Decoder[ZonedDateTime]]
      val codec   = summon[Codec[ZonedDateTime]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Timestamptz)
    },
    test("OffsetDateTime codec is available and uses TIMESTAMP_WITH_TIMEZONE") {
      val encoder = summon[Encoder[OffsetDateTime]]
      val decoder = summon[Decoder[OffsetDateTime]]
      val codec   = summon[Codec[OffsetDateTime]]

      assertTrue(encoder != null) &&
      assertTrue(decoder != null) &&
      assertTrue(codec != null) &&
      assertTrue(encoder.sqlType == SqlType.Timestamptz)
    },
    test("Can use Instant in table definition with timestamptz column type") {
      @tableName("events")
      final case class Event(@key id: Int, occurredAt: Instant, description: String) derives Table

      val table            = Table[Event]
      val occurredAtColumn = table.occurredAt

      assertTrue(occurredAtColumn.columnType == "timestamptz")
    },
    test("Can use LocalDateTime in table definition") {
      @tableName("appointments")
      final case class Appointment(@key id: Int, scheduledFor: LocalDateTime) derives Table

      val table              = Table[Appointment]
      val scheduledForColumn = table.scheduledFor

      assertTrue(scheduledForColumn.columnType == "timestamp")
    },
    test("Can use LocalDate in table definition") {
      @tableName("holidays")
      final case class Holiday(@key id: Int, date: LocalDate, name: String) derives Table

      val table      = Table[Holiday]
      val dateColumn = table.date

      assertTrue(dateColumn.columnType == "date")
    },
    test("Codec companion object has convenience vals") {
      assertTrue(Codec.instant != null) &&
      assertTrue(Codec.localDateTime != null) &&
      assertTrue(Codec.localDate != null) &&
      assertTrue(Codec.localTime != null) &&
      assertTrue(Codec.zonedDateTime != null) &&
      assertTrue(Codec.offsetDateTime != null)
    },
  )
end JavaTimeCodecSpecs
