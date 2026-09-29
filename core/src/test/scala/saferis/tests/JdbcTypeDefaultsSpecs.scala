package saferis.tests

import saferis.*
import zio.json.*
import zio.test.*

object JdbcTypeDefaultsSpecs extends ZIOSpecDefault:
  val spec = suite("Database Column Type Mappings")(
    test("string encoder uses varchar(255) column type") {
      val encoder = summon[Encoder[String]]
      assertTrue(encoder.columnType == "varchar(255)")
    },
    test("short encoder uses smallint column type") {
      val encoder = summon[Encoder[Short]]
      assertTrue(encoder.columnType == "smallint")
    },
    test("int encoder uses integer column type") {
      val encoder = summon[Encoder[Int]]
      assertTrue(encoder.columnType == "integer")
    },
    test("long encoder uses bigint column type") {
      val encoder = summon[Encoder[Long]]
      assertTrue(encoder.columnType == "bigint")
    },
    test("boolean encoder uses boolean column type") {
      val encoder = summon[Encoder[Boolean]]
      assertTrue(encoder.columnType == "boolean")
    },
    test("float encoder uses real column type") {
      val encoder = summon[Encoder[Float]]
      assertTrue(encoder.columnType == "real")
    },
    test("double encoder uses PostgreSQL-specific 'double precision'") {
      val encoder = summon[Encoder[Double]]
      assertTrue(encoder.columnType == "double precision")
    },
    test("local date encoder uses date column type") {
      val encoder = summon[Encoder[java.time.LocalDate]]
      assertTrue(encoder.sqlType == SqlType.Date) &&
      assertTrue(encoder.columnType == "date")
    },
    test("Text opaque type uses text column type (not varchar)") {
      val encoder = summon[Encoder[Text]]
      assertTrue(encoder.columnType == "text") &&
      assertTrue(encoder.sqlType == SqlType.Text)
    },
    test("Text can be used in table definitions") {
      @tableName("articles")
      final case class Article(@key id: Int, title: String, content: Text) derives Table

      val table         = Table[Article]
      val titleColumn   = table.columns.find(_.name == "title").get
      val contentColumn = table.columns.find(_.name == "content").get

      assertTrue(titleColumn.columnType == "varchar(255)") &&
      assertTrue(contentColumn.columnType == "text")
    },
    test("Json[A] encoder uses jsonb") {
      final case class Metadata(tags: List[String], version: Int) derives JsonCodec
      val encoder = summon[Encoder[Json[Metadata]]]
      assertTrue(encoder.sqlType == SqlType.Json) &&
      assertTrue(encoder.columnType == "jsonb")
    },
    test("Json[A] can be used in table definitions") {
      final case class EventMetadata(source: String, priority: Int) derives JsonCodec

      @tableName("events")
      final case class Event(@key id: Int, name: String, metadata: Json[EventMetadata]) derives Table

      val table          = Table[Event]
      val nameColumn     = table.columns.find(_.name == "name").get
      val metadataColumn = table.columns.find(_.name == "metadata").get

      assertTrue(nameColumn.columnType == "varchar(255)") &&
      assertTrue(metadataColumn.columnType == "jsonb")
    },
    test("Option[Json[A]] is supported for nullable JSON columns") {
      final case class Config(setting: String) derives JsonCodec

      @tableName("settings")
      final case class Settings(@key id: Int, config: Option[Json[Config]]) derives Table

      val table        = Table[Settings]
      val configColumn = table.columns.find(_.name == "config").get

      assertTrue(configColumn.columnType == "jsonb") &&
      assertTrue(configColumn.isNullable)
    },
  )
end JdbcTypeDefaultsSpecs
