package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.*
import zio.test.*

import java.time.*
import java.util.UUID

object TypeSupport extends SaferisDocSpecSuite:

  @tableName("type_support_events")
  case class Event(
      @generated @key id: Int,
      name: String,
      occurredAt: Instant,
      scheduledFor: Option[LocalDateTime],
      eventDate: LocalDate,
  ) derives Table

  val events = Table[Event]

  @tableName("type_support_entities")
  case class Entity(@key id: UUID, name: String) derives Table

  val entities = Table[Entity]

  case class Metadata(tags: List[String], version: Int) derives JsonCodec

  @tableName("type_support_json_events")
  case class JsonEvent(
      @generated @key id: Int,
      name: String,
      metadata: Json[Metadata],
  ) derives Table

  val jsonEvents = Table[JsonEvent]

  def doc = page("Type Support")(
    md"""Saferis provides built-in support for common Scala and Java types.""",
    section("java.time Types")(
      md"""All `java.time` types are supported with automatic SQL type mapping.
`OffsetDateTime` and `ZonedDateTime` encode as `Instant` and decode in UTC.

| Scala Type | PostgreSQL Type | SqlType |
|------------|-----------------|--------|
| `java.time.Instant` | `timestamptz` | TimestampTz |
| `java.time.LocalDateTime` | `timestamp` | Timestamp |
| `java.time.LocalDate` | `date` | Date |
| `java.time.LocalTime` | `time` | Time |
| `java.time.ZonedDateTime` | `timestamptz` | TimestampTz |
| `java.time.OffsetDateTime` | `timestamptz` | TimestampTz |
| `java.time.OffsetTime` | `timetz` | TimeTz |

These names (`TimestampTz`, `TimeTz`, `Integer`, `Json`) are the shared types. A dialect spells them: Postgres `timestamptz` and `timetz`, MySQL ISO text for `TimeTz` because it has no zoned time. See [Dialect System](dialect-system.html). `OffsetTime` keeps its offset.""",
      exampleZIO {
        (for
          _ <- ddl.createTable[Event](ifNotExists = true)
          _ <- dml.insert(
            Event(-1, "Conference", Instant.now(), Some(LocalDateTime.now().plusDays(7)), LocalDate.now())
          )
          _   <- dml.insert(Event(-1, "Meeting", Instant.now(), None, LocalDate.now().plusDays(1)))
          all <- sql"SELECT * FROM $events ORDER BY ${events.name}".query[Event]
          names = all.map(_.name) // project to names: stable output across runs
        yield names).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(names) => assertTrue(names.contains("Conference") && names.contains("Meeting"))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("UUID Support")(
      md"""UUIDs can be used as primary keys:""",
      exampleZIO {
        (for
          _ <- ddl.createTable[Entity](ifNotExists = true)
          id1 = UUID.randomUUID()
          id2 = UUID.randomUUID()
          _     <- dml.insert(Entity(id1, "First Entity"))
          _     <- dml.insert(Entity(id2, "Second Entity"))
          found <- sql"SELECT * FROM $entities WHERE ${entities.id} = $id1".queryOne[Entity]
        yield found.map(_.name)).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(found) => assertTrue(found.contains("First Entity"))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("Other Supported Types")(
      md"""| Scala Type | PostgreSQL Type |
|------------|-----------------|
| `String` | `varchar(255)` |
| `Int` | `integer` |
| `Long` | `bigint` |
| `Double` | `double precision` |
| `Float` | `real` |
| `Boolean` | `boolean` |
| `BigDecimal` | `numeric` |
| `Option[T]` | Same as `T`, nullable |"""
    ),
    section("JSON")(
      md"""`Json[A]` stores a Scala value as JSON. PostgreSQL spells that column `jsonb`. MySQL, SQLite, and H2 spell it `json`.

```scala
// Define a type to store as JSON - needs JsonCodec
case class Metadata(tags: List[String], version: Int) derives JsonCodec

// Use Json[A] wrapper in your table definition
@tableName("type_support_json_events")
case class JsonEvent(
  @generated @key id: Int,
  name: String,
  metadata: Json[Metadata]  // jsonb on PostgreSQL, json elsewhere
) derives Table
```""",
      exampleZIO {
        // Create table and insert with JSON data
        (for
          _   <- ddl.createTable[JsonEvent](ifNotExists = true)
          _   <- dml.insert(JsonEvent(-1, "Deploy", Json(Metadata(List("prod", "release"), 1))))
          _   <- dml.insert(JsonEvent(-1, "Rollback", Json(Metadata(List("prod", "hotfix"), 2))))
          all <- sql"SELECT * FROM $jsonEvents ORDER BY ${jsonEvents.name}".query[JsonEvent]
        yield all).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(all) =>
          assertTrue(
            all.exists(e => e.name == "Deploy" && e.metadata.value.version == 1),
            all.exists(e => e.name == "Rollback" && e.metadata.value.tags.contains("hotfix")),
          )
        case Left(err) => assertTrue(false).label(err.message)
      },
      md"""The `Json[A]` wrapper:
- Requires a `zio.json.JsonCodec[A]` instance for the wrapped type
- Binds as `jsonb` on PostgreSQL and as a `json` column on MySQL, SQLite, and H2
- Provides `.value` extension to unwrap: `event.metadata.value` returns `Metadata`

A JSON column decodes as `Json[A]`, not as `String`. `Decoder[String]` reads text, and also the canonical rendering of a number, a boolean, a date, or a uuid. It does not read `jsonb` or `bytea`. Store the JSON as `Text` if you want the raw document as a string.""",
    ),
    section("Integer Widths")(
      md"""Integer decoders read by value, not by column width. `Decoder[Int]` accepts a `bigint` that fits, so `select count(*)` decodes as `Int` on PostgreSQL, and every SQLite integer (which SQLite stores as 64 bits) decodes as `Short`, `Int`, or `Long`. A whole `numeric`, an integral `varchar` or `text` (`123`, `123.0`, `1e2`), and a float that is that exact integer decode too. A fraction, a value that does not fit, or text that is not an integer fails with a `DecodingError` instead of wrapping or truncating.

`String` also reads the canonical text of an integer, a `numeric`, a boolean, a date, or a uuid. `Text` is the strict text column: it reads `varchar` and `text` only."""
    ),
    section("Enumerations")(
      md"""`Codec.enumeration` binds a database enumeration by its labels. For a parameterless Scala 3 enum, the case names are the labels:

```scala
enum Mood:
  case sad, ok

given Codec[Mood] = Codec.enumeration[Mood](TypeName("mood"))
```

On PostgreSQL the value binds uncast, so the server infers the enum type from the column (`create type mood as enum ('sad', 'ok')`). MySQL stores it in an `enum('sad', 'ok')` column, and SQLite and H2 in a text column. It decodes from the enum, `text`, or `varchar`. For an enum whose labels are not its case names, pass the two functions: `Codec.enumeration[E](TypeName("mood"))(e => label(e), text => fromLabel(text))`."""
    ),
    section("Array Columns")(
      md"""`Chunk[A]` is a PostgreSQL array column (`int4[]` for `Chunk[Int]`) and needs no hand-written given. `Chunk[Byte]` stays `bytea`. `Chunk[Option[A]]` allows null members. The other databases have no array parameters: binding one fails with `SaferisError.Unsupported`. For membership, the query builder's `.inList` binds `IN (...)` there instead, and `in(...)` works in raw SQL everywhere (see [Subqueries](subqueries.html))."""
    ),
  )
end TypeSupport
