package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
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
      md"""All `java.time` types are supported with automatic SQL type mapping:

| Scala Type | PostgreSQL Type | JDBC Type |
|------------|-----------------|-----------|
| `java.time.Instant` | `timestamptz` | TIMESTAMP_WITH_TIMEZONE |
| `java.time.LocalDateTime` | `timestamp` | TIMESTAMP |
| `java.time.LocalDate` | `date` | DATE |
| `java.time.LocalTime` | `time` | TIME |
| `java.time.ZonedDateTime` | `timestamptz` | TIMESTAMP_WITH_TIMEZONE |
| `java.time.OffsetDateTime` | `timestamptz` | TIMESTAMP_WITH_TIMEZONE |""",
      exampleZIO {
        xa
          .run(for
            _   <- ddl.createTable[Event](ifNotExists = true)
            _   <- dml.insert(Event(-1, "Conference", Instant.now(), Some(LocalDateTime.now().plusDays(7)), LocalDate.now()))
            _   <- dml.insert(Event(-1, "Meeting", Instant.now(), None, LocalDate.now().plusDays(1)))
            all <- sql"SELECT * FROM $events ORDER BY ${events.name}".query[Event]
            names = all.map(_.name) // project to names: stable output across runs
          yield names)
          .either
      }.assert {
        case Right(names) => assertTrue(names.contains("Conference") && names.contains("Meeting"))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("UUID Support")(
      md"""UUIDs can be used as primary keys:""",
      exampleZIO {
        xa
          .run(for
            _   <- ddl.createTable[Entity](ifNotExists = true)
            id1 = UUID.randomUUID()
            id2 = UUID.randomUUID()
            _     <- dml.insert(Entity(id1, "First Entity"))
            _     <- dml.insert(Entity(id2, "Second Entity"))
            found <- sql"SELECT * FROM $entities WHERE ${entities.id} = $id1".queryOne[Entity]
          yield found.map(_.name))
          .either
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
| `Option[T]` | Same as `T`, nullable |""",
    ),
    section("JSON/JSONB Support")(
      md"""Saferis provides `Json[A]` for storing arbitrary types as JSON in the database. This maps to `JSONB` in PostgreSQL and `JSON` in MySQL.

```scala
// Define a type to store as JSON - needs JsonCodec
case class Metadata(tags: List[String], version: Int) derives JsonCodec

// Use Json[A] wrapper in your table definition
@tableName("type_support_json_events")
case class JsonEvent(
  @generated @key id: Int,
  name: String,
  metadata: Json[Metadata]  // Stored as JSONB in PostgreSQL
) derives Table
```""",
      exampleZIO {
        // Create table and insert with JSON data
        xa
          .run(for
            _   <- ddl.createTable[JsonEvent](ifNotExists = true)
            _   <- dml.insert(JsonEvent(-1, "Deploy", Json(Metadata(List("prod", "release"), 1))))
            _   <- dml.insert(JsonEvent(-1, "Rollback", Json(Metadata(List("prod", "hotfix"), 2))))
            all <- sql"SELECT * FROM $jsonEvents ORDER BY ${jsonEvents.name}".query[JsonEvent]
          yield all)
          .either
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
- Uses `Types.OTHER` JDBC type which maps to `jsonb` in PostgreSQL
- Provides `.value` extension to unwrap: `event.metadata.value` returns `Metadata`""",
    ),
  )
end TypeSupport
