package saferis.tests

import saferis.*
import saferis.Schema.*
import saferis.ddl.*
import saferis.dml.*
import saferis.postgres.PostgresDialect
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.json.*
import zio.test.*

/** Integration tests for Schema DSL features with PostgreSQL. Tests partial indexes, JSON operations, and verifies
  * actual database behavior.
  */
object SchemaIntegrationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Provide PostgresDialect for JSON operators
  given (Dialect & JsonSupport) = PostgresDialect

  // === Test Data Types ===

  final case class UserProfile(email: String, verified: Boolean, role: String) derives JsonCodec
  final case class Metadata(tags: List[String], version: Int, active: Boolean) derives JsonCodec

  // === Test Tables ===

  @tableName("schema_int_users")
  final case class User(
      @generated @key id: Int,
      name: String,
      email: String,
      status: String,
      age: Int,
  ) derives Table

  // Schema-qualified table for testing typed Query DSL against a real namespaced table.
  @tableName("prd.schema_int_qualified")
  final case class QualifiedRow(@key id: Long, name: String) derives Table

  @tableName("schema_int_profiles")
  final case class Profile(
      @generated @key id: Int,
      userId: Int,
      data: Json[UserProfile],
  ) derives Table

  @tableName("schema_int_events")
  final case class Event(
      @generated @key id: Int,
      name: String,
      metadata: Json[Metadata],
  ) derives Table

  // Helper for querying index info from PostgreSQL
  final case class IndexInfo(indexname: String, indexdef: String) derives Table

  // Helper for counting
  final case class CountResult(count: Long) derives Table

  // Helper for JSON field extraction
  final case class JsonFieldResult(value: Option[String]) derives Table

  // Helper for name extraction
  final case class NameResult(name: String) derives Table

  val spec = suite("Schema Integration Tests")(
    // === Partial Index Creation ===
    suite("Partial index creation")(
      test("creates partial index with WHERE clause") {
        val users = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .named("idx_active_users")
          .build

        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(users))
          // Query PostgreSQL system tables to verify index exists
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_active_users'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.toLowerCase.contains("where"),
          indexes.head.indexdef.contains("active"),
        )
        end for
      },
      test("creates unique partial index") {
        val users = Schema[User]
          .withUniqueIndex(_.email)
          .where(_.status)
          .eql("active")
          .named("idx_unique_active_email")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_unique_active_email'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.toLowerCase.contains("unique"),
          indexes.head.indexdef.toLowerCase.contains("where"),
        )
        end for
      },
      test("partial index allows duplicate values outside WHERE condition") {
        val users = Schema[User]
          .withUniqueIndex(_.email)
          .where(_.status)
          .eql("active")
          .named("idx_unique_active_email_test")
          .build

        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(users))
          // Insert two users with same email but different status
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          // This should succeed because the partial index only applies to active users
          result <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice2', 'alice@test.com', 'inactive', 25)".insert
          ).either
        yield assertTrue(result.isRight)
        end for
      },
      test("partial index enforces uniqueness within WHERE condition") {
        val users = Schema[User]
          .withUniqueIndex(_.email)
          .where(_.status)
          .eql("active")
          .named("idx_unique_active_email_enforce")
          .build

        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(users))
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          // This should fail because both are active with same email
          result <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice2', 'alice@test.com', 'active', 25)".insert
          ).either
        yield assertTrue(result.isLeft)
        end for
      },
    ),
    // === Compound Index Creation ===
    suite("Compound index creation")(
      test("creates compound index on multiple columns") {
        val users = Schema[User]
          .withIndex(_.name)
          .and(_.email)
          .named("idx_name_email")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_name_email'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.contains("name"),
          indexes.head.indexdef.contains("email"),
        )
        end for
      }
    ),
    // === JSON Data Operations ===
    suite("JSON data operations")(
      test("insert and query JSON data") {
        for
          _ <- (dropTable[Profile](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (createTable[Profile]())
          // Insert user
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          // Insert profile with JSON data
          jsonData = """{"email":"alice@test.com","verified":true,"role":"admin"}"""
          _ <- (sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, $jsonData::jsonb)".insert)
          // Verify data using SQL JSON extraction
          email <- (
            sql"SELECT data->>'email' as value FROM schema_int_profiles WHERE id = 1".queryOne[JsonFieldResult]
          )
          role <- (
            sql"SELECT data->>'role' as value FROM schema_int_profiles WHERE id = 1".queryOne[JsonFieldResult]
          )
          count <- (sql"SELECT count(*) as count FROM schema_int_profiles".queryOne[CountResult])
        yield assertTrue(
          count.exists(_.count == 1),
          email.flatMap(_.value).contains("alice@test.com"),
          role.flatMap(_.value).contains("admin"),
        )
      },
      test("insert using Json type directly") {
        for
          _ <- (dropTable[Event](ifExists = true))
          _ <- (createTable[Event]())
          // Create event with JSON metadata
          metadata = Metadata(List("important", "urgent"), 1, true)
          _ <- (insert(Event(0, "Test Event", Json(metadata))))
          // Verify using SQL
          events  <- (sql"SELECT name FROM schema_int_events".query[NameResult])
          version <- (sql"SELECT metadata->>'version' as value FROM schema_int_events".queryOne[JsonFieldResult])
          active  <- (sql"SELECT metadata->>'active' as value FROM schema_int_events".queryOne[JsonFieldResult])
        yield assertTrue(
          events.length == 1,
          events.head.name == "Test Event",
          version.flatMap(_.value).contains("1"),
          active.flatMap(_.value).contains("true"),
        )
      },
    ),
    // === JSON Query Operators ===
    suite("JSON query operators")(
      test("query with JSON contains (@>) operator") {
        for
          _ <- (dropTable[Profile](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (createTable[Profile]())
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, '{\"email\":\"alice@test.com\",\"verified\":true,\"role\":\"admin\"}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, '{\"email\":\"bob@test.com\",\"verified\":false,\"role\":\"user\"}'::jsonb)".insert
          )
          // Query using @> operator - verify count only since Json is opaque
          verifiedCount <- (
            sql"""SELECT count(*) as count FROM schema_int_profiles WHERE data @> '{"verified":true}'"""
              .queryOne[CountResult]
          )
          // Also verify the email to ensure we got the right record
          verifiedEmail <- (
            sql"""SELECT data->>'email' as value FROM schema_int_profiles WHERE data @> '{"verified":true}'"""
              .queryOne[JsonFieldResult]
          )
        yield assertTrue(
          verifiedCount.exists(_.count == 1),
          verifiedEmail.flatMap(_.value).contains("alice@test.com"),
        )
      },
      test("query with JSON key exists operator") {
        for
          _ <- (dropTable[Event](ifExists = true))
          _ <- (createTable[Event]())
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event1', '{\"tags\":[\"a\"],\"version\":1,\"active\":true}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event2', '{\"version\":2,\"active\":false}'::jsonb)".insert
          )
          // Query using jsonb_exists function (equivalent to ? operator) - find events that have 'tags' key
          // Note: We use jsonb_exists() instead of ? operator because ? is interpreted as JDBC parameter placeholder
          withTags <- (
            sql"""SELECT id, name, metadata FROM schema_int_events WHERE jsonb_exists(metadata, 'tags')""".query[Event]
          )
        yield assertTrue(
          withTags.length == 1,
          withTags.head.name == "Event1",
        )
      },
      test("query with JSON path extraction (->>)") {
        for
          _ <- (dropTable[Profile](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (createTable[Profile]())
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, '{\"email\":\"alice@test.com\",\"verified\":true,\"role\":\"admin\"}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, '{\"email\":\"bob@test.com\",\"verified\":false,\"role\":\"user\"}'::jsonb)".insert
          )
          // Query using ->> operator for text extraction - verify count and extracted role
          adminCount <- (
            sql"""SELECT count(*) as count FROM schema_int_profiles WHERE data->>'role' = 'admin'"""
              .queryOne[CountResult]
          )
          adminRole <- (
            sql"""SELECT data->>'role' as value FROM schema_int_profiles WHERE data->>'role' = 'admin'"""
              .queryOne[JsonFieldResult]
          )
        yield assertTrue(
          adminCount.exists(_.count == 1),
          adminRole.flatMap(_.value).contains("admin"),
        )
      },
      test("query with JSON has any keys (?|) operator") {
        for
          _ <- (dropTable[Event](ifExists = true))
          _ <- (createTable[Event]())
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event1', '{\"tags\":[\"a\"],\"version\":1,\"active\":true}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event2', '{\"priority\":1,\"version\":2}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event3', '{\"version\":3}'::jsonb)".insert
          )
          // Query using jsonb_exists_any function (equivalent to ?| operator) - find events that have 'tags' OR 'priority' keys
          // Use count to avoid JSON decode issues with partial data
          result <- (
            sql"""SELECT count(*) as count FROM schema_int_events WHERE jsonb_exists_any(metadata, array['tags', 'priority'])"""
              .queryOne[CountResult]
          )
        yield assertTrue(result.exists(_.count == 2))
      },
      test("query with JSON has all keys (?&) operator") {
        for
          _ <- (dropTable[Event](ifExists = true))
          _ <- (createTable[Event]())
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event1', '{\"tags\":[\"a\"],\"version\":1,\"active\":true}'::jsonb)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_events (name, metadata) VALUES ('Event2', '{\"version\":2,\"active\":false}'::jsonb)".insert
          )
          // Query using jsonb_exists_all function (equivalent to ?& operator) - find events that have BOTH 'version' AND 'active' keys
          // Use count to avoid JSON decode issues with partial data
          result <- (
            sql"""SELECT count(*) as count FROM schema_int_events WHERE jsonb_exists_all(metadata, array['version', 'active'])"""
              .queryOne[CountResult]
          )
        yield assertTrue(result.exists(_.count == 2))
      },
    ),
    // === JSON Partial Index Integration ===
    suite("JSON partial index integration")(
      test("create partial index on JSON condition") {
        val profiles = Schema[Profile]
          .withIndex(_.userId)
          .where(_.data)
          .jsonHasKey("verified")
          .named("idx_verified_profiles")
          .build

        for
          _       <- (dropTable[Profile](ifExists = true))
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable[User]())
          _       <- (createTable(profiles))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_profiles' AND indexname = 'idx_verified_profiles'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.contains("jsonb_exists"),
          indexes.head.indexdef.contains("verified"),
        )
        end for
      },
      test("create partial index on JSON path condition") {
        val profiles = Schema[Profile]
          .withIndex(_.userId)
          .where(_.data)
          .jsonPath("role")
          .eql("admin")
          .named("idx_admin_profiles")
          .build

        for
          _       <- (dropTable[Profile](ifExists = true))
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable[User]())
          _       <- (createTable(profiles))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_profiles' AND indexname = 'idx_admin_profiles'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.contains("->>"),
          indexes.head.indexdef.contains("admin"),
        )
        end for
      },
    ),
    // === Complex WHERE Conditions ===
    suite("Complex WHERE conditions in indexes")(
      test("partial index with AND condition") {
        val users = Schema[User]
          .withIndex(_.email)
          .where(_.status)
          .eql("active")
          .and(_.age)
          .gte(18)
          .named("idx_adult_active")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_adult_active'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.toLowerCase.contains("and"),
          indexes.head.indexdef.contains("active"),
          indexes.head.indexdef.contains("18"),
        )
        end for
      },
      test("partial index with OR condition") {
        val users = Schema[User]
          .withIndex(_.email)
          .where(_.status)
          .eql("active")
          .or(_.status)
          .eql("pending")
          .named("idx_active_or_pending")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_active_or_pending'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.toLowerCase.contains("or"),
        )
        end for
      },
      test("partial index with grouped conditions") {
        val users = Schema[User]
          .withIndex(_.email)
          .where(_.status)
          .eql("active")
          .andGroup(g => g.where(_.age).gte(18).or(_.age).lte(5))
          .named("idx_active_age_group")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_active_age_group'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          // Check for parentheses indicating grouped conditions
          indexes.head.indexdef.contains("("),
        )
        end for
      },
      test("partial index with IN clause") {
        val users = Schema[User]
          .withIndex(_.email)
          .where(_.status)
          .in(Seq("active", "pending", "review"))
          .named("idx_multi_status")
          .build

        for
          _       <- (dropTable[User](ifExists = true))
          _       <- (createTable(users))
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_users' AND indexname = 'idx_multi_status'""".query[IndexInfo]
          )
        yield assertTrue(
          indexes.nonEmpty,
          indexes.head.indexdef.toLowerCase.contains("in"),
        )
        end for
      },
    ),
    // === Schema-qualified table names with typed Query DSL ===
    suite("Schema-qualified table names")(
      test("typed Query DSL works against a schema-qualified table") {
        for
          _ <- (sql"create schema if not exists prd".insert)
          _ <- (dropTable[QualifiedRow](ifExists = true))
          _ <- (sql"create table prd.schema_int_qualified (id bigint primary key, name text)".insert)
          _ <- (sql"insert into prd.schema_int_qualified (id, name) values (1, 'alice')".insert)
          _ <- (sql"insert into prd.schema_int_qualified (id, name) values (2, 'bob')".insert)
          // Typed Query DSL — exercises both the FROM-clause alias and the WHERE column reference.
          alice <- (Query[QualifiedRow].where(_.name).eq("alice").queryOne[QualifiedRow])
          all   <- (Query[QualifiedRow].all.query[QualifiedRow])
        yield assertTrue(
          alice.exists(_.id == 1L),
          alice.exists(_.name == "alice"),
          all.length == 2,
        )
      }
    ),
    // === Foreign Key with Index ===
    suite("Foreign key with index")(
      test("create table with FK and index on same column") {
        val profiles = Schema[Profile]
          .withIndex(_.userId)
          .named("idx_profile_user")
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .build

        for
          _ <- (dropTable[Profile](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (createTable(profiles))
          // Verify index exists
          indexes <- (
            sql"""SELECT indexname, indexdef FROM pg_indexes
                  WHERE tablename = 'schema_int_profiles' AND indexname = 'idx_profile_user'""".query[IndexInfo]
          )
          // Verify FK constraint by testing cascade delete
          _ <- (
            sql"INSERT INTO schema_int_users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30)".insert
          )
          _ <- (
            sql"INSERT INTO schema_int_profiles (userId, data) VALUES (1, '{\"email\":\"a@b.com\",\"verified\":true,\"role\":\"user\"}'::jsonb)".insert
          )
          countBefore <- (sql"SELECT count(*) as count FROM schema_int_profiles".queryOne[CountResult])
          _           <- (sql"DELETE FROM schema_int_users WHERE id = 1".delete)
          countAfter  <- (sql"SELECT count(*) as count FROM schema_int_profiles".queryOne[CountResult])
        yield assertTrue(
          indexes.nonEmpty,
          countBefore.exists(_.count == 1),
          countAfter.exists(_.count == 0),
        )
        end for
      }
    ),
  ).provideShared(xaLayer) @@ TestAspect.sequential

end SchemaIntegrationSpecs
