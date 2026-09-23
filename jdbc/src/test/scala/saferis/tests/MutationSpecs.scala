package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*

object MutationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Test table for mutation specs
  @tableName("test_mutation")
  final case class TestUser(@generated @key id: Int, name: String, age: Int, status: String) derives Table

  // ============================================================================
  // Integration Tests (with PostgresTestContainer)
  // ============================================================================

  val integrationTests = suite("Integration Tests")(
    test("Insert.build.execute inserts a row"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        insertResult <- (
          Insert[TestUser]
            .value(_.name, "Alice")
            .value(_.age, 30)
            .value(_.status, "active")
            .build
            .execute
        )
        queryResult <- (sql"select * from test_mutation where name = ${"Alice"}".queryOne[TestUser])
      yield assertTrue(insertResult == 1) &&
        assertTrue(queryResult.exists(_.name == "Alice")) &&
        assertTrue(queryResult.exists(_.age == 30))
    ,
    test("Insert.returning returns inserted row"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        inserted <- (
          Insert[TestUser]
            .value(_.name, "Bob")
            .value(_.age, 25)
            .value(_.status, "pending")
            .returning
            .queryOne[TestUser]
        )
      yield assertTrue(inserted.exists(_.name == "Bob")) &&
        assertTrue(inserted.exists(_.id > 0))
    ,
    test("Update.build.execute updates rows"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Alice', 30, 'active')".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Bob', 25, 'active')".dml)
        updateResult <- (
          Update[TestUser]
            .set(_.status, "inactive")
            .where(_.name)
            .eq("Alice")
            .build
            .execute
        )
        aliceStatus <- (sql"select status from test_mutation where name = ${"Alice"}".queryValue[String])
        bobStatus   <- (sql"select status from test_mutation where name = ${"Bob"}".queryValue[String])
      yield assertTrue(updateResult == 1) &&
        assertTrue(aliceStatus.contains("inactive")) &&
        assertTrue(bobStatus.contains("active"))
    ,
    test("Update with multiple set clauses"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Alice', 30, 'active')".dml)
        updateResult <- (
          Update[TestUser]
            .set(_.name, "Alice Updated")
            .set(_.age, 31)
            .where(_.name)
            .eq("Alice")
            .build
            .execute
        )
        result <- (sql"select * from test_mutation".queryOne[TestUser])
      yield assertTrue(updateResult == 1) &&
        assertTrue(result.exists(_.name == "Alice Updated")) &&
        assertTrue(result.exists(_.age == 31))
    ,
    test("Update.all updates all rows"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Alice', 30, 'active')".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Bob', 25, 'active')".dml)
        updateResult <- (
          Update[TestUser]
            .set(_.status, "archived")
            .all
            .build
            .execute
        )
        archivedCount <- (sql"select count(*) from test_mutation where status = ${"archived"}".queryValue[Long])
      yield assertTrue(updateResult == 2) &&
        assertTrue(archivedCount.contains(2))
    ,
    test("Delete.build.execute deletes rows"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Alice', 30, 'active')".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Bob', 25, 'inactive')".dml)
        deleteResult <- (
          Delete[TestUser]
            .where(_.status)
            .eq("inactive")
            .build
            .execute
        )
        remaining <- (sql"select count(*) from test_mutation".queryValue[Long])
      yield assertTrue(deleteResult == 1) &&
        assertTrue(remaining.contains(1))
    ,
    test("Delete.all deletes all rows"):
      for
        _ <- (sql"drop table if exists test_mutation".dml)
        _ <- (sql"""create table test_mutation (
                id integer generated always as identity primary key,
                name varchar(255) not null,
                age integer not null,
                status varchar(50) not null
              )""".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Alice', 30, 'active')".dml)
        _            <- (sql"insert into test_mutation (name, age, status) values ('Bob', 25, 'active')".dml)
        deleteResult <- (
          Delete[TestUser].all.build.execute
        )
        remaining <- (sql"select count(*) from test_mutation".queryValue[Long])
      yield assertTrue(deleteResult == 2) &&
        assertTrue(remaining.contains(0)),
  ).provideShared(xaLayer) @@ TestAspect.sequential

  val spec = suite("Mutation DSL")(
    integrationTests
  )

end MutationSpecs
