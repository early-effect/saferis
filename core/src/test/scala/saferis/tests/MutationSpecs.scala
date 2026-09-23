package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.json.*
import zio.test.*

object MutationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Test table for mutation specs
  @tableName("test_mutation")
  final case class TestUser(@generated @key id: Int, name: String, age: Int, status: String) derives Table

  // Generic case class with @label annotations and polymorphic given
  final case class MutationPayload(name: String, value: Int) derives JsonCodec

  @tableName("generic_mutation_test")
  final case class GenericMutationRow[E](
      @generated @key id: Int,
      @label("instance_id") instanceId: String,
      @label("sequence_nr") sequenceNr: Long,
      payload: Json[E],
  )
  object GenericMutationRow:
    given [E: JsonCodec]: Table[GenericMutationRow[E]] = Table.derived

  // ============================================================================
  // SQL Generation Tests (Unit Tests)
  // ============================================================================

  val sqlGenerationTests = suite("SQL Generation")(
    suite("Insert")(
      test("single value generates correct SQL"):
        val frag = Insert[TestUser].value(_.name, "Alice").build
        assertTrue(frag.sql == "insert into test_mutation (name) values ($1)") &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1)
      ,
      test("multiple values generates correct SQL"):
        val frag = Insert[TestUser]
          .value(_.name, "Alice")
          .value(_.age, 30)
          .value(_.status, "active")
          .build
        assertTrue(frag.sql == "insert into test_mutation (name, age, status) values ($1, $2, $3)") &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3)
      ,
      test("returning appends RETURNING clause"):
        val frag = Insert[TestUser].value(_.name, "Alice").returning
        assertTrue(frag.sql.contains("returning *")),
    ),
    suite("Update")(
      test("single set with where generates correct SQL"):
        val frag = Update[TestUser]
          .set(_.name, "Bob")
          .where(_.id)
          .eq(1)
          .build
        assertTrue(frag.sql.contains("update test_mutation set name = $1")) &&
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2) // name + id
      ,
      test("multiple set clauses generates correct SQL"):
        val frag = Update[TestUser]
          .set(_.name, "Bob")
          .set(_.age, 25)
          .where(_.id)
          .eq(1)
          .build
        assertTrue(frag.sql.contains("update test_mutation set name = $1, age = $2")) &&
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3) // name + age + id
      ,
      test("multiple where clauses AND together"):
        val frag = Update[TestUser]
          .set(_.status, "active")
          .where(_.age)
          .gte(18)
          .where(_.status)
          .neq("banned")
          .build
        assertTrue(frag.sql.contains("where")) &&
        assertTrue(frag.sql.contains(" and "))
      ,
      test(".all allows building without where"):
        val frag = Update[TestUser]
          .set(_.status, "inactive")
          .all
          .build
        assertTrue(frag.sql == "update test_mutation set status = $1") &&
        assertTrue(!frag.sql.contains("where"))
      ,
      test("returning appends RETURNING clause"):
        val frag = Update[TestUser]
          .set(_.name, "Bob")
          .where(_.id)
          .eq(1)
          .returning
        assertTrue(frag.sql.contains("returning *")),
    ),
    suite("Delete")(
      test("where generates correct SQL"):
        val frag = Delete[TestUser]
          .where(_.id)
          .eq(1)
          .build
        assertTrue(frag.sql.contains("delete from test_mutation")) &&
        assertTrue(frag.sql.contains("where"))
      ,
      test("multiple where clauses AND together"):
        val frag = Delete[TestUser]
          .where(_.status)
          .eq("inactive")
          .where(_.age)
          .lt(18)
          .build
        assertTrue(frag.sql.contains(" and "))
      ,
      test(".all allows building without where"):
        val frag = Delete[TestUser].all.build
        assertTrue(frag.sql == "delete from test_mutation") &&
        assertTrue(!frag.sql.contains("where"))
      ,
      test("returning appends RETURNING clause"):
        val frag = Delete[TestUser]
          .where(_.id)
          .eq(1)
          .returning
        assertTrue(frag.sql.contains("returning *")),
    ),
    suite("WHERE operators")(
      test("eq generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.id).eq(1).build
        assertTrue(frag.sql.contains("="))
      ,
      test("neq generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.status).neq("banned").build
        assertTrue(frag.sql.contains("<>"))
      ,
      test("lt generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.age).lt(18).build
        assertTrue(frag.sql.contains("<"))
      ,
      test("lte generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.age).lte(18).build
        assertTrue(frag.sql.contains("<="))
      ,
      test("gt generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.age).gt(18).build
        assertTrue(frag.sql.contains(">"))
      ,
      test("gte generates correct SQL"):
        val frag = Update[TestUser].set(_.name, "X").where(_.age).gte(18).build
        assertTrue(frag.sql.contains(">="))
      ,
      test("isNull generates correct SQL"):
        val frag = Delete[TestUser].where(_.status).isNull().build
        assertTrue(frag.sql.contains("is null"))
      ,
      test("isNotNull generates correct SQL"):
        val frag = Delete[TestUser].where(_.status).isNotNull().build
        assertTrue(frag.sql.contains("is not null")),
    ),
  )

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

  // ============================================================================
  // Generic Type with @label Tests (Polymorphic Given)
  // ============================================================================

  val genericLabelTests = suite("Generic type with @label and polymorphic given")(
    test("Insert.value uses @label for column name in SQL"):
      val frag = Insert[GenericMutationRow[MutationPayload]]
        .value(_.instanceId, "test-1")
        .value(_.sequenceNr, 42L)
        .build
      // Should use "instance_id" and "sequence_nr" (from @label)
      assertTrue(
        frag.sql.contains("instance_id"),
        frag.sql.contains("sequence_nr"),
        !frag.sql.contains("instanceId"),
        !frag.sql.contains("sequenceNr"),
      )
    ,
    test("Update.set uses @label for column name in SQL"):
      val frag = Update[GenericMutationRow[MutationPayload]]
        .set(_.sequenceNr, 100L)
        .where(_.instanceId)
        .eq("test-1")
        .build
      // Should use "sequence_nr" in SET and "instance_id" in WHERE
      assertTrue(
        frag.sql.contains("set sequence_nr"),
        frag.sql.contains("instance_id ="),
        !frag.sql.contains("sequenceNr"),
        !frag.sql.contains("instanceId"),
      )
    ,
    test("Delete.where uses @label for column name in SQL"):
      val frag = Delete[GenericMutationRow[MutationPayload]]
        .where(_.instanceId)
        .eq("test-1")
        .build
      // Should use "instance_id" (from @label)
      assertTrue(
        frag.sql.contains("instance_id ="),
        !frag.sql.contains("instanceId"),
      ),
  )

  val spec = suite("Mutation DSL")(
    sqlGenerationTests,
    integrationTests,
    genericLabelTests,
  )

end MutationSpecs
