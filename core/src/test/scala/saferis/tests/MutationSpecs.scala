package saferis.tests

import saferis.*
import zio.json.*
import zio.test.*

object MutationSpecs extends ZIOSpecDefault:

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
    genericLabelTests,
  )

end MutationSpecs
