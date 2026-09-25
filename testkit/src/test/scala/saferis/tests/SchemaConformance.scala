package saferis.tests

import saferis.*
import saferis.Schema.*
import saferis.ddl.*
import saferis.tests.SqlSessionConformance.withDialect

import zio.{test as _, *}
import zio.test.*

/** `Schema.verify` on any database whose dialect reads a catalog. Tables come from the target's DDL and are broken with
  * `alter table ... add/drop column`, which every catalog database here speaks, so no test assumes Postgres.
  */
object SchemaConformance:
  @tableName("verify_users")
  final case class User(@generated @key id: Int, name: String, email: String, age: Option[Int]) derives Table

  @tableName("verify_orders")
  final case class Order(@generated @key id: Int, @label("user_id") userId: Int, amount: BigDecimal) derives Table

  private def issuesOf(verify: ZIO[SqlSession, SaferisError, Unit]): ZIO[SqlSession, SaferisError, List[SchemaIssue]] =
    verify.foldZIO(
      {
        case SaferisError.SchemaValidation(issues) => ZIO.succeed(issues)
        case other                                 => ZIO.fail(other)
      },
      _ => ZIO.succeed(Nil),
    )

  private def clean(using Dialect): ZIO[SqlSession, SaferisError, Unit] =
    dropTable[Order](ifExists = true) *> dropTable[User](ifExists = true).unit

  def conformance =
    suite("schema verification")(
      test("a table created from the schema verifies"):
        withDialect:
          for
            _ <- clean
            _ <- createTable[User]()
            _ <- Schema[User].verify
          yield assertCompletes
      ,
      test("a missing table is TableNotFound"):
        withDialect:
          for
            _      <- clean
            issues <- issuesOf(Schema[User].verify)
          yield assertTrue(issues.exists { case _: SchemaIssue.TableNotFound => true; case _ => false })
      ,
      test("a dropped column is MissingColumn and an added one is ExtraColumn"):
        withDialect:
          for
            _      <- clean
            _      <- createTable[User]()
            _      <- sql"alter table verify_users drop column age".dml
            _      <- sql"alter table verify_users add column extra_col varchar(100)".dml
            issues <- issuesOf(Schema[User].verify)
          yield assertTrue(
            issues.exists { case SchemaIssue.MissingColumn(_, "age", _) => true; case _ => false },
            issues.exists { case SchemaIssue.ExtraColumn(_, "extra_col", _) => true; case _ => false },
          )
      ,
      test("a column of another type is TypeMismatch"):
        withDialect:
          for
            _      <- clean
            _      <- createTable[User]()
            _      <- sql"alter table verify_users drop column name".dml
            _      <- sql"alter table verify_users add column name integer".dml
            issues <- issuesOf(Schema[User].verify)
          yield assertTrue(issues.exists { case SchemaIssue.TypeMismatch(_, "name", _, _) => true; case _ => false })
      ,
      test("an index from the schema verifies, and a missing one is MissingIndex"):
        val schema = Schema[User].withIndex(_.name).named(IndexName("idx_verify_users_name")).build
        withDialect:
          for
            _       <- clean
            _       <- createTable[User]()
            missing <- issuesOf(Schema(schema).verify)
            _       <- clean
            _       <- createTable(schema)
            present <- issuesOf(Schema(schema).verify)
          yield assertTrue(
            missing.exists { case _: SchemaIssue.MissingIndex => true; case _ => false },
            present.isEmpty,
          )
      ,
      test("a unique constraint from the schema verifies, and a missing one is MissingUniqueConstraint"):
        val schema = Schema[User].withUniqueConstraint(_.email).named(ConstraintName("uq_verify_users_email")).build
        withDialect:
          for
            _       <- clean
            _       <- createTable[User]()
            missing <- issuesOf(Schema(schema).verify)
            _       <- clean
            _       <- createTable(schema)
            present <- issuesOf(Schema(schema).verify)
          yield assertTrue(
            missing.exists { case _: SchemaIssue.MissingUniqueConstraint => true; case _ => false },
            present.isEmpty,
          )
      ,
      test("a foreign key from the schema verifies, and a missing one is MissingForeignKey"):
        val orders = Schema[Order].withForeignKey(_.userId).references[User](_.id).onDelete(Cascade).build
        withDialect:
          for
            _       <- clean
            _       <- createTable[User]()
            _       <- createTable[Order]()
            missing <- issuesOf(Schema(orders).verify)
            _       <- dropTable[Order](ifExists = true)
            _       <- createTable(orders)
            present <- issuesOf(Schema(orders).verify)
          yield assertTrue(
            missing.exists { case _: SchemaIssue.MissingForeignKey => true; case _ => false },
            present.isEmpty,
          ),
    ) @@ TestAspect.sequential
end SchemaConformance
