package saferis.tests

import saferis.*
import saferis.Schema.*
import saferis.ddl.*
import saferis.mysql.MySQLDialect
import saferis.postgres.PostgresDialect
import saferis.spark.SparkDialect
import saferis.sqlite.SQLiteDialect
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*

/** Integration tests for Schema validation feature. */
object SchemaValidationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  given Dialect = PostgresDialect

  // === Test Tables ===

  @tableName("validation_users")
  final case class User(
      @generated @key id: Int,
      name: String,
      email: String,
      age: Option[Int],
  ) derives Table

  @tableName("validation_orders")
  final case class Order(
      @generated @key id: Int,
      @label("user_id") userId: Int,
      amount: BigDecimal,
  ) derives Table

  @tableName("quoted_name_cols")
  final case class QuotedColumn(@key id: Int, name: String) derives Table

  @tableName("fk_email_users")
  final case class FkEmailUser(@key id: Int, email: String) derives Table

  @tableName("fk_email_children")
  final case class FkEmailChild(@key id: Int, email: String) derives Table

  // Helper to extract validation issues from SaferisError.SchemaValidation
  extension [R](zio: ZIO[R, SaferisError, Unit])
    def schemaValidationIssues: ZIO[R, Nothing, List[SchemaIssue]] =
      zio.exit.flatMap:
        case Exit.Failure(cause) =>
          cause.failureOption match
            case Some(SaferisError.SchemaValidation(issues)) => ZIO.succeed(issues)
            case other => ZIO.die(new IllegalStateException(s"Expected SchemaValidation, got $other"))
        case Exit.Success(_) =>
          ZIO.die(new IllegalStateException("verify succeeded"))

  val spec = suite("Schema Validation")(
    suite("Basic verification")(
      test("verify succeeds when schema matches") {
        val schema = Schema[User].build
        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(schema))
          _ <- (Schema(schema).verify)
        yield assertCompletes
      },
      test("verify fails with TableNotFound when table missing") {
        val schema = Schema[User].build
        for
          _      <- (dropTable[User](ifExists = true))
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.TableNotFound => true; case _ => false })
      },
      test("verify fails with MissingColumn when column missing") {
        for
          _ <- (sql"DROP TABLE IF EXISTS validation_users".execute)
          // Create table missing the 'age' column
          _ <- (
            sql"CREATE TABLE validation_users (id SERIAL PRIMARY KEY, name VARCHAR(255) NOT NULL, email VARCHAR(255) NOT NULL)".execute
          )
          schema = Schema[User].build
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists {
          case SchemaIssue.MissingColumn(_, "age", _) => true
          case _                                      => false
        })
      },
      test("verify fails with ExtraColumn when DB has extra columns") {
        val schema = Schema[User].build
        for
          _      <- (dropTable[User](ifExists = true))
          _      <- (createTable(schema))
          _      <- (sql"ALTER TABLE validation_users ADD COLUMN extra_col VARCHAR(100)".execute)
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists {
          case SchemaIssue.ExtraColumn(_, "extra_col", _) => true
          case _                                          => false
        })
        end for
      },
      test("verify succeeds with checkExtraColumns = false") {
        val schema  = Schema[User].build
        val options = VerifyOptions(checkExtraColumns = false)
        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(schema))
          _ <- (sql"ALTER TABLE validation_users ADD COLUMN extra_col VARCHAR(100)".execute)
          _ <- (Schema(schema).verifyWith(options))
        yield assertCompletes
      },
      test("verify fails with NullabilityMismatch when nullability differs") {
        for
          _ <- (sql"DROP TABLE IF EXISTS validation_users".execute)
          // Create table with age as NOT NULL instead of nullable
          _ <- (
            sql"CREATE TABLE validation_users (id SERIAL PRIMARY KEY, name VARCHAR(255) NOT NULL, email VARCHAR(255) NOT NULL, age INT NOT NULL)".execute
          )
          schema = Schema[User].build
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists {
          case SchemaIssue.NullabilityMismatch(_, "age", true, false) => true
          case _                                                      => false
        })
      },
      test("verify fails with TypeMismatch when the catalog type differs") {
        for
          _ <- (sql"DROP TABLE IF EXISTS validation_orders".execute)
          _ <- (sql"DROP TABLE IF EXISTS validation_users".execute)
          _ <- (
            sql"""CREATE TABLE validation_users (
                  id SERIAL PRIMARY KEY,
                  name INTEGER NOT NULL,
                  email VARCHAR(255) NOT NULL,
                  age INT
                )""".execute
          )
          schema = Schema[User].build
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists {
          case SchemaIssue.TypeMismatch(_, "name", _, "int4") => true
          case _                                              => false
        })
      },
      test("verify fails with PrimaryKeyMismatch when the primary key differs") {
        for
          _ <- (sql"DROP TABLE IF EXISTS validation_orders".execute)
          _ <- (sql"DROP TABLE IF EXISTS validation_users".execute)
          _ <- (
            sql"""CREATE TABLE validation_users (
                  id INT NOT NULL,
                  name VARCHAR(255) NOT NULL,
                  email VARCHAR(255) NOT NULL PRIMARY KEY,
                  age INT
                )""".execute
          )
          schema = Schema[User].build
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists {
          case SchemaIssue.PrimaryKeyMismatch(_, expected, actual) =>
            expected.map(_.toLowerCase) == Seq("id") && actual == Seq("email")
          case _ => false
        })
      },
      test("quoted Name is MissingColumn for name and ExtraColumn for Name") {
        for
          _ <- (sql"DROP TABLE IF EXISTS quoted_name_cols".execute)
          _ <- (
            sql"""CREATE TABLE quoted_name_cols (
                  id INTEGER PRIMARY KEY,
                  "Name" VARCHAR(255) NOT NULL
                )""".execute
          )
          issues <- (Schema[QuotedColumn].verify).schemaValidationIssues
        yield assertTrue(
          issues.collect { case SchemaIssue.MissingColumn(_, column, _) => column } == List("name"),
          issues.collect { case SchemaIssue.ExtraColumn(_, column, _) => column } == List("Name"),
          issues.length == 2,
        )
      },
    ),
    suite("Index verification")(
      test("verify fails with MissingIndex when expected index missing") {
        val schema = Schema[User]
          .withIndex(_.email)
          .named("idx_users_email")
          .build
        for
          _      <- (dropTable[User](ifExists = true))
          _      <- (createTable[User]())
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.MissingIndex => true; case _ => false })
      },
      test("verify succeeds when index exists") {
        val schema = Schema[User]
          .withIndex(_.email)
          .named("idx_users_email")
          .build
        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable(schema))
          _ <- (Schema(schema).verify)
        yield assertCompletes
      },
      test("verify succeeds with checkIndexes = false even if index missing") {
        val schema = Schema[User]
          .withIndex(_.email)
          .named("idx_users_email")
          .build
        val options = VerifyOptions(checkIndexes = false)
        for
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (Schema(schema).verifyWith(options))
        yield assertCompletes
      },
    ),
    suite("Foreign key verification")(
      test("verify fails with MissingForeignKey when FK missing") {
        val ordersSchema = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .build
        for
          _      <- (dropTable[Order](ifExists = true))
          _      <- (dropTable[User](ifExists = true))
          _      <- (createTable[User]())
          _      <- (createTable[Order]())
          issues <- (Schema(ordersSchema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.MissingForeignKey => true; case _ => false })
      },
      test("verify succeeds when FK exists") {
        val ordersSchema = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .build
        for
          _ <- (dropTable[Order](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (createTable(ordersSchema))
          _ <- (Schema(ordersSchema).verify)
        yield assertCompletes
      },
      test("duplicate uq_email on another table does not change the referenced table") {
        val schema = Schema[FkEmailChild]
          .withForeignKey(_.email)
          .references[FkEmailUser](_.email)
          .build
        for
          _ <- (sql"DROP TABLE IF EXISTS fk_email_children".execute)
          _ <- (sql"DROP TABLE IF EXISTS fk_email_orders".execute)
          _ <- (sql"DROP TABLE IF EXISTS fk_email_users".execute)
          _ <- (
            sql"""CREATE TABLE fk_email_users (
                  id INTEGER PRIMARY KEY,
                  email VARCHAR(255) NOT NULL,
                  CONSTRAINT uq_email UNIQUE (email)
                )""".execute
          )
          // A second UNIQUE named uq_email fails: Postgres names the backing index uq_email,
          // and that relation name is unique per schema. A foreign key can reuse the name.
          // key_column_usage then has two uq_email rows. confrelid must still be fk_email_users.
          _ <- (
            sql"""CREATE TABLE fk_email_orders (
                  id INTEGER PRIMARY KEY,
                  email VARCHAR(255) NOT NULL,
                  CONSTRAINT uq_email FOREIGN KEY (email) REFERENCES fk_email_users (email)
                )""".execute
          )
          _ <- (
            sql"""CREATE TABLE fk_email_children (
                  id INTEGER PRIMARY KEY,
                  email VARCHAR(255) NOT NULL,
                  FOREIGN KEY (email) REFERENCES fk_email_users (email)
                )""".execute
          )
          _     <- (Schema(schema).verify)
          found <- (SchemaIntrospection.introspect("fk_email_children"))
        yield assertTrue(
          found.exists(
            _.foreignKeys.exists(fk =>
              fk.toTable == "fk_email_users" &&
                fk.fromColumns == Seq("email") &&
                fk.toColumns == Seq("email")
            )
          )
        )
        end for
      },
    ),
    suite("Unique constraint verification")(
      test("verify fails with MissingUniqueConstraint when constraint missing") {
        val schema = Schema[User]
          .withUniqueConstraint(_.email)
          .named("uq_users_email")
          .build
        for
          _      <- (dropTable[Order](ifExists = true))
          _      <- (dropTable[User](ifExists = true))
          _      <- (createTable[User]())
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.MissingUniqueConstraint => true; case _ => false })
      },
      test("verify succeeds when unique constraint exists") {
        val schema = Schema[User]
          .withUniqueConstraint(_.email)
          .named("uq_users_email")
          .build
        for
          _     <- (dropTable[Order](ifExists = true))
          _     <- (dropTable[User](ifExists = true))
          _     <- (createTable(schema))
          _     <- (Schema(schema).verify)
          found <- (SchemaIntrospection.introspect("validation_users"))
        yield assertTrue(
          found.exists(_.uniqueConstraints.exists(_.columns == Seq("email")))
        )
      },
      test("a unique index is not a unique constraint") {
        val schema = Schema[User]
          .withUniqueConstraint(_.email)
          .named("uq_users_email")
          .build
        for
          _      <- (dropTable[Order](ifExists = true))
          _      <- (dropTable[User](ifExists = true))
          _      <- (createTable[User]())
          _      <- (sql"CREATE UNIQUE INDEX uq_users_email ON validation_users (email)".execute)
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.MissingUniqueConstraint => true; case _ => false })
      },
      test("a partial unique index is not a unique constraint") {
        val schema = Schema[User]
          .withUniqueConstraint(_.email)
          .named("uq_users_email")
          .build
        for
          _ <- (dropTable[Order](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          _ <- (createTable[User]())
          _ <- (
            sql"CREATE UNIQUE INDEX uq_users_email_partial ON validation_users (email) WHERE age IS NOT NULL".execute
          )
          issues <- (Schema(schema).verify).schemaValidationIssues
        yield assertTrue(issues.exists { case _: SchemaIssue.MissingUniqueConstraint => true; case _ => false })
      },
    ),
    suite("VerifyOptions presets")(
      test("VerifyOptions.minimal only checks table and columns") {
        // Build schema with index and unique constraint
        val withIndex = Schema[User].withIndex(_.email).build
        val schema    = Schema(withIndex).withUniqueConstraint(_.name).build
        for
          _ <- (dropTable[Order](ifExists = true))
          _ <- (dropTable[User](ifExists = true))
          // Create table without index or unique constraint
          _ <- (createTable[User]())
          _ <- (sql"ALTER TABLE validation_users ADD COLUMN extra_col VARCHAR(100)".execute)
          // Minimal should pass despite missing index/constraint and extra column
          _ <- (Schema(schema).verifyWith(VerifyOptions.minimal))
        yield assertCompletes
        end for
      }
    ),
    suite("Dialects without catalogs")(
      test("MySQL, SQLite, and Spark verify fail with Unsupported") {
        def unsupported(dialect: Dialect) =
          Schema[User]
            .verify(using dialect)
            .exit
            .map: exit =>
              assertTrue:
                exit match
                  case Exit.Failure(cause) =>
                    cause.failureOption match
                      case Some(SaferisError.Unsupported(message)) => message.contains(dialect.name)
                      case _                                       => false
                  case Exit.Success(_) => false
        for
          mysql  <- unsupported(MySQLDialect)
          sqlite <- unsupported(SQLiteDialect)
          spark  <- unsupported(SparkDialect)
        yield mysql && sqlite && spark
      }
    ),
  ).provideShared(xaLayer) @@ TestAspect.sequential

end SchemaValidationSpecs
