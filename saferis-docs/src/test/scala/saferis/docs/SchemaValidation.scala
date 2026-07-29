package saferis.docs

import saferis.*
import saferis.Schema.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object SchemaValidation extends SaferisDocSpecSuite:

  @tableName("verify_users")
  case class VerifyUser(@generated @key id: Int, name: String, email: String) derives Table

  @tableName("verify_users_indexed")
  case class VerifyUserIndexed(@generated @key id: Int, name: String, email: String) derives Table

  @tableName("options_users")
  case class OptionsUser(@generated @key id: Int, name: String) derives Table

  @tableName("verify_orders")
  case class VerifyOrder(
    @generated @key id: Int,
    userId: Int,
    amount: BigDecimal,
    status: String,
  ) derives Table

  @tableName("verify_customers")
  case class VerifyCustomer(@generated @key id: Int, email: String) derives Table

  @tableName("startup_customers")
  case class StartupCustomer(@generated @key id: Int, email: String) derives Table

  @tableName("startup_orders")
  case class StartupOrder(@generated @key id: Int, customerId: Int, amount: BigDecimal) derives Table

  def doc = page("Schema Validation")(
    md"""Saferis provides runtime schema validation to verify that your table definitions match the actual database schema. This is useful for detecting schema drift, validating migrations, and ensuring consistency between code and database.""",
    section("Basic Verification")(
      md"""Use `Schema(instance).verify` to validate a schema against the database:""",
      exampleZIO {
        val schema = Schema[VerifyUser].build
        xa.run(for
          _ <- ddl.createTable(schema)
          // Verify succeeds when schema matches
          _ <- Schema(schema).verify
        yield "Schema verification passed").either
      }.assert {
        case Right(msg) => assertTrue(msg.contains("passed"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
      md"""When verification fails, it returns a `SaferisError.SchemaValidation` containing a list of issues:""",
      exampleZIO {
        val schema = Schema[VerifyUserIndexed].withIndex(_.email).named("idx_verify_email").build
        xa.run(for
          // Create table without the index
          _      <- ddl.createTable[VerifyUserIndexed](ifNotExists = true)
          // Verification will find the missing index
          result <- Schema(schema).verify.either
        yield result match
          case Left(SaferisError.SchemaValidation(issues)) =>
            issues.map(_.description).mkString("\n")
          case Left(e)  => s"Unexpected error: ${e.message}"
          case Right(_) => "Verification passed"
        ).either
      }.assert {
        case Right(msg) => assertTrue(msg.nonEmpty && !msg.startsWith("Unexpected"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
    ),
    section("VerifyOptions")(
      md"""Customize verification behavior with `VerifyOptions`:""",
      exampleValue {
        // Available options
        val options = VerifyOptions(
          checkExtraColumns = true,      // Report columns in DB not defined in schema
          checkIndexes = true,           // Verify indexes exist
          checkUniqueConstraints = true, // Verify unique constraints exist
          checkForeignKeys = true,       // Verify foreign keys exist
          checkNullability = true,       // Verify column nullable/NOT NULL matches
          checkTypes = true,             // Verify column types match
          strictTypeMatching = false,    // Require exact type match (not just compatible)
          strictNameMatching = false,    // Fail if index/constraint name differs
        )
        options
      }.assert(opts => assertTrue(opts.checkExtraColumns && opts.checkIndexes && !opts.strictNameMatching)),
      md"""### Preset Options

| Preset | Description |
|--------|-------------|
| `VerifyOptions.default` | Check everything except strict name matching |
| `VerifyOptions.minimal` | Only check table and columns exist |
| `VerifyOptions.strict` | Check everything including exact names |""",
      exampleZIO {
        val schema = Schema[OptionsUser].build
        xa.run(for
          _ <- ddl.createTable[OptionsUser](ifNotExists = true)
          // Add an extra column to the database
          _ <- sql"ALTER TABLE options_users ADD COLUMN IF NOT EXISTS extra VARCHAR(100)".execute

          // Default verification fails due to extra column
          defaultResult <- Schema(schema).verify.either

          // Minimal verification ignores extra columns
          minimalResult <- Schema(schema).verifyWith(VerifyOptions.minimal).either
        yield (
          defaultResult.fold(e => s"Default failed: ${e.message.take(60)}...", _ => "passed"),
          minimalResult.fold(_.message, _ => "Minimal passed"),
        )).either
      }.assert {
        case Right((defaultMsg, minimalMsg)) =>
          assertTrue(defaultMsg.contains("Default failed") && minimalMsg.contains("Minimal passed"))
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("SchemaIssue Types")(
      md"""Verification returns specific issue types defined in [SchemaIssue.scala](https://github.com/russwyte/saferis/blob/main/core/src/main/scala/saferis/SchemaIssue.scala):

### Column Issues

| Issue | Description |
|-------|-------------|
| `TableNotFound` | Table does not exist in database |
| `MissingColumn` | Expected column is missing |
| `TypeMismatch` | Column has wrong type |
| `NullabilityMismatch` | Column nullable/NOT NULL differs |
| `ExtraColumn` | Column in DB not in schema |
| `PrimaryKeyMismatch` | Primary key columns don't match |

### Constraint Issues

| Issue | Description |
|-------|-------------|
| `MissingIndex` | Expected index not found |
| `IndexNameMismatch` | Index exists but with different name |
| `MissingUniqueConstraint` | Expected unique constraint not found |
| `UniqueConstraintNameMismatch` | Constraint exists but with different name |
| `MissingForeignKey` | Expected foreign key not found |
| `ForeignKeyNameMismatch` | Foreign key exists but with different name |""",
    ),
    section("Verifying Complex Schemas")(
      md"""Verify schemas with indexes, unique constraints, and foreign keys:""",
      exampleZIO {
        // Build a schema with index and foreign key
        val ordersSchema = Schema[VerifyOrder]
          .withIndex(_.status)
          .named("idx_order_status")
          .withForeignKey(_.userId)
          .references[VerifyCustomer](_.id)
          .onDelete(Cascade)
          .build

        xa.run(for
          _ <- ddl.createTable[VerifyCustomer](ifNotExists = true)
          _ <- ddl.createTable(ordersSchema)
          // Full verification including FK
          _ <- Schema(ordersSchema).verify
        yield "All constraints verified").either
      }.assert {
        case Right(msg) => assertTrue(msg.contains("verified"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
    ),
    section("Type Compatibility")(
      md"""By default, type checking uses "type families" for compatibility. Types in the same family are considered compatible:

| Family | Compatible Types |
|--------|------------------|
| Integer | `integer`, `int`, `int4`, `serial`, `bigint`, `int8`, `bigserial`, `smallint` |
| Text | `varchar`, `text`, `character varying`, `char`, `bpchar` |
| Numeric | `numeric`, `decimal`, `real`, `float`, `double precision` |
| Boolean | `boolean`, `bool`, `bit` |
| Timestamp | `timestamp`, `timestamptz`, `timestamp with time zone` |
| JSON | `json`, `jsonb` |

Use `strictTypeMatching = true` to require exact type matches.""",
    ),
    section("Application Startup Validation")(
      md"""A common pattern is to verify schemas at application startup. The program below is
exactly the shape you'd put in `ZIOAppDefault.run`, here validating two schemas
against the live database:""",
      exampleZIO {
        def validateSchemas: ZIO[Any, SaferisError, Unit] =
          val customerSchema = Schema[StartupCustomer].build
          val orderSchema    = Schema[StartupOrder].build
          xa.run(for
            _ <- ddl.createTable(customerSchema)
            _ <- ddl.createTable(orderSchema)
            _ <- Schema(customerSchema).verify
            _ <- Schema(orderSchema).verify
          yield ())

        validateSchemas
          .tapError(e => ZIO.logError(s"Schema validation failed: ${e.message}"))
          .as("All schemas validated successfully")
          .either
      }.assert {
        case Right(msg) => assertTrue(msg.contains("validated"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
    ),
  )
end SchemaValidation
