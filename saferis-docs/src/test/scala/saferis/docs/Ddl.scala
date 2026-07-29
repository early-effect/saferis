package saferis.docs

import saferis.*
import saferis.Schema.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Ddl extends SaferisDocSpecSuite:

  @tableName("ddl_customers")
  case class Customer(
      @generated @key id: Long,
      name: String,
      email: String,
      status: String = "active",
      notes: Option[String],
  ) derives Table

  @tableName("ddl_schema_users")
  case class SchemaUser(
      @generated @key id: Int,
      name: String,
      email: String,
      status: String,
  ) derives Table

  @tableName("ddl_jobs")
  case class Job(@generated @key id: Int, status: String, retryAt: Option[java.time.Instant]) derives Table

  def doc = page("Data Definition Layer (DDL)")(
    md"""The DDL layer provides type-safe schema management operations.""",
    section("Creating Tables")(
      exampleZIO {
        xa.run(ddl.createTable[Customer](ifNotExists = true)).either
      }.assert {
        case Right(_)  => assertTrue(true)
        case Left(err) => assertTrue(false).label(err.message)
      }
    ),
    section("Other DDL Operations")(
      md"""```scala
import saferis.*

@tableName("ddl_customers")
case class Customer(
  @generated @key id: Long,
  name: String,
  email: String,
  status: String = "active",
  notes: Option[String]
) derives Table

// Drop table
ddl.dropTable[Customer](ifExists = true)

// Truncate table
ddl.truncateTable[Customer]()

// Add column
ddl.addColumn[Customer, String]("new_column")

// Drop column
ddl.dropColumn[Customer]("old_column")

// Drop index
ddl.dropIndex("idx_name", ifExists = true)
```"""
    ),
    section("createTable Options")(
      md"""The `createTable` function accepts optional parameters:

```scala
import saferis.*

@tableName("ddl_my_table")
case class MyTable(@key id: Int, name: String) derives Table

// Default: creates table and indexes for compound primary keys
ddl.createTable[MyTable]()

// Skip table creation if it already exists
ddl.createTable[MyTable](ifNotExists = true)

// Create table without indexes (create them separately later)
ddl.createTable[MyTable](createIndexes = false)
```"""
    ),
    section("Schema DSL for Indexes and Constraints")(
      md"""Use the `Schema` DSL to define indexes, unique constraints, and foreign keys with full DDL generation:""",
      exampleValue {
        // Simple index
        Schema[SchemaUser]
          .withIndex(_.name)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("index") && sql.contains("name"))),
      exampleValue {
        // Unique index
        Schema[SchemaUser]
          .withUniqueIndex(_.email)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("unique") && sql.contains("email"))),
      exampleValue {
        // Compound index on multiple columns
        Schema[SchemaUser]
          .withIndex(_.name)
          .and(_.status)
          .named("idx_name_status")
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("idx_name_status"))),
      exampleValue {
        // Partial index with WHERE clause
        Schema[SchemaUser]
          .withIndex(_.name)
          .where(_.status)
          .eql("active")
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("where") && sql.toLowerCase.contains("active"))),
      exampleValue {
        // Partial unique index - uniqueness only for active users
        Schema[SchemaUser]
          .withUniqueIndex(_.email)
          .where(_.status)
          .eql("active")
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("unique") && sql.toLowerCase.contains("active"))),
      exampleValue {
        // Multiple indexes chained together
        Schema[SchemaUser]
          .withIndex(_.name)
          .withUniqueIndex(_.email)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("name") && sql.contains("email"))),
      exampleValue {
        // Compound unique constraint
        Schema[SchemaUser]
          .withUniqueConstraint(_.name)
          .and(_.status)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("unique"))),
    ),
    section("Creating Tables with Schema")(
      md"""Use `.build` to get an Instance for `ddl.createTable`:""",
      exampleZIO {
        // Build schema with indexes and create table
        val schemaUsers = Schema[SchemaUser]
          .withIndex(_.name)
          .withUniqueIndex(_.email)
          .build

        xa.run(ddl.createTable(schemaUsers)).either
      }.assert {
        case Right(_)  => assertTrue(true)
        case Left(err) => assertTrue(false).label(err.message)
      },
    ),
    section("Partial Indexes via Runtime API")(
      md"""Create partial indexes programmatically using `ddl.createIndex`:""",
      exampleZIO {
        xa.run(for
          _ <- ddl.createTable[Job](createIndexes = false)
          // Create a partial index for pending jobs with retry times
          _ <- ddl.createIndex[Job](
            "idx_pending_retry",
            Seq("retryat"),
            where = Some("status = 'pending'"),
          )
          _    <- dml.insert(Job(-1, "pending", Some(java.time.Instant.now())))
          _    <- dml.insert(Job(-1, "completed", None))
          jobs <- sql"SELECT * FROM ${Table[Job]}".query[Job]
        yield jobs)
          .either
      }.assert {
        case Right(jobs) => assertTrue(jobs.exists(_.status == "pending") && jobs.exists(_.status == "completed"))
        case Left(err)   => assertTrue(false).label(err.message)
      },
    ),
  )
end Ddl
