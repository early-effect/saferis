package saferis.docs

import saferis.*
import saferis.Schema.*
import saferis.docs.DocsTransactor.xa
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object ForeignKeys extends SaferisDocSpecSuite:

  // Parent table
  @tableName("fk_users")
  case class FkUser(@generated @key id: Int, name: String) derives Table

  // Child table with foreign key column
  @tableName("fk_orders")
  case class FkOrder(@generated @key id: Int, userId: Int, amount: BigDecimal) derives Table

  @tableName("action_users")
  case class ActionUser(@generated @key id: Int, name: String) derives Table

  @tableName("action_orders")
  case class ActionOrder(@generated @key id: Int, userId: Int) derives Table

  @tableName("named_users")
  case class NamedUser(@generated @key id: Int, name: String) derives Table

  @tableName("named_orders")
  case class NamedOrder(@generated @key id: Int, userId: Int) derives Table

  // Parent with compound primary key
  @tableName("compound_products")
  case class CompoundProduct(@key tenantId: String, @key sku: String, name: String) derives Table

  // Child referencing the compound key
  @tableName("compound_inventory")
  case class CompoundInventory(
    @generated @key id: Int,
    tenantId: String,
    productSku: String,
    quantity: Int,
  ) derives Table

  @tableName("multi_users")
  case class MultiUser(@generated @key id: Int, name: String) derives Table

  @tableName("multi_products")
  case class MultiProduct(@generated @key id: Int, name: String) derives Table

  @tableName("multi_order_items")
  case class MultiOrderItem(
    @generated @key id: Int,
    userId: Int,
    productId: Int,
    quantity: Int,
  ) derives Table

  @tableName("type_users")
  case class TypeUser(@generated @key id: Int, name: String) derives Table

  @tableName("type_orders")
  case class TypeOrder(@generated @key id: Int, userId: Int, userName: String) derives Table

  def doc = page("Foreign Key Support")(
    md"""Saferis provides a type-safe `Schema` builder for defining foreign key constraints. The builder uses Scala 3 macros to extract column names at compile time, ensuring type safety and catching errors early.""",
    section("Basic Foreign Keys")(
      md"""Define a foreign key using `Schema[A].withForeignKey(_.column).references[Table](_.column)`:""",
      exampleValue {
        // Define the foreign key relationship and get DDL
        Schema[FkOrder]
          .withForeignKey(_.userId)
          .references[FkUser](_.id)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("foreign") && sql.contains("userId"))),
      exampleZIO {
        // Build the instance for use with ddl.createTable
        val orders = Schema[FkOrder]
          .withForeignKey(_.userId)
          .references[FkUser](_.id)
          .build

        // Create tables with foreign key constraint
        xa.run(for
          _      <- ddl.createTable[FkUser](ifNotExists = true)
          _      <- ddl.createTable(orders)
          _      <- dml.insert(FkUser(-1, "Alice"))
          _      <- dml.insert(FkOrder(-1, 1, BigDecimal(99.99)))
          result <- sql"SELECT * FROM ${Table[FkOrder]}".query[FkOrder]
        yield result).either
      }.assert {
        case Right(orders) => assertTrue(orders.exists(_.amount == BigDecimal(99.99)))
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
    section("ON DELETE and ON UPDATE Actions")(
      md"""Specify what happens when a referenced row is deleted or updated:""",
      exampleValue {
        // CASCADE: Deleting a user deletes their orders
        Schema[ActionOrder]
          .withForeignKey(_.userId)
          .references[ActionUser](_.id)
          .onDelete(Cascade)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("cascade"))),
      exampleValue {
        // SET NULL: Sets FK column to NULL when parent is deleted
        // Note: The FK column should be nullable (Option[T]) for SET NULL to work properly at runtime
        Schema[ActionOrder]
          .withForeignKey(_.userId)
          .references[ActionUser](_.id)
          .onDelete(SetNull)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("set null") || sql.toLowerCase.contains("setnull"))),
      md"""Available actions (import `saferis.Schema.*` to use short names):

| Action | Description |
|--------|-------------|
| `NoAction` | Fail if referenced row is deleted/updated (default) |
| `Cascade` | Delete/update child rows when parent is deleted/updated |
| `SetNull` | Set the FK column to NULL |
| `SetDefault` | Set the FK column to its default value |
| `Restrict` | Fail immediately (same as NoAction but checked immediately) |""",
    ),
    section("Named Constraints")(
      md"""Give your foreign key constraint a custom name:""",
      exampleValue {
        Schema[NamedOrder]
          .withForeignKey(_.userId)
          .references[NamedUser](_.id)
          .onDelete(Cascade)
          .named("fk_order_user")
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("fk_order_user"))),
    ),
    section("Compound Foreign Keys")(
      md"""Reference a composite primary key with multiple columns using `.and()`:""",
      exampleValue {
        // Reference multiple columns using .and()
        Schema[CompoundInventory]
          .withForeignKey(_.tenantId)
          .and(_.productSku)
          .references[CompoundProduct](_.tenantId)
          .and(_.sku)
          .onDelete(Cascade)
          .ddl()
          .sql
      }.assert(sql =>
        assertTrue(
          sql.contains("tenantId") && sql.contains("productSku") && sql.toLowerCase.contains("cascade"),
        ),
      ),
      exampleZIO {
        // Build and create tables with compound FK
        val inventory = Schema[CompoundInventory]
          .withForeignKey(_.tenantId)
          .and(_.productSku)
          .references[CompoundProduct](_.tenantId)
          .and(_.sku)
          .onDelete(Cascade)
          .build

        xa.run(for
          _      <- ddl.createTable[CompoundProduct](ifNotExists = true)
          _      <- ddl.createTable(inventory)
          _      <- dml.insert(CompoundProduct("tenant1", "SKU-001", "Widget"))
          _      <- dml.insert(CompoundInventory(-1, "tenant1", "SKU-001", 100))
          result <- sql"SELECT * FROM ${Table[CompoundInventory]}".query[CompoundInventory]
        yield result).either
      }.assert {
        case Right(rows) => assertTrue(rows.exists(r => r.productSku == "SKU-001" && r.quantity == 100))
        case Left(err)   => assertTrue(false).label(err.message)
      },
    ),
    section("Multiple Foreign Keys")(
      md"""Chain multiple foreign keys using `.withForeignKey()`:""",
      exampleValue {
        // Multiple foreign keys on one table
        Schema[MultiOrderItem]
          .withForeignKey(_.userId)
          .references[MultiUser](_.id)
          .onDelete(Cascade)
          .withForeignKey(_.productId)
          .references[MultiProduct](_.id)
          .onDelete(Restrict)
          .ddl()
          .sql
      }.assert(sql => assertTrue(sql.contains("userId") && sql.contains("productId"))),
    ),
    section("Compile-Time Column Safety")(
      md"""The foreign key builder extracts column names from selectors at compile time, so
the source and target columns must actually exist on their tables:""",
      exampleValue {
        // This compiles - userId and id are both real columns
        val valid = Schema[TypeOrder]
          .withForeignKey(_.userId)
          .references[TypeUser](_.id)
        valid.ddl().sql
      }.assert(sql => assertTrue(sql.contains("userId"))),
      md"""Referencing a column that doesn't exist on the source table is a compile error.
The snippet below does not compile; the actual compiler diagnostic is shown
beneath it:""",
      expectFail("""
import saferis.*
import saferis.Schema.*

@tableName("type_users")
case class TypeUser(@generated @key id: Int, name: String) derives Table

@tableName("type_orders")
case class TypeOrder(@generated @key id: Int, userId: Int) derives Table

// `nope` is not a field of TypeOrder: compile error.
val invalid = Schema[TypeOrder]
  .withForeignKey(_.nope).references[TypeUser](_.id)
""").assert(errs => assertTrue(errs.nonEmpty)),
    ),
  )
end ForeignKeys
