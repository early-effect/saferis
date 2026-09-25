package saferis.tests

import saferis.*
import saferis.Schema.*
import saferis.ddl.*
import saferis.dml.*
import saferis.postgres.given
import saferis.tests.DataSourceProvider
import zio.*
import zio.test.*

object ForeignKeyIntegrationSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  // Test tables
  @tableName("fk_int_users")
  final case class User(@generated @key id: Int, name: String, email: String) derives Table

  @tableName("fk_int_products")
  final case class Product(@generated @key id: Int, name: String, price: BigDecimal) derives Table

  @tableName("fk_int_orders")
  final case class Order(@generated @key id: Int, userId: Int, productId: Int, amount: BigDecimal) derives Table

  // For testing SET NULL
  @tableName("fk_int_orders_nullable")
  final case class OrderNullable(@generated @key id: Int, userId: Option[Int], amount: BigDecimal) derives Table

  // For compound FK test
  @tableName("fk_int_order_details")
  final case class OrderDetail(@key orderId: Int, @key lineNum: Int, sku: String) derives Table

  @tableName("fk_int_order_items")
  final case class OrderItem(@generated @key id: Int, orderId: Int, lineNum: Int, price: BigDecimal) derives Table

  val spec = suite("Foreign Key Integration Tests")(
    test("createTable with Instance generates FK constraints") {
      val sql = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .ddl(ifNotExists = false)
        .sql

      assertTrue(
        sql.contains("foreign key (userId) references fk_int_users (id)"),
        sql.contains("foreign key (productId) references fk_int_products (id)"),
      )
    },
    test("createTable with Instance includes ON DELETE actions") {
      val sql = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .onDelete(Cascade)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .onDelete(SetNull)
        .ddl(ifNotExists = false)
        .sql

      assertTrue(
        sql.contains("on delete cascade"),
        sql.contains("on delete set null"),
      )
    },
    test("createTable with Instance includes ON UPDATE actions") {
      val sql = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .onUpdate(Cascade)
        .ddl(ifNotExists = false)
        .sql

      assertTrue(sql.contains("on update cascade"))
    },
    test("createTable with Instance includes constraint names") {
      val sql = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .named(ConstraintName("fk_order_user"))
        .ddl(ifNotExists = false)
        .sql

      assertTrue(sql.contains("constraint fk_order_user foreign key"))
    },
    test("FK constraint prevents insert when parent doesn't exist") {
      val orders = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .build

      for
        // Clean up
        _ <- (dropTable[Order](ifExists = true))
        _ <- (dropTable[User](ifExists = true))
        _ <- (dropTable[Product](ifExists = true))
        // Create parent tables first
        _ <- (createTable[User]())
        _ <- (createTable[Product]())
        // Create child table with FK
        _ <- (createTable(orders))
        // Try to insert order with non-existent user (should fail)
        result <- (sql"insert into fk_int_orders (userId, productId, amount) values (999, 1, 100.00)".insert).either
      yield assertTrue(result.isLeft)
      end for
    },
    test("FK constraint allows insert when parent exists") {
      val orders = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .build

      for
        // Clean up
        _ <- (dropTable[Order](ifExists = true))
        _ <- (dropTable[User](ifExists = true))
        _ <- (dropTable[Product](ifExists = true))
        // Create parent tables first
        _ <- (createTable[User]())
        _ <- (createTable[Product]())
        // Create child table with FK
        _ <- (createTable(orders))
        // Insert parent records
        _ <- (sql"insert into fk_int_users (name, email) values ('Alice', 'alice@test.com')".insert)
        _ <- (sql"insert into fk_int_products (name, price) values ('Widget', 9.99)".insert)
        // Insert order referencing existing parents (should succeed)
        result <- (sql"insert into fk_int_orders (userId, productId, amount) values (1, 1, 100.00)".insert).either
      yield assertTrue(result.isRight)
      end for
    },
    test("ON DELETE CASCADE deletes child rows") {
      val orders = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .onDelete(Cascade)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .build

      for
        // Clean up
        _ <- (dropTable[Order](ifExists = true))
        _ <- (dropTable[User](ifExists = true))
        _ <- (dropTable[Product](ifExists = true))
        // Create tables
        _ <- (createTable[User]())
        _ <- (createTable[Product]())
        _ <- (createTable(orders))
        // Insert parent records
        _ <- (sql"insert into fk_int_users (name, email) values ('Alice', 'alice@test.com')".insert)
        _ <- (sql"insert into fk_int_products (name, price) values ('Widget', 9.99)".insert)
        // Insert order
        _ <- (sql"insert into fk_int_orders (userId, productId, amount) values (1, 1, 100.00)".insert)
        // Verify order exists
        countBefore <- (sql"select count(*) as count from fk_int_orders".queryOne[CountResult])
        // Delete user (should cascade to orders)
        _ <- (sql"delete from fk_int_users where id = 1".delete)
        // Verify order was deleted
        countAfter <- (sql"select count(*) as count from fk_int_orders".queryOne[CountResult])
      yield assertTrue(
        countBefore.map(_.count).contains(1),
        countAfter.map(_.count).contains(0),
      )
      end for
    },
    test("ON DELETE SET NULL sets FK column to null") {
      // For SET NULL, the FK column must be nullable. We create the table with FK via SQL
      // since the type-safe API requires matching types (Option[Int] vs Int)
      for
        // Clean up (drop child tables first due to FK constraints)
        _ <- (dropTable[OrderNullable](ifExists = true))
        _ <- (dropTable[Order](ifExists = true))
        _ <- (dropTable[User](ifExists = true))
        // Create user table
        _ <- (createTable[User]())
        // Create nullable order table with SET NULL FK via SQL
        _ <- (
          sql"""create table fk_int_orders_nullable (
            id serial primary key,
            userId int,
            amount decimal(10,2) not null,
            foreign key (userId) references fk_int_users (id) on delete set null
          )""".dml
        )
        // Insert parent record
        _ <- (sql"insert into fk_int_users (name, email) values ('Bob', 'bob@test.com')".insert)
        // Insert order
        _ <- (sql"insert into fk_int_orders_nullable (userId, amount) values (1, 50.00)".insert)
        // Verify order has userId
        before <- (sql"select userId from fk_int_orders_nullable where id = 1".queryOne[UserIdResult])
        // Delete user (should set userId to null)
        _ <- (sql"delete from fk_int_users where id = 1".delete)
        // Verify userId is now null
        after <- (sql"select userId from fk_int_orders_nullable where id = 1".queryOne[UserIdResult])
      yield assertTrue(
        before.flatMap(_.userId).contains(1),
        after.exists(_.userId.isEmpty),
      )
    },
    test("ON DELETE RESTRICT prevents parent deletion") {
      val orders = Schema[Order]
        .withForeignKey(_.userId)
        .references[User](_.id)
        .onDelete(Restrict)
        .withForeignKey(_.productId)
        .references[Product](_.id)
        .build

      for
        // Clean up (drop all child tables first due to FK constraints)
        _ <- (dropTable[Order](ifExists = true))
        _ <- (dropTable[OrderNullable](ifExists = true))
        _ <- (dropTable[User](ifExists = true))
        _ <- (dropTable[Product](ifExists = true))
        // Create tables
        _ <- (createTable[User]())
        _ <- (createTable[Product]())
        _ <- (createTable(orders))
        // Insert parent records
        _ <- (sql"insert into fk_int_users (name, email) values ('Charlie', 'charlie@test.com')".insert)
        _ <- (sql"insert into fk_int_products (name, price) values ('Gadget', 19.99)".insert)
        // Insert order
        _ <- (sql"insert into fk_int_orders (userId, productId, amount) values (1, 1, 75.00)".insert)
        // Try to delete user (should fail due to RESTRICT)
        result <- (sql"delete from fk_int_users where id = 1".delete).either
      yield assertTrue(result.isLeft)
      end for
    },
    test("compound FK constraint works correctly") {
      val orderItems = Schema[OrderItem]
        .withForeignKey(_.orderId)
        .and(_.lineNum)
        .references[OrderDetail](_.orderId)
        .and(_.lineNum)
        .onDelete(Cascade)
        .build

      for
        // Clean up
        _ <- (dropTable[OrderItem](ifExists = true))
        _ <- (dropTable[OrderDetail](ifExists = true))
        // Create tables
        _ <- (createTable[OrderDetail]())
        _ <- (createTable(orderItems))
        // Insert detail
        _ <- (insert(OrderDetail(1, 1, "SKU-001")))
        // Insert item referencing the detail (should succeed)
        insertResult <-
          (sql"insert into fk_int_order_items (orderId, lineNum, price) values (1, 1, 25.00)".insert).either
        // Try to insert item with non-existent compound key (should fail)
        failResult <-
          (sql"insert into fk_int_order_items (orderId, lineNum, price) values (999, 999, 10.00)".insert).either
      yield assertTrue(
        insertResult.isRight,
        failResult.isLeft,
      )
      end for
    },
  ).provideShared(xaLayer) @@ TestAspect.sequential

  // Helper case classes for query results
  final case class CountResult(count: Long) derives Table
  final case class UserIdResult(userId: Option[Int]) derives Table
end ForeignKeyIntegrationSpecs
