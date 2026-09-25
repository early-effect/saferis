package saferis.tests

import saferis.*
import saferis.Schema.*
import saferis.postgres.PostgresDialect
import zio.json.*
import zio.test.*

/** Comprehensive tests for the Schema DSL API. Tests index building, WHERE conditions, JSON operators, foreign keys,
  * and unique constraints.
  */
object SchemaSpecs extends ZIOSpecDefault:
  // Provide PostgresDialect as Dialect & JsonSupport for JSON operators
  given (Dialect & JsonSupport) = PostgresDialect

  // === Test case classes ===

  @tableName("schema_test_users")
  final case class User(
      @generated @key id: Int,
      name: String,
      email: String,
      age: Int,
      status: String,
      score: Option[BigDecimal],
  ) derives Table

  @tableName("schema_test_products")
  final case class Product(@generated @key id: Int, name: String, price: BigDecimal) derives Table

  @tableName("schema_test_orders")
  final case class Order(@generated @key id: Int, userId: Int, productId: Int, amount: BigDecimal) derives Table

  // Compound key tables
  @tableName("schema_test_order_details")
  final case class OrderDetail(@key orderId: Int, @key lineNum: Int, sku: String, quantity: Int) derives Table

  @tableName("schema_test_order_items")
  final case class OrderItem(@key id: Int, orderId: Int, lineNum: Int, price: BigDecimal) derives Table

  // JSON data types
  final case class UserData(email: String, verified: Boolean) derives JsonCodec
  final case class Metadata(tags: List[String], version: Int) derives JsonCodec

  @tableName("schema_test_profiles")
  final case class Profile(
      @generated @key id: Int,
      name: String,
      data: Json[UserData],
  ) derives Table

  @tableName("schema_test_events")
  final case class Event(
      @generated @key id: Int,
      name: String,
      metadata: Json[Metadata],
  ) derives Table

  val spec = suite("Schema DSL")(
    // === Index Building ===
    suite("Index building")(
      test("single column index") {
        val sql = Schema[User]
          .withIndex(_.name)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("create index"),
          sql.contains("idx_schema_test_users_name"),
          sql.contains("(\"name\")"),
        )
      },
      test("compound index with and()") {
        val sql = Schema[User]
          .withIndex(_.name)
          .and(_.email)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_schema_test_users_name_email"),
          sql.contains("(\"name\", \"email\")"),
        )
      },
      test("three column compound index") {
        val sql = Schema[User]
          .withIndex(_.name)
          .and(_.email)
          .and(_.status)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_schema_test_users_name_email_status"),
          sql.contains("(\"name\", \"email\", \"status\")"),
        )
      },
      test("named index") {
        val sql = Schema[User]
          .withIndex(_.name)
          .named(IndexName("idx_custom_name"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("idx_custom_name"))
      },
      test("unique index") {
        val sql = Schema[User]
          .withUniqueIndex(_.email)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("create unique index"),
          sql.contains("idx_schema_test_users_email"),
        )
      },
      test("multiple indexes on same table") {
        val sql = Schema[User]
          .withIndex(_.name)
          .withIndex(_.email)
          .withUniqueIndex(_.status)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_schema_test_users_name"),
          sql.contains("idx_schema_test_users_email"),
          sql.contains("create unique index"),
        )
      },
    ),
    // === WHERE DSL Basic Operators ===
    suite("WHERE DSL - Basic operators")(
      test("eql generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active'"))
      },
      test("neql generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .neql("deleted")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status <> 'deleted'"))
      },
      test("gt generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .gt(18)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age > 18"))
      },
      test("gte generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .gte(21)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age >= 21"))
      },
      test("lt generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .lt(65)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age < 65"))
      },
      test("lte generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .lte(100)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age <= 100"))
      },
    ),
    // === WHERE DSL NULL Operators ===
    suite("WHERE DSL - NULL operators")(
      test("isNull generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.score)
          .where(_.score)
          .isNull
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where score is null"))
      },
      test("isNotNull generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.score)
          .where(_.score)
          .isNotNull
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where score is not null"))
      },
    ),
    // === WHERE DSL LIKE Operators ===
    suite("WHERE DSL - LIKE operators")(
      test("like generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.name)
          .where(_.name)
          .like("A%")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where name like 'A%'"))
      },
      test("notLike generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.name)
          .where(_.name)
          .notLike("Z%")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where name not like 'Z%'"))
      },
      test("like escapes single quotes") {
        val sql = Schema[User]
          .withIndex(_.name)
          .where(_.name)
          .like("O'Brien%")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where name like 'O''Brien%'"))
      },
    ),
    // === WHERE DSL IN Operators ===
    suite("WHERE DSL - IN operators")(
      test("in generates correct SQL for strings") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .in(Seq("active", "pending"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status in ('active', 'pending')"))
      },
      test("in generates correct SQL for integers") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .in(Seq(18, 21, 25))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age in (18, 21, 25)"))
      },
      test("notIn generates correct SQL") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .notIn(Seq("deleted", "banned"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status not in ('deleted', 'banned')"))
      },
    ),
    // === WHERE DSL BETWEEN Operator ===
    suite("WHERE DSL - BETWEEN operator")(
      test("between generates correct SQL for integers") {
        val sql = Schema[User]
          .withIndex(_.age)
          .where(_.age)
          .between(18, 65)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age between 18 and 65"))
      }
    ),
    // === WHERE DSL Compound Conditions ===
    suite("WHERE DSL - Compound conditions")(
      test("and chains conditions correctly") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .and(_.age)
          .gte(18)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active' and age >= 18"))
      },
      test("or chains conditions correctly") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .or(_.status)
          .eql("pending")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active' or status = 'pending'"))
      },
      test("multiple and conditions") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .and(_.age)
          .gte(18)
          .and(_.score)
          .isNotNull
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active' and age >= 18 and score is not null"))
      },
    ),
    // === WHERE DSL Grouped Conditions ===
    suite("WHERE DSL - Grouped conditions")(
      test("andGroup generates correct SQL with parentheses") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .andGroup(g => g.where(_.age).gte(18).or(_.age).lte(5))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active' and (age >= 18 or age <= 5)"))
      },
      test("orGroup generates correct SQL with parentheses") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.age)
          .lt(18)
          .orGroup(g => g.where(_.status).eql("vip").and(_.age).gte(65))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where age < 18 or (status = 'vip' and age >= 65)"))
      },
      test("grouped conditions with like") {
        val sql = Schema[User]
          .withIndex(_.name)
          .where(_.status)
          .eql("active")
          .andGroup(g => g.where(_.name).like("A%").or(_.name).like("B%"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where status = 'active' and (name like 'A%' or name like 'B%')"))
      },
      test("nested groups with in operator") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .in(Seq("active", "pending"))
          .andGroup(g => g.where(_.age).between(18, 65).or(_.score).isNotNull)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("where status in ('active', 'pending') and (age between 18 and 65 or score is not null)")
        )
      },
    ),
    // === WHERE DSL Named Partial Indexes ===
    suite("WHERE DSL - Named partial indexes")(
      test("named partial index") {
        val sql = Schema[User]
          .withIndex(_.status)
          .where(_.status)
          .eql("active")
          .named(IndexName("idx_active_users"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_active_users"),
          sql.contains("where status = 'active'"),
        )
      },
      test("named unique partial index") {
        val sql = Schema[User]
          .withUniqueIndex(_.email)
          .where(_.status)
          .eql("active")
          .named(IndexName("idx_unique_active_email"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_unique_active_email"),
          sql.contains("create unique index"),
          sql.contains("where status = 'active'"),
        )
      },
    ),
    // === JSON WHERE Operators ===
    suite("JSON WHERE operators")(
      test("jsonHasKey generates correct PostgreSQL SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonHasKey("email")
          .ddl(ifNotExists = false)
          .sql
        // Uses jsonb_exists function to avoid JDBC parameter placeholder conflict with ? operator
        assertTrue(sql.contains("where jsonb_exists(data, 'email')"))
      },
      test("jsonHasAnyKey generates correct PostgreSQL SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonHasAnyKey(Seq("email", "verified"))
          .ddl(ifNotExists = false)
          .sql
        // Uses jsonb_exists_any function to avoid JDBC parameter placeholder conflict with ?| operator
        assertTrue(sql.contains("where jsonb_exists_any(data, array['email', 'verified'])"))
      },
      test("jsonHasAllKeys generates correct PostgreSQL SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonHasAllKeys(Seq("email", "verified"))
          .ddl(ifNotExists = false)
          .sql
        // Uses jsonb_exists_all function to avoid JDBC parameter placeholder conflict with ?& operator
        assertTrue(sql.contains("where jsonb_exists_all(data, array['email', 'verified'])"))
      },
      test("jsonContains generates correct PostgreSQL SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonContains(UserData("test@example.com", true))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("""where data @> '{"email":"test@example.com","verified":true}'"""))
      },
    ),
    // === JSON Path Operators ===
    suite("JSON path operators")(
      test("jsonPath.eql generates correct SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("email")
          .eql("admin@test.com")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'email' = 'admin@test.com'"))
      },
      test("jsonPath.neql generates correct SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("email")
          .neql("spam@test.com")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'email' <> 'spam@test.com'"))
      },
      test("jsonPath.like generates correct SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("email")
          .like("%@example.com")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'email' like '%@example.com'"))
      },
      test("jsonPath.isNull generates correct SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("email")
          .isNull
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'email' is null"))
      },
      test("jsonPath.isNotNull generates correct SQL") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("email")
          .isNotNull
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'email' is not null"))
      },
    ),
    // === JSON Combined with Other Conditions ===
    suite("JSON operators combined with other conditions")(
      test("jsonHasKey with AND condition") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonHasKey("email")
          .and(_.name)
          .eql("Alice")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where jsonb_exists(data, 'email') and name = 'Alice'"))
      },
      test("jsonPath with OR condition") {
        val sql = Schema[Profile]
          .withIndex(_.data)
          .where(_.data)
          .jsonPath("verified")
          .eql("true")
          .or(_.name)
          .like("Admin%")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("where data->>'verified' = 'true' or name like 'Admin%'"))
      },
    ),
    // === MySQL JSON Dialect ===
    suite("MySQL JSON dialect SQL generation")(
      test("MySQLDialect.jsonHasKeySql generates correct SQL") {
        val sql = saferis.mysql.MySQLDialect.jsonHasKeySql(SqlText("data"), "email")
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'one', '$.email')")
      },
      test("MySQLDialect.jsonContainsSql generates correct SQL") {
        val sql = saferis.mysql.MySQLDialect.jsonContainsSql(SqlText("data"), JsonText("""{"verified":true}"""))
        assertTrue(sql == """JSON_CONTAINS(data, '{"verified":true}')""")
      },
      test("MySQLDialect.jsonExtractSql generates correct SQL") {
        val sql = saferis.mysql.MySQLDialect.jsonExtractSql(SqlText("data"), "email")
        assertTrue(sql == "JSON_EXTRACT(data, '$.email')")
      },
      test("MySQLDialect.jsonHasAnyKeySql generates correct SQL") {
        val sql = saferis.mysql.MySQLDialect.jsonHasAnyKeySql(SqlText("data"), Seq("email", "name"))
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'one', '$.email', '$.name')")
      },
      test("MySQLDialect.jsonHasAllKeysSql generates correct SQL") {
        val sql = saferis.mysql.MySQLDialect.jsonHasAllKeysSql(SqlText("data"), Seq("email", "name"))
        assertTrue(sql == "JSON_CONTAINS_PATH(data, 'all', '$.email', '$.name')")
      },
    ),
    // === Foreign Key Building ===
    suite("Foreign key building")(
      test("single column FK generates correct SQL") {
        val sql = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (userId)"),
          sql.contains("references schema_test_users (id)"),
        )
      },
      test("FK with onDelete cascade") {
        val sql = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (userId)"),
          sql.contains("on delete cascade"),
        )
      },
      test("FK with onUpdate cascade") {
        val sql = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onUpdate(Cascade)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("on update cascade"))
      },
      test("FK with named constraint") {
        val sql = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .named(ConstraintName("fk_order_user"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("constraint fk_order_user foreign key"))
      },
      test("compound FK with and()") {
        val sql = Schema[OrderItem]
          .withForeignKey(_.orderId)
          .and(_.lineNum)
          .references[OrderDetail](_.orderId)
          .and(_.lineNum)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (orderId, lineNum)"),
          sql.contains("references schema_test_order_details (orderId, lineNum)"),
        )
      },
      test("compound FK with onDelete and onUpdate") {
        val sql = Schema[OrderItem]
          .withForeignKey(_.orderId)
          .and(_.lineNum)
          .references[OrderDetail](_.orderId)
          .and(_.lineNum)
          .onDelete(Cascade)
          .onUpdate(Cascade)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (orderId, lineNum)"),
          sql.contains("on delete cascade"),
          sql.contains("on update cascade"),
        )
      },
      test("multiple FKs on same table") {
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
          sql.contains("foreign key (userId)"),
          sql.contains("references schema_test_users (id)"),
          sql.contains("foreign key (productId)"),
          sql.contains("references schema_test_products (id)"),
        )
      },
    ),
    // === ForeignKeyAction ===
    suite("ForeignKeyAction")(
      test("toSql returns correct SQL for each action") {
        assertTrue(
          ForeignKeyAction.NoAction.toSql == "no action",
          ForeignKeyAction.Cascade.toSql == "cascade",
          ForeignKeyAction.SetNull.toSql == "set null",
          ForeignKeyAction.SetDefault.toSql == "set default",
          ForeignKeyAction.Restrict.toSql == "restrict",
        )
      }
    ),
    // === Unique Constraints ===
    suite("Unique constraints")(
      test("single column unique constraint") {
        val sql = Schema[User]
          .withUniqueConstraint(_.email)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("unique (email)"))
      },
      test("compound unique constraint with and()") {
        val sql = Schema[User]
          .withUniqueConstraint(_.name)
          .and(_.email)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("unique (name, email)"))
      },
      test("named unique constraint") {
        val sql = Schema[User]
          .withUniqueConstraint(_.email)
          .named(ConstraintName("uq_user_email"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(sql.contains("constraint uq_user_email unique"))
      },
    ),
    // === Schema Build Method ===
    suite("Schema.build method")(
      test("build returns Instance with indexes") {
        val instance = Schema[User]
          .withIndex(_.name)
          .withIndex(_.email)
          .build
        assertTrue(
          instance.indexes.length == 2,
          instance.indexes(0).columns == Seq("name"),
          instance.indexes(1).columns == Seq("email"),
        )
      },
      test("build returns Instance with foreign keys") {
        val instance = Schema[Order]
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .build
        assertTrue(
          instance.foreignKeys.length == 1,
          instance.foreignKeys.head.fromColumns == Seq("userId"),
          instance.foreignKeys.head.toTable == "schema_test_users",
          instance.foreignKeys.head.toColumns == Seq("id"),
          instance.foreignKeys.head.onDelete == ForeignKeyAction.Cascade,
        )
      },
      test("build returns Instance with unique constraints") {
        val instance = Schema[User]
          .withUniqueConstraint(_.email)
          .build
        assertTrue(
          instance.uniqueConstraints.length == 1,
          instance.uniqueConstraints.head.columns == Seq("email"),
        )
      },
      test("build preserves table metadata") {
        val instance = Schema[User]
          .withIndex(_.name)
          .build
        assertTrue(
          instance.tableName == "schema_test_users",
          instance.columns.map(_.name).toSet == Set("id", "name", "email", "age", "status", "score"),
        )
      },
    ),
    // === Complex Schema Composition ===
    suite("Complex schema composition")(
      test("indexes + FKs + unique constraints together") {
        val sql = Schema[Order]
          .withIndex(_.userId)
          .withForeignKey(_.userId)
          .references[User](_.id)
          .onDelete(Cascade)
          .withForeignKey(_.productId)
          .references[Product](_.id)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("create index"),
          sql.contains("idx_schema_test_orders_userId"),
          sql.contains("foreign key (userId)"),
          sql.contains("foreign key (productId)"),
        )
      },
      test("partial index + FK on same column") {
        val sql = Schema[Order]
          .withIndex(_.userId)
          .where(_.amount)
          .gt(BigDecimal(100))
          .withForeignKey(_.userId)
          .references[User](_.id)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("where amount > 100"),
          sql.contains("foreign key (userId)"),
        )
      },
    ),
    // === @label Annotation Support ===
    suite("@label annotation support")(
      test("index uses @label for column name") {
        @tableName("label_test_events")
        final case class LabeledEvent(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent]
          .withIndex(_.instanceId)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_label_test_events_instance_id"),
          sql.contains("(\"instance_id\")"),
          !sql.contains("instanceId"),
        )
      },
      test("compound index uses @label for column names") {
        @tableName("label_test_events2")
        final case class LabeledEvent2(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent2]
          .withIndex(_.instanceId)
          .and(_.sequenceNr)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("idx_label_test_events2_instance_id_sequence_nr"),
          sql.contains("(\"instance_id\", \"sequence_nr\")"),
          !sql.contains("instanceId"),
          !sql.contains("sequenceNr"),
        )
      },
      test("partial index WHERE uses @label for column name") {
        @tableName("label_test_events3")
        final case class LabeledEvent3(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent3]
          .withIndex(_.sequenceNr)
          .where(_.instanceId)
          .eql("test")
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("where instance_id = 'test'"),
          !sql.contains("instanceId"),
        )
      },
      test("unique constraint uses @label for column name") {
        @tableName("label_test_events4")
        final case class LabeledEvent4(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent4]
          .withUniqueConstraint(_.instanceId)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("unique (instance_id)"),
          !sql.contains("instanceId"),
        )
      },
      test("compound unique constraint uses @label for column names") {
        @tableName("label_test_events5")
        final case class LabeledEvent5(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent5]
          .withUniqueConstraint(_.instanceId)
          .and(_.sequenceNr)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("unique (instance_id, sequence_nr)"),
          !sql.contains("instanceId"),
          !sql.contains("sequenceNr"),
        )
      },
      test("foreign key uses @label for source column name") {
        @tableName("label_test_refs")
        final case class LabeledRef(
            @generated @key id: Int,
            @label("user_ref_id") userRefId: Int,
        ) derives Table

        val sql = Schema[LabeledRef]
          .withForeignKey(_.userRefId)
          .references[User](_.id)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (user_ref_id)"),
          !sql.contains("userRefId"),
        )
      },
      test("foreign key uses @label for target column name") {
        @tableName("label_test_target")
        final case class LabeledTarget(
            @generated @key @label("target_id") targetId: Int,
            name: String,
        ) derives Table

        @tableName("label_test_source")
        final case class LabeledSource(
            @generated @key id: Int,
            @label("target_ref") targetRef: Int,
        ) derives Table

        val sql = Schema[LabeledSource]
          .withForeignKey(_.targetRef)
          .references[LabeledTarget](_.targetId)
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("foreign key (target_ref)"),
          sql.contains("references label_test_target (target_id)"),
          !sql.contains("targetRef"),
          !sql.contains("targetId"),
        )
      },
      test("grouped WHERE condition uses @label") {
        @tableName("label_test_events6")
        final case class LabeledEvent6(
            @generated @key id: Int,
            @label("instance_id") instanceId: String,
            @label("sequence_nr") sequenceNr: Long,
        ) derives Table

        val sql = Schema[LabeledEvent6]
          .withIndex(_.instanceId)
          .where(_.sequenceNr)
          .gt(0L)
          .andGroup(g => g.where(_.instanceId).eql("a").or(_.instanceId).eql("b"))
          .ddl(ifNotExists = false)
          .sql
        assertTrue(
          sql.contains("where sequence_nr > 0 and (instance_id = 'a' or instance_id = 'b')"),
          !sql.contains("instanceId"),
          !sql.contains("sequenceNr"),
        )
      },
    ),
  )
end SchemaSpecs
