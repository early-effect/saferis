package saferis.docs

import saferis.*
import saferis.Schema.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object CoreConcepts extends SaferisDocSpecSuite:

  @tableName("core_concepts_products")
  case class Product(
      @generated @key id: Long,
      name: String,
      sku: String,
      price: Double,
      inStock: Boolean = true,
      description: Option[String],
  ) derives Table

  @tableName("core_concepts_property_products")
  case class PropertyProduct(
      @generated @key id: Long,
      @label("product_name") name: String,
      quantity: Int = 0,
      price: Double,
      notes: Option[String],
  ) derives Table

  @tableName("core_concepts_order_items")
  case class OrderItem(
      @key orderId: Long,
      @key productId: Long,
      quantity: Int,
  ) derives Table

  val products = Table[Product]

  def doc = page("Core Concepts")(
    section("Table Definitions")(
      md"""Define tables using case classes with the `Table` typeclass:

```scala
import saferis.*

@tableName("core_concepts_products")
case class Product(
  @generated @key id: Long,      // Auto-generated primary key
  name: String,
  sku: String,
  price: Double,
  inStock: Boolean = true,        // Has default value
  description: Option[String]     // Nullable
) derives Table
```

### Annotations

| Annotation | Purpose |
|------------|---------|
| `@tableName("name")` | Specifies the SQL table name |
| `@key` | Marks a primary key column |
| `@generated` | Marks an auto-generated column (identity/auto-increment) |
| `@label("column_name")` | Maps field to a different column name |

For indexes, unique constraints, and foreign keys, use the [Schema DSL](ddl.html#schema-dsl-for-indexes-and-constraints).

### Automatic Column Properties

Saferis infers column properties from your Scala types:

| Scala Type | SQL Property |
|------------|--------------|
| `T` (non-Option) | `NOT NULL` |
| `Option[T]` | Nullable |
| Field with default value | `DEFAULT <value>` |""",
      exampleValue {
        Schema[PropertyProduct].ddl().sql
      }.assert(sql => assertTrue(sql.contains("product_name") && sql.contains("default"))),
      md"""### Compound Primary Keys

Use multiple `@key` annotations to create a composite primary key:""",
      exampleValue {
        Schema[OrderItem].ddl().sql
      }.assert(sql => assertTrue(sql.contains("primary key") && sql.contains("orderId"))),
      exampleZIO {
        (for
          _     <- ddl.createTable[OrderItem](ifNotExists = true)
          _     <- dml.insert(OrderItem(1, 100, 2))
          _     <- dml.insert(OrderItem(1, 101, 1))
          _     <- dml.insert(OrderItem(2, 100, 3))
          items <- sql"SELECT * FROM ${Table[OrderItem]}".query[OrderItem]
        yield items).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(items) => assertTrue(items.exists(_.orderId == 1L) && items.exists(_.productId == 101L))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("SQL Interpolation")(
      md"""The `sql"..."` interpolator is Saferis's primary defense against SQL injection. It automatically distinguishes between different types of interpolated values:""",
      exampleValue {
        val minPrice = 10.0
        sql"SELECT * FROM $products WHERE ${products.price} > $minPrice".sql
      }.assert(sql => assertTrue(sql.contains("$1"))),
      exampleValue {
        sql"SELECT ${products.name}, ${products.price} FROM $products WHERE ${products.inStock} = ${true}".sql
      }.assert(sql => assertTrue(sql.contains("core_concepts_products") && sql.contains("$1"))),
      md"""The interpolator handles each type differently:

| Interpolated Type | Treatment | Example |
|-------------------|-----------|---------|
| Table instance | SQL identifier | `$$products` → `products` |
| Column reference | SQL identifier | `$${products.name}` → `name` |
| Scalar values | Parameter | `$$minPrice` → `$$1` with a bound value |
| `SqlFragment` | Embedded SQL | Nested fragments are spliced in |

`fragment.sql` is the Postgres inspection form (`$$1`, `$$2`). It is not the text a driver sends.
See [SQL Injection Prevention](sql-injection-prevention.html) for the complete security model.""",
    ),
    section("The session")(
      md"""`SqlSession` executes statements. On the JVM, `PostgresJdbc.layer` builds one from a `DataSource`. `JdbcSession.layer` is the same session with a `JdbcAdapter` you supply:

```scala
import saferis.*
import saferis.postgres.jdbc.PostgresJdbc
import zio.*
import javax.sql.DataSource

val dataSource: DataSource = ???

@tableName("core_concepts_users")
case class User(@generated @key id: Int, name: String) derives Table

val session = ZLayer.succeed(dataSource) >>> PostgresJdbc.layer()

val result: ZIO[SqlSession, SaferisError, Chunk[User]] =
  sql"SELECT * FROM $${Table[User]}".query[User]
```

`JdbcSessionConfig` carries the session-wide statement timeout, a JDBC `configure` callback, and a vendor retry hook. Statement observation is `SqlListener.observe`, not a field on the driver config. A connection pool already queues callers. The session does not add a second semaphore.

Open a transaction with `transact`. Nested `transact` joins the outer transaction: one commit, one rollback. See [Statement Timeouts](statement-timeouts.html) for `defaultTimeout`.

Once `COMMIT` is sent, the session waits for the answer and an interrupt does not cut it off, because an abandoned commit leaves you not knowing whether it happened. The statement timeout does not cover `COMMIT`, so the bound on that wait is the socket. On Node the pool turns on TCP keepalive. On JDBC set your driver's socket timeout on the `DataSource` (pgjdbc `socketTimeout`, Connector/J `socketTimeout`) so a peer that vanished without a reset fails the commit instead of hanging it."""
    ),
  )
end CoreConcepts
