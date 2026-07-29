package saferis.docs

import saferis.*
import saferis.Schema.*
import saferis.docs.DocsTransactor.xa
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
        xa
          .run(for
            _     <- ddl.createTable[OrderItem](ifNotExists = true)
            _     <- dml.insert(OrderItem(1, 100, 2))
            _     <- dml.insert(OrderItem(1, 101, 1))
            _     <- dml.insert(OrderItem(2, 100, 3))
            items <- sql"SELECT * FROM ${Table[OrderItem]}".query[OrderItem]
          yield items)
          .either
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
      }.assert(sql => assertTrue(sql.contains("?"))),
      exampleValue {
        sql"SELECT ${products.name}, ${products.price} FROM $products WHERE ${products.inStock} = ${true}".sql
      }.assert(sql => assertTrue(sql.contains("core_concepts_products") && sql.contains("?"))),
      md"""The interpolator handles each type differently:

| Interpolated Type | Treatment | Example |
|-------------------|-----------|---------|
| Table instance | SQL identifier | `$$products` → `products` |
| Column reference | SQL identifier | `$${products.name}` → `name` |
| Scalar values | Prepared statement `?` | `$$minPrice` → `?` with bound value |
| `SqlFragment` | Embedded SQL | Nested fragments are composed |

See [SQL Injection Prevention](sql-injection-prevention.html) for the complete security model.""",
    ),
    section("The Transactor")(
      md"""The `Transactor` wraps a `ConnectionProvider` and executes SQL operations:

```scala
import saferis.*
import zio.*
import javax.sql.DataSource

// Assuming you have a DataSource
val dataSource: DataSource = ???

@tableName("core_concepts_users")
case class User(@generated @key id: Int, name: String) derives Table

// From a ConnectionProvider
val provider = ConnectionProvider.FromDataSource(dataSource)
val xa = Transactor(provider, _ => (), None)

// Execute operations
val result = xa.run(
  sql"SELECT * FROM $${Table[User]}".query[User]
)
```

### Concurrency Limiting

The `Transactor.layer` method accepts an optional `maxConcurrency` parameter that limits concurrent database operations using a ZIO Semaphore:

```scala
import saferis.*

// Default: no concurrency limit (recommended for connection pools)
val defaultLayer = Transactor.layer()

// With concurrency limit (for SQLite or direct JDBC without pooling)
val limitedLayer = Transactor.layer(maxConcurrency = 1L)
```

`Transactor.layer` also accepts an optional `defaultTimeout` that applies a JDBC statement timeout to every query run through the Transactor, see [Statement Timeouts](statement-timeouts.html).

**When to use `maxConcurrency`:**
- SQLite or other embedded databases without connection pooling
- Direct JDBC connections without a pool
- When you need concurrency limits below pool size for backpressure

**When NOT to use `maxConcurrency`:**
- With HikariCP or similar connection pools. The pool handles queuing more efficiently and HikariCP specifically recommends letting threads wait on the pool rather than limiting concurrency externally. Using a semaphore with a pool creates double-queuing and adds overhead in high-contention scenarios.""",
    ),
  )
end CoreConcepts
