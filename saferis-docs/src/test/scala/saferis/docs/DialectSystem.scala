package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object DialectSystem extends SaferisDocSpecSuite:

  def doc = page("Dialect System")(
    md"""Saferis supports multiple databases with compile-time type safety. Each dialect provides database-specific SQL generation and type mappings.""",
    section("Available Dialects")(
      md"""```scala
import saferis.*

// PostgreSQL (default) - full feature support
// No additional import needed - it's the default

// MySQL:    import saferis.mysql.given
// SQLite:   import saferis.sqlite.given
// Spark SQL: import saferis.spark.given

// Switching dialect is a one-line import change; your query code adapts.
val pg = summon[Dialect]
```""",
      exampleValue {
        summon[Dialect].getClass.getSimpleName
      }.assert(name => assertTrue(name.nonEmpty)),
      md"""A `Dialect` takes typed names and returns `SqlText` (`ColumnType` for type spellings), so a dialect you write outside Saferis says what each string is. See [Core Concepts](core-concepts.html). Spark's `dropTableSql` and `truncateTableSql` quote the table name with backticks, as every other dialect does; they used to splice it unquoted.""",
    ),
    section("Feature Comparison")(
      md"""| Feature | PostgreSQL | MySQL | SQLite | Spark |
|---------|------------|-------|--------|-------|
| RETURNING clause | Yes | No | Yes | No |
| JSON operations | Yes | Yes | No | No |
| Array types | Yes | No | No | No |
| UPSERT | Yes | No | No | No |
| IF NOT EXISTS (indexes) | Yes | No | Yes | Yes |
| Window functions | Yes | Yes | Yes | Yes |
| CTEs | Yes | Yes | Yes | Yes |
| `Schema.verify` | Yes | Yes | Yes | No |
| Adapter module | `saferis-postgres-jdbc`, `saferis-postgres-node` | `saferis-mysql-jdbc` | `saferis-sqlite-jdbc` | None: SQL only |

`H2Dialect` ships in `saferis-h2-jdbc` rather than in core. It claims none of the optional capabilities. See [Databases](databases.html) for what each adapter does."""
    ),
    section("Type Mappings")(
      md"""The DDL each dialect renders for a column. SQLite stores by affinity but keeps the declared name, and its adapter reads that name back, so SQLite declares a name for every type.

| SqlType | PostgreSQL | MySQL | SQLite | H2 |
|---------|------------|-------|--------|----|
| Bool | boolean | boolean | boolean | boolean |
| Int2 | smallint | smallint | integer | smallint |
| Int4 | integer | int | integer | integer |
| Int8 | bigint | bigint | integer | bigint |
| Float4 | real | float | real | real |
| Float8 | double precision | double | double | double precision |
| Numeric | numeric | decimal(65, 30) | numeric | numeric(1000, 100) |
| VarChar | varchar(255) | varchar(255) | varchar(255) | varchar(255) |
| Text | text | longtext | text | character large object |
| Bytea | bytea | blob | blob | varbinary |
| Date | date | date | date | date |
| Time | time | time(6) | time | time(6) |
| Timestamp | timestamp | datetime(6) | timestamp | timestamp(6) |
| Timestamptz | timestamptz | timestamp(6) | timestamptz | timestamp(6) with time zone |
| Jsonb | jsonb | json | json | json |
| Uuid | uuid | char(36) | uuid | uuid |
| Array(element) | element[] | json | text | element array |

MySQL's bare `decimal` is `decimal(10,0)` and drops the fraction, so `Numeric` asks for the widest scale. MySQL's `datetime` is a local timestamp and `timestamp` an instant, so the two Scala types get different columns. Every SQLite integer is `integer`, so an integer primary key stays SQLite's rowid and autoincrements."""
    ),
    section("Auto-Increment Syntax")(
      md"""| Database | Syntax |
|----------|--------|
| PostgreSQL | `GENERATED ALWAYS AS IDENTITY` |
| MySQL | `AUTO_INCREMENT` |
| SQLite | `AUTOINCREMENT` |

For a deeper look at how capabilities are enforced at compile time, see
[Type-Safe Capabilities](type-safe-capabilities.html)."""
    ),
  )
end DialectSystem
