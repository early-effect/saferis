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
| CTEs | Yes | Yes | Yes | Yes |"""
    ),
    section("Type Mappings")(
      md"""| SqlType | PostgreSQL | MySQL | SQLite |
|--------|------------|-------|--------|
| VarChar | varchar(255) | varchar(255) | text |
| Integer | integer | int | integer |
| BigInt | bigint | bigint | integer |
| Double | double precision | double | real |
| Bool | boolean | boolean | integer |
| Timestamp | timestamp | timestamp | text |
| Text | text | longtext | text |
| Binary | bytea | blob | blob |
| Uuid | uuid | char(36) | text |"""
    ),
    section("Auto-Increment Syntax")(
      md"""| Database | Syntax |
|----------|--------|
| PostgreSQL | `GENERATED ALWAYS AS IDENTITY` |
| MySQL | `AUTO_INCREMENT` |
| SQLite | `AUTOINCREMENT` |

For a deeper look at how capabilities are enforced at compile time, see
[Type-Safe Capabilities](capabilities.html)."""
    ),
  )
end DialectSystem
