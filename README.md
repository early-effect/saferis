# saferis

[![Scala CI](https://github.com/russwyte/saferis/actions/workflows/scala.yml/badge.svg)](https://github.com/russwyte/saferis/actions/workflows/scala.yml)
[![Maven Repository](https://img.shields.io/maven-central/v/rocks.earlyeffect/saferis_3?logo=apachemaven)](https://mvnrepository.com/artifact/rocks.earlyeffect/saferis)

*The name is derived from 'safe' and 'eris' (Greek for 'strife' or 'discord')*

**Saferis mitigates the discord of unsafe SQL.** A type-safe, resource-safe SQL client library for Scala 3 and ZIO.

## Key Features

- **SQL Injection Protection** - Safe SQL interpolator with compile-time validation
- **Multi-Database Support** - Works with PostgreSQL, MySQL, SQLite, and any JDBC database
- **Type-Safe Capabilities** - Operations only available when your database supports them
- **Resource Safety** - Guaranteed connection and transaction management with ZIO
- **Label-Based Decoding** - Column-to-field mapping by name, not position
- **Compile-Time Validation** - Table schemas, column names, and SQL verified at compile time
- **Unified Query Builder** - Type-safe joins, subqueries, and pagination in one fluent API
- **Streaming** - `queryStream` for real-time row iteration; `pagedStream`/`seekingStream` for cursor-based pagination with connection release between pages and checkpoint support for resumable processing

## Installation

Add to your `build.sbt`:

```scala
// JVM. Add saferis and the one adapter for your database. Each adapter brings
// saferis-jdbc (java.sql only) and its own JDBC driver.
libraryDependencies ++= Seq(
  "rocks.earlyeffect" %% "saferis" % "<version>",
  "rocks.earlyeffect" %% "saferis-postgres-jdbc" % "<version>", // or one of the adapters below
)
```

| Database | Adapter module | Layer | Dialect import |
|----------|----------------|-------|----------------|
| PostgreSQL | `saferis-postgres-jdbc` (pgjdbc) | `PostgresJdbc.layer()` | default |
| MySQL | `saferis-mysql-jdbc` (Connector/J) | `MySqlJdbc.layer()` | `import saferis.mysql.given` |
| SQLite | `saferis-sqlite-jdbc` (sqlite-jdbc) | `SqliteJdbc.layer()` | `import saferis.sqlite.given` |
| H2, for fast in-memory tests | `saferis-h2-jdbc` | `H2Jdbc.memory("db") >>> H2Jdbc.layer()` | `import saferis.h2.given` |

Each layer needs a `javax.sql.DataSource` (your pool) and provides a `SqlSession`.

### Bring your own database

A database Saferis does not ship needs a `Dialect` for its SQL and a `JdbcAdapter` for its driver. `StandardJdbcAdapter` covers what `java.sql` specifies, so an adapter overrides only what its driver does differently:

```scala
object MyDialect extends Dialect:
  val name = "MyDb"
  def columnType(tpe: SqlType): String = ...
  def autoIncrementClause(isGenerated: Boolean, isPrimaryKey: Boolean, hasCompoundKey: Boolean): String = ...

object MyAdapter extends StandardJdbcAdapter:
  // Map vendor error codes to the SQLSTATEs Saferis classifies (23505 unique, 40P01 deadlock, ...).
  override def serverError(e: SQLException): ServerError = ...

val session = JdbcSession.layer(MyAdapter)
```

Every shipped database runs one conformance suite by providing its `SqlSession` and a description of itself as layers, and a database brought from outside the library runs the same suite the same way.

Node (Scala.js) uses `saferis-postgres-node`. Compile does not download `pg`. Install the same versions this repository links against, or `require` fails when the bundle loads:

```scala
libraryDependencies ++= Seq(
  "rocks.earlyeffect" %%% "saferis" % "<version>",
  "rocks.earlyeffect" %%% "saferis-postgres-node" % "<version>",
)
```

```bash
npm install pg@8.16.3 pg-cursor@2.22.0
```

## Database Dialects

Saferis provides compile-time guarantees that operations are only available when your database supports them:

| Feature | PostgreSQL | MySQL | SQLite |
|---------|------------|-------|--------|
| RETURNING clause | Yes | No | Yes |
| JSON operations | Yes | Yes | No |
| Array types | Yes | No | No |
| UPSERT | Yes | No | No |
| `Schema.verify` | Yes | Yes | No |

The query builder's `.in` / `.inList` binds one array parameter on PostgreSQL and one parameter per value elsewhere. In a raw `sql` string, `in(...)` works on every database.

Switch databases by changing one import - your code adapts automatically:

```scala
import saferis.*                 // PostgreSQL (default)
import saferis.mysql.{given}     // Override with MySQL
import saferis.sqlite.{given}    // Override with SQLite
```

See the [full documentation](https://www.earlyeffect.rocks/saferis/) for:
- Complete API reference
- Running examples with real database output
- Dialect system details
- DDL and DML operations
- Advanced pagination patterns

The documentation is built with [Specular](https://github.com/early-effect/specular): every example is a DocSpec that asserts under zio-test and runs against a live PostgreSQL database, so the output shown is real and the build fails if an example breaks.

## License

[Apache 2.0](LICENSE)

## Development

```bash
./scripts/install-git-hooks  # once per clone: pre-commit runs scalafmtCheckAll
```
