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
libraryDependencies += "rocks.earlyeffect" %% "saferis" % "<version>"
```

## Database Dialects

Saferis provides compile-time guarantees that operations are only available when your database supports them:

| Feature | PostgreSQL | MySQL | SQLite |
|---------|------------|-------|--------|
| RETURNING clause | Yes | No | Yes |
| JSON operations | Yes | Yes | No |
| Array types | Yes | No | No |
| UPSERT | Yes | No | No |

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
