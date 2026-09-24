package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object GettingStarted extends SaferisDocSpecSuite:

  @tableName("getting_started_quick_users")
  case class QuickUser(@generated @key id: Int, name: String, email: String) derives Table

  def doc = page("Getting Started")(
    section("Installation")(
      md"""Add Saferis to your `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "rocks.earlyeffect" %% "saferis" % "0.18.0",
  "rocks.earlyeffect" %% "saferis-postgres-jdbc" % "0.18.0",
)
```

Add `saferis` and the one adapter for your database. `saferis-jdbc` is `java.sql` only, and each adapter supplies what its database does differently (bind, read, server errors, cursors) and brings its own JDBC driver:

| Database | Adapter module | Layer |
|----------|----------------|-------|
| PostgreSQL | `saferis-postgres-jdbc` (pgjdbc) | `PostgresJdbc.layer()` |
| MySQL | `saferis-mysql-jdbc` (Connector/J) | `MySqlJdbc.layer()` with `import saferis.mysql.given` |
| SQLite | `saferis-sqlite-jdbc` (sqlite-jdbc) | `SqliteJdbc.layer()` with `import saferis.sqlite.given` |
| H2, for fast in-memory tests | `saferis-h2-jdbc` | `H2Jdbc.layer()` with `import saferis.h2.given` |

A database Saferis does not ship needs a `Dialect` and a `JdbcAdapter`. Extend `StandardJdbcAdapter`, override what your driver does differently, and pass it to `JdbcSession.layer`.

A Node application depends on the Scala.js artifacts and on npm packages. The versions below are the ones this repository tests. A missing `require` fails when the bundle loads.

```scala
libraryDependencies ++= Seq(
  "rocks.earlyeffect" %%% "saferis" % "0.18.0",
  "rocks.earlyeffect" %%% "saferis-postgres-node" % "0.18.0",
)
```

```bash
npm install pg@8.16.3 pg-cursor@2.22.0
```

Saferis requires ZIO as a provided dependency:

```scala
libraryDependencies += "dev.zio" %% "zio" % "2.1.24"
```"""
    ),
    section("Quick Example")(
      md"""Saferis operations are plain ZIO effects. Throughout these docs the examples are
real `ZIO` programs run against a live PostgreSQL database. The suite provides an
`SqlSession` connected to a test container.

Define a table with the `Table` typeclass, then create it, insert rows, and query
it, all type-safe, all against a real database:""",
      exampleZIO {
        (for
          _     <- ddl.createTable[QuickUser](ifNotExists = true)
          _     <- dml.insert(QuickUser(-1, "Alice", "alice@example.com"))
          _     <- dml.insert(QuickUser(-1, "Bob", "bob@example.com"))
          users <- sql"SELECT * FROM ${Table[QuickUser]}".query[QuickUser]
        yield users).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(users) => assertTrue(users.exists(_.email == "alice@example.com"))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("Anatomy of an Application")(
      md"""In a real application you provide `SqlSession` as a layer and let your
`ZIOAppDefault` run the program. Fragments read the session from the environment:

```mermaid
flowchart TB
  ds[DataSource] --> session[PostgresJdbc.layer]
  session --> prog[ZIO program]
```

```scala
import saferis.*
import saferis.postgres.jdbc.PostgresJdbc
import zio.*
import javax.sql.DataSource

@tableName("getting_started_app_users")
case class AppUser(@generated @key id: Int, name: String) derives Table

object MyApp extends ZIOAppDefault:
  val program: ZIO[SqlSession, SaferisError, Chunk[AppUser]] =
    for
      _     <- ddl.createTable[AppUser](ifNotExists = true)
      _     <- dml.insert(AppUser(-1, "Alice"))
      users <- sql"SELECT * FROM $${Table[AppUser]}".query[AppUser]
    yield users

  // A JDBC pool (for example HikariCP) is the DataSource.
  // PostgresJdbc.layer is the Postgres JdbcAdapter on that pool.
  def dataSource: DataSource = ???

  def run = program.provide(ZLayer.succeed(dataSource) >>> PostgresJdbc.layer())
```

Next, read [Core Concepts](core-concepts.html) to understand table definitions, the
`sql"..."` interpolator, and `SqlSession`."""
    ),
  )
end GettingStarted
