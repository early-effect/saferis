package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
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
libraryDependencies += "rocks.earlyeffect" %% "saferis" % "0.18.0"
```

Saferis requires ZIO as a provided dependency:

```scala
libraryDependencies += "dev.zio" %% "zio" % "2.1.24"
```"""
    ),
    section("Quick Example")(
      md"""Saferis operations are plain ZIO effects. Throughout these docs the examples are
real `ZIO` programs run against a live PostgreSQL database: `xa` is a `Transactor`
connected to a test container, and `.debug` prints each effect's result:

Define a table with the `Table` typeclass, then create it, insert rows, and query
it, all type-safe, all against a real database:""",
      exampleZIO {
        xa
          .run(for
            _     <- ddl.createTable[QuickUser](ifNotExists = true)
            _     <- dml.insert(QuickUser(-1, "Alice", "alice@example.com"))
            _     <- dml.insert(QuickUser(-1, "Bob", "bob@example.com"))
            users <- sql"SELECT * FROM ${Table[QuickUser]}".query[QuickUser]
          yield users)
          .either
      }.assert {
        case Right(users) => assertTrue(users.exists(_.email == "alice@example.com"))
        case Left(err)    => assertTrue(false).label(err.message)
      },
    ),
    section("Anatomy of an Application")(
      md"""In a real application you provide the `Transactor` as a layer and let your
`ZIOAppDefault` run the program: `xa.run(...)` turns a database program into an
ordinary `ZIO` effect:

```mermaid
flowchart TB
  ds[DataSource] --> cp[ConnectionProvider]
  cp --> xa[Transactor.layer]
  xa --> prog[xa.run ZIO program]
```

```scala
import saferis.*
import zio.*
import javax.sql.DataSource

@tableName("getting_started_app_users")
case class AppUser(@generated @key id: Int, name: String) derives Table

object MyApp extends ZIOAppDefault:
  val program: ZIO[Transactor, SaferisError, Chunk[AppUser]] =
    for
      xa    <- ZIO.service[Transactor]
      users <- xa.run(for
                 _     <- ddl.createTable[AppUser](ifNotExists = true)
                 _     <- dml.insert(AppUser(-1, "Alice"))
                 users <- sql"SELECT * FROM $${Table[AppUser]}".query[AppUser]
               yield users)
    yield users

  // Your DataSource (e.g. a HikariCP pool) becomes a ConnectionProvider,
  // which Transactor.layer turns into a Transactor.
  def dataSource: DataSource = ???
  val connectionProvider = ZLayer.succeed(ConnectionProvider.FromDataSource(dataSource))

  def run = program.provide(connectionProvider, Transactor.layer())
```

Next, read [Core Concepts](core-concepts.html) to understand table definitions, the
`sql"..."` interpolator, and the `Transactor`."""
    ),
  )
end GettingStarted
