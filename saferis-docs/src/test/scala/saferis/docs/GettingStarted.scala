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
libraryDependencies += "rocks.earlyeffect" %% "saferis" % "0.18.0"
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
  ds[DataSource] --> session[JdbcSession.layer]
  session --> prog[ZIO program]
```

```scala
import saferis.*
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
  // JdbcSession.layer turns it into an SqlSession.
  def dataSource: DataSource = ???

  def run = program.provide(ZLayer.succeed(dataSource) >>> JdbcSession.layer())
```

Next, read [Core Concepts](core-concepts.html) to understand table definitions, the
`sql"..."` interpolator, and `SqlSession`."""
    ),
  )
end GettingStarted
