package saferis.docs

import mermoid.Mermaid
import mermoid.ascent.MermoidAscent
import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** The site front. Specular still emits a summary index; [[BuildSite]] copies this page over it. */
object Front extends DocSpecSuite:

  /** Release coordinate. The site JVM passes `specular.meta.displayVersion` when that value is installable. */
  private object Install:
    /** Last published tag. Used when the build version is a snapshot or a dynver distance. */
    private val published = "0.20.0"

    val version: String =
      released(sys.props.get("specular.meta.displayVersion"))
        .orElse(released(sys.props.get("specular.meta.version")))
        .getOrElse(published)

    val coordinate: String =
      s"""libraryDependencies ++= Seq(
         |  "rocks.earlyeffect" %% "saferis" % "$version",
         |  "rocks.earlyeffect" %% "saferis-postgres-jdbc" % "$version",
         |)""".stripMargin

    private def released(raw: Option[String]): Option[String] =
      raw.map(_.trim).filter(_.nonEmpty).filter(isRelease)

    private def isRelease(raw: String): Boolean =
      !raw.contains("SNAPSHOT") && !raw.contains("-ci") && !raw.contains("+")
  end Install

  private val orders =
    Mermaid("""erDiagram
              |    fk_users ||--o{ fk_orders : places
              |    fk_users {
              |        int id PK
              |        string name
              |    }
              |    fk_orders {
              |        int id PK
              |        int userId FK
              |        decimal amount
              |    }
              |""".stripMargin)

  private val postgresDialect =
    Mermaid("""classDiagram
              |    direction TB
              |    class Dialect
              |    class PostgresDialect
              |    Dialect <|-- PostgresDialect
              |""".stripMargin)

  private val postgresReturning =
    Mermaid("""classDiagram
              |    direction TB
              |    class ReturningSupport <<trait>>
              |    class PostgresDialect
              |    ReturningSupport <|-- PostgresDialect
              |""".stripMargin)

  private val mysqlCap =
    Mermaid("""classDiagram
              |    direction TB
              |    class Dialect
              |    class MySQLDialect
              |    Dialect <|-- MySQLDialect
              |""".stripMargin)

  private val statements =
    Mermaid("""classDiagram
              |    direction TB
              |    class ReturningQuery~A~
              |    class UpdateReady~A~ {
              |        +build() SqlFragment
              |        +returningAs() ReturningQuery
              |    }
              |    UpdateReady --> ReturningQuery : returningAs
              |""".stripMargin)

  /** Phone column is about 390px. A figure wider than that scrolls. */
  private def fits(text: String): Boolean =
    svgWidth(text).exists(_ <= 400)

  private def svgWidth(text: String): Option[Double] =
    val attrs = List(
      """width,Str\(([0-9.]+)\)""".r,
      """width="([0-9.]+)"""".r,
      """data-mermoid-width="([0-9.]+)"""".r,
      """data-mermoid-width,Str\(([0-9.]+)\)""".r,
    )
    attrs.flatMap(_.findAllMatchIn(text)).flatMap(m => m.group(1).toDoubleOption).maxOption

  def doc = page("Injection is a type error")(
    md"""A query parameter is not SQL text. Concatenating a value into the statement is rejected by the compiler, not caught by a sanitizer.""",
    section("The schema")(
      md"""
Getting Started creates `getting_started_quick_users` (`id`, `name`, `email`). Foreign Key Support creates `fk_users` and `fk_orders`: one user, and zero or more orders whose `userId` points at that user. DDL's `ddl_customers` is the same kind of row. The figure is the foreign-key pair those pages build.
""",
      illustration(MermoidAscent.svgDiagram(orders)).assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("fk_users"), text.contains("fk_orders"), text.contains("userId"), fits(text))
          .label(s"schema width ${svgWidth(text)}")
      },
    ),
    section("RETURNING")(
      md"""
There is no statement type that has RETURNING and a second type that lacks the method. `UpdateReady.build` returns a `SqlFragment` with no RETURNING clause. `UpdateReady.returningAs` returns a `ReturningQuery`. Both methods are on that type. `returningAs` asks for `Dialect & ReturningSupport`. `PostgresDialect` is a `Dialect` and a `ReturningSupport`. `MySQLDialect` is only a `Dialect`. Under `import saferis.mysql.given`, `summon[Dialect]` is `MySQLDialect`, and the compiler refuses to treat it as `ReturningSupport`. `returningAs` still compiles in that file: `Dialect.defaultDialect` is a `PostgresDialect` given, and the intersection search finds it.
""",
      illustration(MermoidAscent.svgDiagram(postgresDialect)).assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("PostgresDialect"), text.contains("Dialect"), fits(text))
          .label(s"postgres dialect width ${svgWidth(text)}")
      },
      illustration(MermoidAscent.svgDiagram(postgresReturning)).assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("ReturningSupport"), text.contains("PostgresDialect"), fits(text))
          .label(s"returning width ${svgWidth(text)}")
      },
      illustration(MermoidAscent.svgDiagram(mysqlCap)).assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("MySQLDialect"), text.contains("Dialect"), fits(text))
          .label(s"mysql width ${svgWidth(text)}")
      },
      illustration(MermoidAscent.svgDiagram(statements)).assert { ui =>
        val text = ui.toString
        assertTrue(text.contains("UpdateReady"), text.contains("ReturningQuery"), text.contains("build"), fits(text))
          .label(s"statement width ${svgWidth(text)}")
      },
      expectFail("""import saferis.*
import saferis.mysql.given

val refused: Dialect & ReturningSupport = summon[Dialect]
""").assert { errors =>
        assertTrue(errors.exists(_.message.contains("ReturningSupport")))
      },
    ),
    section("Install")(
      md"""The coordinate is the release this site advertises.

```scala
${Install.coordinate}
```
"""
    ),
    section("sql")(
      md"""
`sql` is an inline extension on `StringContext`. Naming it in a cite expands the macro, and a call is not a definition, so the panel is `SqlFragment.interpolate`: the method that expansion calls, with the literal parts and the placeholders kept apart. The footer is that definition.
""",
      cite(SqlFragment.interpolate(_, _)),
    ),
  )

  override def spec =
    suite("Injection is a type error")(
      super.spec,
      test("install coordinate is a release") {
        val version = Install.version
        val line    = Install.coordinate
        assertTrue(
          line.contains(version),
          line.contains("rocks.earlyeffect"),
          line.contains("%% \"saferis\""),
          line.contains("saferis-postgres-jdbc"),
          !version.contains("-ci"),
          !version.contains("SNAPSHOT"),
          !line.contains("-ci"),
          !line.contains("SNAPSHOT"),
        )
      },
    )
end Front
