package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Smoke page: prove 3.8 docs can `derives Table` against 3.3-built core macros. */
object MacroSmoke extends SaferisDocSpecSuite:

  @tableName("macro_smoke_users")
  case class SmokeUser(@generated @key id: Int, name: String) derives Table

  def doc = page("Macro smoke")(
    md"""Docs-only Scala **3.8.4** against LTS **3.3.8** core: table derivation and SQL interpolator.""",
    exampleValue {
      val t = Table[SmokeUser]
      sql"SELECT * FROM $t WHERE ${t.name} = ${"Alice"}".sql
    }.assert(s => assertTrue(s.contains("macro_smoke_users") && s.contains("?"))),
    exampleZIO {
      DocsTransactor.xa
        .run(for
          _     <- ddl.createTable[SmokeUser](ifNotExists = true)
          _     <- dml.insert(SmokeUser(-1, "Alice"))
          users <- sql"SELECT * FROM ${Table[SmokeUser]}".query[SmokeUser]
        yield users)
        .either
    }.assert {
      case Right(users) => assertTrue(users.exists(_.name == "Alice"))
      case Left(err)    => assertTrue(false).label(err.message)
    },
  )
end MacroSmoke
