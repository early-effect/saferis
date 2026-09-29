package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Smoke page: `derives Table` and the SQL interpolator. */
object MacroSmoke extends SaferisDocSpecSuite:

  @tableName("macro_smoke_users")
  case class SmokeUser(@generated @key id: Int, name: String) derives Table

  def doc = page("Macro smoke")(
    md"""Table derivation and the SQL interpolator.""",
    exampleValue {
      val t = Table[SmokeUser]
      sql"SELECT * FROM $t WHERE ${t.name} = ${"Alice"}".sql
    }.assert(s => assertTrue(s.contains("macro_smoke_users") && s.contains("$1"))),
    exampleZIO {
      (for
        _     <- ddl.createTable[SmokeUser](ifNotExists = true)
        _     <- dml.insert(SmokeUser(-1, "Alice"))
        users <- sql"SELECT * FROM ${Table[SmokeUser]}".query[SmokeUser]
      yield users).either
        .provideLayer(DocsTransactor.layer)
    }.assert {
      case Right(users) => assertTrue(users.exists(_.name == "Alice"))
      case Left(err)    => assertTrue(false).label(err.message)
    },
  )
end MacroSmoke
