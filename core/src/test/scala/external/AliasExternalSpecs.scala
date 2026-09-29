package external

// Deliberately OUTSIDE the `saferis` package to verify the `Alias("literal")` macro still expands when
// `Alias.unsafe` is `private[saferis]`. If macro hygiene did NOT preserve access, this would fail to compile.
// It also documents that user code cannot call the internal factory directly.

import saferis.*
import saferis.postgres.PostgresDialect
import scala.compiletime.testing.typeCheckErrors
import zio.test.*

object AliasExternalSpecs extends ZIOSpecDefault:

  def spec = suite("Alias from an external package")(
    test("Alias(\"literal\") macro compiles and works from outside the saferis package"):
      val alias = Alias("u")
      assertTrue(alias.value == "u", PostgresDialect.escapeIdentifier(alias) == "\"u\"")
    ,
    test("Alias.unsafe is not accessible from user code"):
      // unsafe is private[saferis]; a direct call must not typecheck from an external package.
      val errs = typeCheckErrors("""saferis.Alias.unsafe("x")""")
      assertTrue(errs.nonEmpty)
    ,
    test("Alias with a runtime variable still does NOT compile from an external package"):
      val errs = typeCheckErrors("""val x = "u"; saferis.Alias(x)""")
      assertTrue(errs.nonEmpty, errs.exists(_.message.contains("string literal"))),
  )
end AliasExternalSpecs
