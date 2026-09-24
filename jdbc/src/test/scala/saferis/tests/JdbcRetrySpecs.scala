package saferis.tests

import saferis.*
import saferis.tests.DataSourceProvider
import zio.*
import zio.test.*
import zio.test.Assertion.*

object JdbcRetrySpecs extends ZIOSpecDefault:

  private val syntaxIsRetryable: ServerError => Boolean =
    e => e.sqlState.exists(_.startsWith("42"))

  private val customClassifierLayer =
    DataSourceProvider.configured(JdbcSessionConfig(retry = syntaxIsRetryable))

  private val defaultClassifierLayer =
    DataSourceProvider.default

  private val brokenQuery = sql"deli meat from nowhere".dml

  override def spec = suite("JdbcSession retry hook")(
    test("a syntax error stays SyntaxError even when the hook matches class 42"):
      for result <- brokenQuery.exit
      yield assert(result)(fails(isSubtype[SaferisError.SyntaxError](anything)))
    .provideShared(customClassifierLayer),
    test("default hook leaves syntax errors as SyntaxError"):
      for result <- brokenQuery.exit
      yield assert(result)(fails(isSubtype[SaferisError.SyntaxError](anything)))
    .provideShared(defaultClassifierLayer),
    test("the hook does not resume a failed transaction"):
      for result <- transact(brokenQuery).exit
      yield assert(result)(fails(isSubtype[SaferisError.SyntaxError](anything)))
    .provideShared(customClassifierLayer),
  ) @@ TestAspect.sequential

end JdbcRetrySpecs
