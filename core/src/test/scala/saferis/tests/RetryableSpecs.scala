package saferis.tests

import saferis.*
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.sql.SQLException

object RetryableSpecs extends ZIOSpecDefault:

  private def classified(state: String, vendor: Boolean = false, timedOut: Boolean = false): SaferisError =
    SqlState.classify(
      Some(state),
      s"server $state",
      Some("constraint_name"),
      Some("insert into t values ($1)"),
      vendor,
      timedOut,
    )

  private val classifyTests = suite("SqlState.classify")(
    test("08xxx is ConnectionLost and retryable by name"):
      val err = classified("08006")
      assertTrue(err.isInstanceOf[SaferisError.ConnectionLost]) &&
      assertTrue(SqlState.defaultRetryable(Some("08006")))
    ,
    test("40001 is SerializationFailure"):
      assertTrue(classified("40001").isInstanceOf[SaferisError.SerializationFailure])
    ,
    test("40P01 is Deadlock"):
      assertTrue(classified("40P01").isInstanceOf[SaferisError.Deadlock])
    ,
    test("23505 redacts the server message"):
      classified("23505") match
        case SaferisError.UniqueViolation(constraint, message, sql) =>
          assertTrue(
            constraint.contains("constraint_name"),
            message == "unique violation",
            sql.exists(_.contains("insert")),
          )
        case other =>
          assertTrue(false).label(other.message)
    ,
    test("23514 redacts the server message and other class 23 keeps it"):
      val check = classified("23514")
      val fk    = classified("23503")
      assertTrue(
        check match
          case SaferisError.ConstraintViolation("23514", _, "check violation", _) => true
          case _                                                                  => false
        ,
        fk match
          case SaferisError.ConstraintViolation("23503", _, message, _) => message == "server 23503"
          case _                                                        => false,
      )
    ,
    test("42 stays SyntaxError even when the vendor hook matches"):
      assertTrue(classified("42601", vendor = true).isInstanceOf[SaferisError.SyntaxError])
    ,
    test("timeout wins over a vendor hook"):
      val err = SqlState.classify(Some("42601"), "cancel", None, Some("select 1"), vendorRetry = true, timedOut = true)
      assertTrue(err.isInstanceOf[SaferisError.Timeout])
    ,
    test("vendor code is Retryable"):
      assertTrue(classified("8000", vendor = true).isInstanceOf[SaferisError.Retryable]),
  )

  private val syntaxIsRetryable: SQLException => Boolean =
    e => Option(e.getSQLState).exists(_.startsWith("42"))

  private val customClassifierLayer =
    DataSourceProvider.configured(JdbcSessionConfig(retry = syntaxIsRetryable))

  private val defaultClassifierLayer =
    DataSourceProvider.default

  private val brokenQuery = sql"deli meat from nowhere".dml

  private val wiringTests = suite("JdbcSession retry hook")(
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
  )

  override def spec = suite("Retryable error classification")(
    classifyTests,
    wiringTests,
  ) @@ TestAspect.sequential

end RetryableSpecs
