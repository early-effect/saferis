package saferis.tests

import saferis.*
import zio.test.*

object RetryableSpecs extends ZIOSpecDefault:

  private def classified(state: Option[SqlState], vendor: Boolean = false): SaferisError =
    SqlState.classify(
      ServerError(state, s"server ${state.getOrElse("none")}", Some(ConstraintName("constraint_name"))),
      Some(SqlText("insert into t values ($1)")),
      _ => vendor,
    )

  private def classified(state: SqlState): SaferisError = classified(Some(state))

  private val classifyTests = suite("SqlState.classify")(
    test("08xxx is ConnectionLost and retryable by name"):
      val err = classified(SqlState.ConnectionFailure)
      assertTrue(err.isInstanceOf[SaferisError.ConnectionLost]) &&
      assertTrue(SqlState.defaultRetryable(Some(SqlState.ConnectionFailure)))
    ,
    test("40001 is SerializationFailure"):
      assertTrue(classified(SqlState.SerializationFailure).isInstanceOf[SaferisError.SerializationFailure])
    ,
    test("40P01 is Deadlock"):
      assertTrue(classified(SqlState.Deadlock).isInstanceOf[SaferisError.Deadlock])
    ,
    test("23505 redacts the server message"):
      classified(SqlState.UniqueViolation) match
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
      val check = classified(SqlState.CheckViolation)
      val fk    = classified(SqlState.ForeignKeyViolation)
      assertTrue(
        check match
          case SaferisError.ConstraintViolation(SqlState.CheckViolation, _, "check violation", _) => true
          case _                                                                                  => false
        ,
        fk match
          case SaferisError.ConstraintViolation(SqlState.ForeignKeyViolation, _, message, _) =>
            message == "server 23503"
          case _ => false,
      )
    ,
    test("42 stays SyntaxError even when the vendor hook matches"):
      assertTrue(classified(Some(SqlState.SyntaxError), vendor = true).isInstanceOf[SaferisError.SyntaxError])
    ,
    test("57014 is Timeout even when the vendor hook matches"):
      val err = SqlState.classify(
        ServerError(Some(SqlState.QueryCanceled), "cancel"),
        Some(SqlText("select 1")),
        _ => true,
      )
      assertTrue(err.isInstanceOf[SaferisError.Timeout])
    ,
    test("no SQLSTATE is Retryable when the vendor hook matches"):
      assertTrue(classified(None, vendor = true).isInstanceOf[SaferisError.Retryable]),
  )

  private val parseTests = suite("SqlState.parse")(
    test("accepts five digits or upper-case letters"):
      assertTrue(
        SqlState.parse("23505").contains(SqlState.UniqueViolation),
        SqlState.parse("25P02").contains(SqlState.InFailedTransaction),
        SqlState.parse("HY000").isDefined,
      )
    ,
    test("rejects vendor codes, transport codes, and lower case"):
      assertTrue(
        SqlState.parse("8000").isEmpty,
        SqlState.parse("ECONNRESET").isEmpty,
        SqlState.parse("25p02").isEmpty,
        SqlState.parse("").isEmpty,
      ),
  )

  override def spec = suite("Retryable error classification")(
    classifyTests,
    parseTests,
  )

end RetryableSpecs
