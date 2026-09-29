package saferis.tests

import saferis.*

import zio.test.*

object RetryableSpecs extends ZIOSpecDefault:

  private def reported(state: Option[SqlState], message: String): ServerError =
    val condition = state match
      case Some(code) if code == SqlState.UniqueViolation =>
        SqlCondition.Unique(Some(ConstraintName("constraint_name")))
      case other => SqlCondition.fromSqlState(other)
    ServerError(condition, message, state)

  private def classified(state: Option[SqlState], vendor: Boolean = false): SaferisError =
    SqlState.classify(
      reported(state, s"server ${state.getOrElse("none")}"),
      Some(SqlText("insert into t values ($1)")),
      _ => vendor,
    )

  private def classified(state: SqlState): SaferisError = classified(Some(state))

  private val classifyTests = suite("SqlState.classify")(
    test("class 08 is ConnectionLost and retryable"):
      val err = classified(SqlState.ConnectionException)
      assertTrue(
        err.isInstanceOf[SaferisError.ConnectionLost],
        SqlState.defaultRetryable(reported(Some(SqlState.ConnectionException), "lost")),
      )
    ,
    test("40001 is SerializationFailure"):
      assertTrue(classified(SqlState.SerializationFailure).isInstanceOf[SaferisError.SerializationFailure])
    ,
    test("23505 keeps the server message and the constraint"):
      classified(SqlState.UniqueViolation) match
        case SaferisError.UniqueViolation(detail) =>
          assertTrue(
            detail.constraint.contains("constraint_name"),
            detail.message == "server 23505",
            detail.sqlState.contains(SqlState.UniqueViolation),
            detail.sql.exists(_.contains("insert")),
          )
        case other =>
          assertTrue(false).label(other.message)
    ,
    test("23514 and 23503 keep the server message"):
      val check = classified(SqlState.CheckViolation)
      val fk    = classified(SqlState.ForeignKeyViolation)
      assertTrue(
        check match
          case SaferisError.CheckViolation(detail) => detail.message == "server 23514"
          case _                                   => false
        ,
        fk match
          case SaferisError.ForeignKeyViolation(detail) => detail.message == "server 23503"
          case _                                        => false,
      )
    ,
    test("class 42 stays SyntaxError even when the vendor hook matches"):
      val state = SqlState.parse("42000")
      assertTrue(classified(state, vendor = true).isInstanceOf[SaferisError.SyntaxError])
    ,
    test("Canceled is Timeout even when the vendor hook matches"):
      val err = SqlState.classify(
        ServerError(SqlCondition.Canceled, "cancel"),
        Some(SqlText("select 1")),
        _ => true,
      )
      assertTrue(err.isInstanceOf[SaferisError.Timeout], err.message == "cancel")
    ,
    test("Other is Retryable when the vendor hook matches"):
      assertTrue(classified(None, vendor = true).isInstanceOf[SaferisError.Retryable])
    ,
    test("a fixed code classifies any message the same way and the message is unchanged"):
      val codes = Gen.elements("23505", "23503", "23502", "08001", "22012", "42000", "40001")
      check(codes, Gen.string): (code, message) =>
        val state = SqlState.parse(code)
        val left  = reported(state, message)
        val right = reported(state, message.reverse)
        val got   = SqlState.classify(left, None, _ => false)
        assertTrue(left.condition == right.condition, got.message == message),
  )

  private val parseTests = suite("SqlState.parse")(
    test("succeeds exactly on five digits or upper-case letters"):
      check(Gen.string): raw =>
        val ok = raw.length == 5 && raw.forall(c => c.isDigit || (c >= 'A' && c <= 'Z'))
        assertTrue(SqlState.parse(raw).isDefined == ok)
  )

  override def spec = suite("Retryable error classification")(
    classifyTests,
    parseTests,
  )
end RetryableSpecs
