package saferis.docs

import saferis.*
import saferis.Schema.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object ErrorHandling extends SaferisDocSpecSuite:

  @tableName("error_handling_users")
  case class ErrorUser(@generated @key id: Int, email: String, name: String) derives Table

  @tableName("error_handling_unique_emails")
  case class UniqueEmail(@generated @key id: Int, email: String) derives Table

  @tableName("error_handling_crash_keys")
  case class CrashKey(@key id: Int, label: String) derives Table

  def doc = page("Error Handling")(
    md"""Saferis uses a principled error type hierarchy defined in [SaferisError.scala](https://github.com/russwyte/saferis/blob/main/core/src/main/scala/saferis/SaferisError.scala) that enables proper pattern matching and eliminates unsafe casting. All database operations return `ZIO` effects with `SaferisError` as the error type.""",
    section("Error Categories")(
      md"""| Error Type | When It Occurs |
|------------|----------------|
| `UniqueViolation` | Unique violation. Message and SQLSTATE stay as sent (`23505` on Postgres, `23000` on MySQL). |
| `ForeignKeyViolation` | Foreign key (`23503` on Postgres). |
| `NotNullViolation` | Not null (`23502` on Postgres). |
| `CheckViolation` | Check (`23514` on Postgres). The message stays the server's sentence. |
| `Deadlock` | The driver named a deadlock (Postgres `40P01`, MySQL errno 1213). |
| `SerializationFailure` | `40001`, or SQLite busy and locked. |
| `SyntaxError` | Class `42`. |
| `UndefinedTable` | Postgres `42P01`. |
| `UndefinedColumn` | Postgres `42703`. |
| `DataError` | Class `22`. |
| `Aborted` | The server reported an aborted transaction (Postgres `25P02`). |
| `QueryError` | A server error with no named condition. |
| `Timeout` | The driver canceled the statement, or Postgres `57014`. See [Statement Timeouts](statement-timeouts.html) |
| `Shutdown` | Postgres `57P01`, `57P02`, or `57P03`. |
| `Retryable` | `SqlCondition.Other` that the vendor hook marked transient. See [Retryable Errors](retryable-errors.html) |
| `ConnectionLost` | Class `08`, or a socket failure. A socket has no SQLSTATE. |
| `ConnectionError` | Cannot acquire a connection, or `configure` threw |
| `DecodingError` | Cannot decode a column value to the expected Scala type |
| `EncodingError` | Cannot encode a parameter value for the prepared statement |
| `ReturningOperationFailed` | INSERT/UPDATE/DELETE RETURNING returned no rows |
| `SchemaValidation` | Schema verification found mismatches (see [Schema Validation](schema-validation.html)) |
| `Unexpected` | Non-SQL errors (wrapped in Unexpected) |"""
    ),
    section("SQL Error Classification")(
      md"""The case is the condition. The message and the SQLSTATE are what the server sent. The cases do not carry a `Throwable`.

| What the server sent | Error |
|----------|------------|
| `23505` | `UniqueViolation` |
| `23503` | `ForeignKeyViolation` |
| `23502` | `NotNullViolation` |
| `23514` | `CheckViolation` |
| `40001` | `SerializationFailure` |
| class `08` | `ConnectionLost` |
| class `42` | `SyntaxError` |
| class `22` | `DataError` |
| Postgres `40P01` | `Deadlock` |
| Postgres `25P02` | `Aborted` |
| Postgres `57014` | `Timeout` |
| Postgres `42P01` | `UndefinedTable` |
| Postgres `42703` | `UndefinedColumn` |
| Postgres `57P01`, `57P02`, `57P03` | `Shutdown` |
| other | `QueryError`, or `Retryable` when the vendor hook matches `SqlCondition.Other` |"""
    ),
    section("Pattern Matching on Errors")(
      md"""Use pattern matching for type-safe error handling:""",
      exampleZIO {
        (for
          _      <- ddl.createTable[ErrorUser](ifNotExists = true)
          _      <- dml.insert(ErrorUser(-1, "alice@example.com", "Alice"))
          _      <- dml.insert(ErrorUser(-1, "bob@example.com", "Bob"))
          result <- sql"SELECT * FROM ${Table[ErrorUser]} WHERE ${Table[ErrorUser].email} = ${"alice@example.com"}"
            .queryOne[ErrorUser]
            .either
        yield result match
          case Left(SaferisError.DecodingError(col, expected, _)) =>
            s"Failed to decode column '$col' as $expected"
          case Left(error) =>
            s"Other error: ${error.message}"
          case Right(user) =>
            s"Found user: ${user.map(_.name).getOrElse("none")}"
        ).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(msg) => assertTrue(msg.contains("Alice"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
    ),
    section("Handling Constraint Violations")(
      exampleZIO {
        val schema = Schema[UniqueEmail].withUniqueConstraint(_.email).build
        (for
          _ <- ddl.createTable(schema)
          _ <- dml.insert(UniqueEmail(-1, "alice@example.com"))
          // Try to insert duplicate email
          result <- dml.insert(UniqueEmail(-1, "alice@example.com")).either
        yield result match
          case Left(SaferisError.UniqueViolation(detail)) =>
            s"Unique violation: ${detail.constraint.getOrElse("unknown")}"
          case Left(error) =>
            s"Other error: ${error.message}"
          case Right(_) =>
            "Insert succeeded"
        ).either
          .provideLayer(DocsTransactor.layer)
      }.assert {
        case Right(msg) => assertTrue(msg.contains("Unique violation"))
        case Left(err)  => assertTrue(false).label(err.message)
      },
      md"""### What the violation looks like unhandled

The previous example caught the error with `.either`. If you *don't* recover, the
constraint violation propagates as a real failure; the actual `SaferisError` is
shown below the snippet:""",
      expectCrash {
        (for
          _ <- ddl.createTable[CrashKey](ifNotExists = true)
          _ <- dml.insert(CrashKey(1, "first"))
          _ <- dml.insert(CrashKey(1, "duplicate")) // duplicate primary key → constraint violation
        yield ())
          .provideLayer(DocsTransactor.layer)
      }.assert(c => assertTrue(c.failures.nonEmpty)),
    ),
    section("Classifying a SQLSTATE")(
      md"""Drivers call `SqlState.classify`. Application code matches the case. There is no `fromThrowable`.""",
      exampleValue {
        val unique = SqlState.classify(
          ServerError(
            SqlCondition.Unique(Some(ConstraintName("users_email_key"))),
            "duplicate key",
            Some(SqlState.UniqueViolation),
          ),
          Some(SqlText("insert into users values ($1)")),
          _ => false,
        )
        val other = SqlState.classify(ServerError(SqlCondition.Other, "something went wrong"), None, _ => false)
        (unique, other)
      }.assert {
        case (SaferisError.UniqueViolation(detail), _: SaferisError.QueryError) =>
          assertTrue(detail.constraint.contains("users_email_key"), detail.message == "duplicate key")
        case other => assertTrue(false).label(s"unexpected: $other")
      },
    ),
    section("Accessing Error Details")(
      md"""Each error type provides relevant details:""",
      exampleValue {
        def logError(error: SaferisError): String = error match
          case e: SaferisError.UniqueViolation =>
            s"Unique ${e.detail.constraint.getOrElse("constraint")} violated. SQL: ${e.detail.sql.getOrElse("N/A")}"
          case e: SaferisError.ForeignKeyViolation =>
            s"Foreign key violated. SQL: ${e.detail.sql.getOrElse("N/A")}"
          case e: SaferisError.SyntaxError =>
            s"Syntax error: ${e.message}. SQL: ${e.detail.sql.getOrElse("N/A")}"
          case e: SaferisError.DecodingError =>
            s"Failed to decode column '${e.columnName}' as ${e.expectedType}"
          case e: SaferisError.SchemaValidation =>
            s"Schema issues:\n${e.issues.map(_.description).mkString("\n")}"
          case e =>
            e.message

        val sample = SqlState.classify(
          ServerError(
            SqlCondition.Unique(Some(ConstraintName("users_email_key"))),
            "duplicate key",
            Some(SqlState.UniqueViolation),
          ),
          Some(SqlText("insert into users values ($1)")),
          _ => false,
        )
        logError(sample)
      }.assert(msg => assertTrue(msg.contains("Unique") && msg.contains("violated"))),
    ),
  )
end ErrorHandling
