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
| `UniqueViolation` | SQLSTATE `23505`. The message is `unique violation` |
| `ConstraintViolation` | Other class `23` failures (foreign key, check, not null). `23514` says `check violation` |
| `Deadlock` | SQLSTATE `40P01` |
| `SerializationFailure` | SQLSTATE `40001` |
| `SyntaxError` | Class `42` |
| `DataError` | Class `22` |
| `QueryError` | Other SQL execution errors, including `25P02` after a failed statement |
| `Timeout` | Statement timeout, or SQLSTATE `57014`. See [Statement Timeouts](statement-timeouts.html) |
| `Retryable` | Vendor code the session hook marked transient. See [Retryable Errors](retryable-errors.html) |
| `ConnectionLost` | Class `08` |
| `ConnectionError` | Cannot acquire a connection, or `configure` threw |
| `DecodingError` | Cannot decode a column value to the expected Scala type |
| `EncodingError` | Cannot encode a parameter value for the prepared statement |
| `ReturningOperationFailed` | INSERT/UPDATE/DELETE RETURNING returned no rows |
| `SchemaValidation` | Schema verification found mismatches (see [Schema Validation](schema-validation.html)) |
| `Unexpected` | Non-SQL errors (wrapped in Unexpected) |"""
    ),
    section("SQL Error Classification")(
      md"""SQL errors are categorized by SQLSTATE. The cases do not carry a `Throwable`.

| SQLSTATE | Error Type |
|----------|------------|
| `23505` | `UniqueViolation` |
| `23514` | `ConstraintViolation` (`check violation`) |
| other `23` | `ConstraintViolation` (server message kept) |
| `40P01` | `Deadlock` |
| `40001` | `SerializationFailure` |
| `08` | `ConnectionLost` |
| `42` | `SyntaxError` |
| `22` | `DataError` |
| `57014` | `Timeout` |
| other | `QueryError`, or `Retryable` when the vendor hook matches |"""
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
          case Left(SaferisError.UniqueViolation(constraint, _, _)) =>
            s"Unique violation: ${constraint.getOrElse("unknown")}"
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
          ServerError(Some("23505"), "duplicate key", Some("users_email_key")),
          Some("insert into users values ($1)"),
          _ => false,
        )
        val other = SqlState.classify(ServerError(None, "something went wrong"), None, _ => false)
        (unique, other)
      }.assert {
        case (SaferisError.UniqueViolation(constraint, "unique violation", _), _: SaferisError.QueryError) =>
          assertTrue(constraint.contains("users_email_key"))
        case other => assertTrue(false).label(s"unexpected: $other")
      },
    ),
    section("Accessing Error Details")(
      md"""Each error type provides relevant details:""",
      exampleValue {
        def logError(error: SaferisError): String = error match
          case e: SaferisError.UniqueViolation =>
            s"Unique ${e.constraint.getOrElse("constraint")} violated. SQL: ${e.sql.getOrElse("N/A")}"
          case e: SaferisError.ConstraintViolation =>
            s"Constraint ${e.sqlState} violated. SQL: ${e.sql.getOrElse("N/A")}"
          case e: SaferisError.SyntaxError =>
            s"Syntax error: ${e.message}. SQL: ${e.sql.getOrElse("N/A")}"
          case e: SaferisError.DecodingError =>
            s"Failed to decode column '${e.columnName}' as ${e.expectedType}"
          case e: SaferisError.SchemaValidation =>
            s"Schema issues:\n${e.issues.map(_.description).mkString("\n")}"
          case e =>
            e.message

        val sample = SqlState.classify(
          ServerError(Some("23505"), "duplicate key", Some("users_email_key")),
          Some("insert into users values ($1)"),
          _ => false,
        )
        logError(sample)
      }.assert(msg => assertTrue(msg.contains("Unique") && msg.contains("violated"))),
    ),
  )
end ErrorHandling
