package saferis.docs

import saferis.*
import saferis.jdbc.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object RetryableErrors extends SaferisDocSpecSuite:

  @tableName("retryable_errors_users")
  case class User(@generated @key id: Int, name: String) derives Table

  private def worthRetry(error: SaferisError): Boolean = error match
    case _: SaferisError.ConnectionLost | _: SaferisError.Deadlock | _: SaferisError.SerializationFailure |
        _: SaferisError.Retryable =>
      true
    case _ => false

  def doc = page("Retryable Errors")(
    md"""Some database failures are transient and worth retrying: connection loss, deadlocks, serialization failures, and vendor transport errors. Those are distinct cases. A vendor hook does not rename them.""",
    section("Named transient states")(
      md"""`SqlState.classify` maps the standard codes before it consults a vendor hook:

- `08xxx`: `ConnectionLost`
- `40001`: `SerializationFailure`
- `40P01`: `Deadlock`
- `57014`, or a driver statement timeout: `Timeout`
- `23505`: `UniqueViolation` (message `unique violation`)
- `42xxx`: `SyntaxError`, even when the vendor hook returns true

`SqlState.defaultRetryable` is true for class `08`, `40001`, and `40P01`. The JDBC session uses that as the default `JdbcSessionConfig.retry` hook, and the hook only fills codes that are not already named. A match becomes `Retryable`."""
    ),
    section("Driving retries with ZIO")(
      exampleValue {
        def reportWithRetry(session: SqlSession) =
          (sql"SELECT * FROM ${Table[User]}"
            .query[User])
            .retry(
              Schedule.recurs(3) && Schedule.exponential(100.millis) && Schedule.recurWhile[SaferisError](worthRetry)
            )
        reportWithRetry
      }.assert(_ => assertTrue(true))
    ),
    section("Supplying a vendor hook")(
      md"""Drivers that tunnel over HTTP (Databricks, Snowflake) can surface transport errors as vendor-specific codes. Put a hook on `JdbcSessionConfig.retry`. It does not replace the named states above:""",
      exampleValue {
        val databricks: ServerError => Boolean =
          e => e.vendorCode.contains(8000)

        val session = JdbcSession.layer(JdbcSessionConfig(retry = databricks))
        val vendor  = SqlState.classify(
          ServerError(Some("8000"), "http blip", vendorCode = Some(8000)),
          Some("select 1"),
          databricks,
        )
        (vendor, session)
      }.assert { case (classified, _) =>
        assertTrue(classified.isInstanceOf[SaferisError.Retryable])
      },
      md"""Named states stay named. The hook only classifies what `SqlState.classify` would otherwise call `QueryError`.""",
    ),
    section("When *not* to retry")(
      md"""`SaferisError.Retryable` only signals that an error *might* be safe to retry; your application still owns the semantics:

- For **read-only queries**, retrying is generally safe.
- For **writes**, retry only if the operation is idempotent (e.g., upsert by primary key, set-to-fixed-value updates) or if the failure happened before any partial state could be observed.
- For **transactions**, the whole transaction must be re-attempted; a single statement retry inside a failed transaction is meaningless."""
    ),
  )
end RetryableErrors
