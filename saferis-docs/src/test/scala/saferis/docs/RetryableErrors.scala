package saferis.docs

import saferis.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

import java.sql.SQLException

object RetryableErrors extends SaferisDocSpecSuite:

  @tableName("retryable_errors_users")
  case class User(@generated @key id: Int, name: String) derives Table

  def doc = page("Retryable Errors")(
    md"""Some database failures are transient and worth retrying; connection blips, deadlocks, serialization failures, transport errors on HTTP-tunnelled drivers (Databricks, Snowflake). Saferis classifies such errors as `SaferisError.Retryable` so that a `ZIO.retry` policy can target exactly those cases without your code grokking JDBC error codes.""",
    section("The default classifier")(
      md"""Every Dialect ships a default classifier (`Dialect.retryClassifier`) that recognizes standard transient SQLState classes:

- `08xxx`: connection exception (the driver lost or could not establish a connection)
- `40001`: serialization failure
- `40P01`: deadlock detected

If a thrown `SQLException` matches one of these, Saferis emits `SaferisError.Retryable` instead of the usual SQLState-based variant.""",
    ),
    section("Driving retries with ZIO")(
      exampleValue {
        def reportWithRetry(xa: Transactor) =
          xa.run(sql"SELECT * FROM ${Table[User]}".query[User])
            .retry(
              Schedule.recurs(3) && Schedule.exponential(100.millis) && Schedule.recurWhile[SaferisError]:
                case _: SaferisError.Retryable => true
                case _                         => false
            )
        reportWithRetry
      }.assert(_ => assertTrue(true)),
    ),
    section("Supplying a custom classifier")(
      md"""Drivers that tunnel over HTTP (Databricks, Snowflake) can surface transport errors as vendor-specific codes that the standards-based default does not catch. Provide your own classifier on the Transactor; it replaces the dialect default:""",
      exampleValue {
        // Treat Databricks vendor code 8000 (HTTP transport error) as transient.
        val databricksClassifier: SaferisError.RetryClassifier =
          case e: SQLException =>
            e.getErrorCode == 8000 || SaferisError.defaultRetryClassifier(e)
          case _ => false

        val xaLayer = Transactor.layer(retryClassifier = Some(databricksClassifier))
        (databricksClassifier(new SQLException("http blip", "08000", 8000)), xaLayer)
      }.assert {
        case (classified, _) => assertTrue(classified)
      },
      md"""Composing with the default (as shown above) keeps the standard SQLState rules and adds your driver-specific quirks on top.""",
    ),
    section("When *not* to retry")(
      md"""`SaferisError.Retryable` only signals that an error *might* be safe to retry; your application still owns the semantics:

- For **read-only queries**, retrying is generally safe.
- For **writes**, retry only if the operation is idempotent (e.g., upsert by primary key, set-to-fixed-value updates) or if the failure happened before any partial state could be observed.
- For **transactions**, the whole transaction must be re-attempted; a single statement retry inside a failed transaction is meaningless.""",
    ),
  )
end RetryableErrors
