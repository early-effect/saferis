package saferis.tests

import saferis.Dialect
import saferis.SaferisError
import saferis.SqlSession

import zio.ZIO
import zio.durationInt
import zio.test.*

/** Behavior every shipped database runs. A driver proves itself by providing a `SqlSession` and a [[DatabaseTarget]].
  *
  * Dialect suites (Postgres SQL, a catalog, a statement timeout) are composed by that dialect. They are not gated here,
  * so a database that does not speak them does not report them as ignored.
  */
object SqlSessionConformance:
  val suite: Spec[SqlSession & DatabaseTarget, Any] =
    zio.test.suite("conformance")(
      TransactionConformance.conformance,
      PortableDmlConformance.conformance,
      IdentifierConformance.conformance,
    )
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)

  /** Run `body` with the target's dialect as the given `Dialect`. */
  def withDialect[R, A](body: Dialect ?=> ZIO[R, SaferisError, A]): ZIO[R & DatabaseTarget, SaferisError, A] =
    ZIO.serviceWithZIO[DatabaseTarget](target => body(using target.dialect))
end SqlSessionConformance
