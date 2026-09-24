package saferis.tests

import saferis.Dialect
import saferis.SaferisError
import saferis.SqlSession

import zio.ZIO
import zio.durationInt
import zio.test.*

/** One suite for every database. A driver proves itself by providing a `SqlSession` and a [[DatabaseTarget]]:
  *
  * {{{
  *   SqlSessionConformance.suite.provideShared(session ++ ZLayer.succeed(DatabaseTarget(MySQLDialect, ...)))
  * }}}
  *
  * The portable tests always run. A group gated by a [[Capability]] runs only when the target declares it.
  */
object SqlSessionConformance:
  val suite: Spec[SqlSession & DatabaseTarget, Any] =
    zio.test.suite("conformance")(
      TransactionConformance.conformance,
      PortableDmlConformance.conformance,
      SchemaConformance.conformance.whenZIO(has(Capability.Catalog)),
      postgresSql.whenZIO(has(Capability.PostgresSql)),
    )
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)

  /** Run `body` with the target's dialect as the given `Dialect`. */
  def withDialect[R, A](body: Dialect ?=> ZIO[R, SaferisError, A]): ZIO[R & DatabaseTarget, SaferisError, A] =
    ZIO.serviceWithZIO[DatabaseTarget](target => body(using target.dialect))

  private def has(capability: Capability) =
    ZIO.serviceWith[DatabaseTarget](_.has(capability))

  private def postgresSql =
    zio.test.suite("Postgres SQL")(
      ValueConformance.conformance,
      InCollectionIntegrationSpecs.conformance,
      DataManipulationLayerSpecs.conformance,
      SchemaIntegrationSpecs.conformance,
      SchemaValidationSpecs.conformance,
      StreamSpecs.conformance,
      PagedStreamSpecs.conformance,
      UpsertSpecs.conformance,
    )
end SqlSessionConformance
