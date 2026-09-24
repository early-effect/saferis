package saferis.tests

import saferis.SqlSession

import zio.ZLayer
import zio.durationInt
import zio.test.*

/** One suite, two drivers. The session layer is the only thing that changes. */
object SqlSessionConformance:
  def suite[E](
      driver: String,
      session: ZLayer[PostgresTestContainer, E, SqlSession],
  ): Spec[PostgresTestContainer, Any] =
    zio.test
      .suite(s"$driver conformance")(
        TransactionConformance.conformance,
        InCollectionIntegrationSpecs.conformance,
        DataManipulationLayerSpecs.conformance,
        SchemaIntegrationSpecs.conformance,
        SchemaValidationSpecs.conformance,
        StreamSpecs.conformance,
        PagedStreamSpecs.conformance,
        UpsertSpecs.conformance,
      )
      .provideSomeShared[PostgresTestContainer](session)
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)
end SqlSessionConformance
