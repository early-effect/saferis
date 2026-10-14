package saferis.postgres

import zio.durationInt
import zio.test.*

/** Postgres SQL both drivers run: arrays, enums, jsonb, returning, upsert, and the Postgres catalog. Portable behavior
  * stays in `saferis.tests.SqlSessionConformance`.
  */
object PostgresConformance:
  def suite =
    zio.test.suite("postgres")(
      ValueConformance.conformance,
      InCollectionIntegrationSpecs.conformance,
      DataManipulationLayerSpecs.conformance,
      SchemaIntegrationSpecs.conformance,
      SchemaValidationSpecs.conformance,
      StreamSpecs.conformance,
      PagedStreamSpecs.conformance,
      UpsertSpecs.conformance,
    )
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)
end PostgresConformance
