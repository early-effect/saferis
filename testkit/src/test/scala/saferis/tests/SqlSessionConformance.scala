package saferis.tests

import saferis.Dialect
import saferis.SqlFragment
import saferis.SqlSession
import saferis.postgres.PostgresDialect
import saferis.sql

import zio.ZLayer
import zio.durationInt
import zio.test.*

/** The same suites for every driver. The session layer and the dialect are the only things that change. */
object SqlSessionConformance:

  /** Behavior every database Saferis ships has to share, with SQL the dialect renders.
    *
    * @param longStatement
    *   A statement that runs for several seconds, on a database that has one. The timeout test runs only then.
    */
  def portable[R, E](
      driver: String,
      session: ZLayer[R, E, SqlSession],
      longStatement: Option[SqlFragment],
  )(using Dialect): Spec[R, Any] =
    zio.test
      .suite(s"$driver portable conformance")(
        TransactionConformance.conformance(longStatement),
        PortableDmlConformance.conformance,
      )
      .provideSomeShared[R](session)
      @@ TestAspect.sequential
      @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)

  /** The portable suite plus everything Postgres adds: arrays, enums, `jsonb`, catalog verification, and upsert. */
  def postgres[E](
      driver: String,
      session: ZLayer[PostgresTestContainer, E, SqlSession],
  ): Spec[PostgresTestContainer, Any] =
    given Dialect = PostgresDialect
    zio.test.suite(s"$driver conformance")(
      portable(driver, session, Some(sql"select pg_sleep(5)")),
      zio.test
        .suite(s"$driver Postgres conformance")(
          ValueConformance.conformance,
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
        @@ TestAspect.timeout(30.seconds),
    ) @@ TestAspect.sequential
  end postgres
end SqlSessionConformance
