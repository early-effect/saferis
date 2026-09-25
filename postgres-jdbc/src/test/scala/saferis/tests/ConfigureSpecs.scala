package saferis.tests

import saferis.*
import saferis.jdbc.JdbcSessionConfig

import zio.*
import zio.test.*

/** JDBC checkout settings. These do not belong in the shared conformance suite. */
object ConfigureSpecs extends ZIOSpecDefault:
  def spec =
    suite("JDBC configure")(
      test("configure runs before an autocommit statement"):
        val session = DataSourceProvider.configured:
          JdbcSessionConfig(configure = conn =>
            val statement = conn.createStatement()
            try statement.execute("set search_path to pg_catalog")
            finally statement.close())
        for schema <- sql"select current_schema()".queryValue[String].provide(session)
        yield assertTrue(schema.contains("pg_catalog"))
      ,
      test("a later command with no timeout keeps the role statement_timeout"):
        val session = DataSourceProvider.default
        val body    =
          for
            _    <- sql"alter role current_user set statement_timeout = '1s'".dml
            exit <- transact(
              sql"select 1".withTimeout(8.seconds).queryValue[Int] *>
                sql"select pg_sleep(3)".queryValue[Int]
            ).exit
          yield exit
        body
          .ensuring(sql"alter role current_user reset statement_timeout".dml.ignore)
          .provide(session)
          .map: exit =>
            assertTrue:
              exit match
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(_: SaferisError.Timeout) => true
                    case _                             => false
                case _ => false,
    ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
end ConfigureSpecs
