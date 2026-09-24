package saferis.tests

import saferis.*

import zio.{test as _, *}
import zio.test.*

/** Transaction behavior every driver has to share. No pool, listener, or socket assumptions. */
object TransactionConformance:
  def conformance =
    suite("transaction semantics")(
      test("a failed transact rolls back"):
        for
          _    <- sql"drop table if exists conformance_rollback".dml
          _    <- sql"create table conformance_rollback (id integer primary key)".dml
          exit <- transact(
            sql"insert into conformance_rollback (id) values (1)".dml *>
              ZIO.fail(SaferisError.Unexpected("rollback"))
          ).exit
          count <- sql"select count(*) from conformance_rollback".queryValue[Long]
        yield assertTrue(exit.isFailure, count.contains(0L))
      ,
      test("nested transact joins and a later failure rolls the outer write back"):
        for
          _    <- sql"drop table if exists conformance_nested".dml
          _    <- sql"create table conformance_nested (id integer primary key)".dml
          seen <- Ref.make[Option[Long]](None)
          exit <- transact(
            for
              _ <- transact(sql"insert into conformance_nested (id) values (1)".dml)
              n <- sql"select count(*) from conformance_nested".queryValue[Long]
              _ <- seen.set(n)
              _ <- ZIO.fail(SaferisError.Unexpected("still open"))
            yield ()
          ).exit
          captured <- seen.get
          count    <- sql"select count(*) from conformance_nested".queryValue[Long]
        yield assertTrue(captured.contains(1L), count.contains(0L), exit.isFailure)
      ,
      test("catching a statement failure makes the next command 25P02"):
        for
          _    <- sql"drop table if exists conformance_abort".dml
          _    <- sql"create table conformance_abort (id integer primary key)".dml
          seen <- Ref.make[Option[Either[SaferisError, Option[Long]]]](None)
          exit <- transact(
            for
              _     <- sql"insert into conformance_abort (id) values (1)".dml
              _     <- sql"deli meat from conformance_abort".dml.catchAll(_ => ZIO.succeed(0L))
              later <- sql"select count(*) from conformance_abort".queryValue[Long].either
              _     <- seen.set(Some(later))
            yield ()
          ).exit
          captured <- seen.get
          count    <- sql"select count(*) from conformance_abort".queryValue[Long]
          aborted = captured match
            case Some(Left(SaferisError.QueryError(Some("25P02"), _, _))) => true
            case _                                                        => false
          recorded = exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(_: SaferisError.SyntaxError) => true
                case _                                 => false
            case _ => false
        yield assertTrue(aborted, recorded, count.contains(0L))
      ,
      test("a unique violation message is exactly unique violation"):
        for
          _    <- sql"drop table if exists conformance_uniq".dml
          _    <- sql"create table conformance_uniq (id integer primary key)".dml
          _    <- sql"insert into conformance_uniq (id) values (1)".dml
          exit <- sql"insert into conformance_uniq (id) values (1)".dml.exit
        yield assertTrue:
          exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(SaferisError.UniqueViolation(_, message, _)) => message == "unique violation"
                case _                                                 => false
            case _ => false
      ,
      test("a one second cap cancels pg_sleep"):
        for exit <- sql"select pg_sleep(5)".withTimeout(1.second).queryValue[Int].exit
        yield assertTrue:
          exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(_: SaferisError.Timeout) => true
                case _                             => false
            case _ => false
      ,
      test("catching a failed stream inside transact does not commit"):
        for
          _    <- sql"drop table if exists conformance_stream_abort".dml
          _    <- sql"create table conformance_stream_abort (id integer primary key)".dml
          exit <- transact(
            for
              _       <- sql"insert into conformance_stream_abort (id) values (1)".dml
              command <- sql"select * from conformance_stream_missing".toCommand
              _       <- ZIO.serviceWithZIO[SqlSession]: session =>
                session.stream(command)(_ => Right(())).runDrain.catchAll(_ => ZIO.unit)
            yield 1
          ).exit
          count <- sql"select count(*) from conformance_stream_abort".queryValue[Long]
        yield assertTrue(exit.isFailure, count.contains(0L)),
    )
end TransactionConformance
