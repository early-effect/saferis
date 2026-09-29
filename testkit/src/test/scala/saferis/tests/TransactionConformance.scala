package saferis.tests

import saferis.*

import zio.{test as _, *}
import zio.test.*

/** Transaction behavior every driver has to share. No pool, listener, or socket assumptions, and only SQL that every
  * database Saferis ships speaks.
  */
object TransactionConformance:
  def conformance =
    suite("transaction semantics")(portable*)

  /** A dialect that can block for several seconds includes this. The statement is that dialect's sleep. */
  def cancelsLongStatement(statement: SqlFragment)(using Dialect) =
    test("a one second cap cancels a long statement"):
      for
        command <- statement.withTimeout(1.second).toCommand
        exit    <- ZIO.serviceWithZIO[SqlSession](_.query(command)(_ => Right(()))).exit
      yield assertTrue:
        exit match
          case Exit.Failure(cause) =>
            cause.failureOption match
              case Some(_: SaferisError.Timeout) => true
              case _                             => false
          case _ => false
    @@ TestAspect.withLiveClock
      @@ TestAspect.timeout(30.seconds)

  private def portable =
    List(
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
      test("a unique violation keeps the server message"):
        for
          _    <- sql"drop table if exists conformance_uniq".dml
          _    <- sql"create table conformance_uniq (id integer primary key)".dml
          _    <- sql"insert into conformance_uniq (id) values (1)".dml
          exit <- sql"insert into conformance_uniq (id) values (1)".dml.exit
        yield assertTrue:
          exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(SaferisError.UniqueViolation(detail)) => detail.message.nonEmpty
                case _                                          => false
            case _ => false
      ,
      test("a failed stream inside transact rolls back"):
        for
          _    <- sql"drop table if exists conformance_stream_abort".dml
          _    <- sql"create table conformance_stream_abort (id integer primary key)".dml
          exit <- transact(
            for
              _       <- sql"insert into conformance_stream_abort (id) values (1)".dml
              command <- sql"select * from conformance_stream_missing".toCommand
              _       <- ZIO.serviceWithZIO[SqlSession]: session =>
                session.stream(command)(_ => Right(())).runDrain
            yield 1
          ).exit
          count <- sql"select count(*) from conformance_stream_abort".queryValue[Long]
        yield assertTrue(exit.isFailure, count.contains(0L)),
    )

  /** MySQL, SQLite, and H2 keep the transaction open after a constraint failure. Postgres does not, so this stays off
    * the portable suite.
    */
  def continuesAfterUniqueViolation =
    test("a caught unique violation leaves the transaction open and the commit persists"):
      for
        _      <- sql"drop table if exists conformance_continue".dml
        _      <- sql"create table conformance_continue (id integer primary key)".dml
        caught <- Ref.make(false)
        _      <- transact(
          for
            _ <- sql"insert into conformance_continue (id) values (1)".dml
            _ <- sql"insert into conformance_continue (id) values (1)".dml.catchAll(_ => caught.set(true).as(0L))
            _ <- sql"insert into conformance_continue (id) values (2)".dml
          yield ()
        )
        did   <- caught.get
        one   <- sql"select id from conformance_continue where id = 1".queryValue[Int]
        two   <- sql"select id from conformance_continue where id = 2".queryValue[Int]
        count <- sql"select count(*) from conformance_continue".queryValue[Long]
      yield assertTrue(did, one.contains(1), two.contains(2), count.contains(2L))
end TransactionConformance
