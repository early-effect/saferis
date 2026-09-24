package saferis.tests

import saferis.*
import zio.*
import zio.test.*

import java.time.Instant

import scala.scalajs.js

object PgSessionSpecs extends ZIOSpecDefault:
  private val pastInt8    = 9223372036854775807L
  private val pastNumeric = BigDecimal("9223372036854775808.25")
  private val micros      = Instant.parse("2024-09-23T15:04:05.123456Z")

  private def env(name: String): Option[String] =
    val value = js.Dynamic.global.process.env.selectDynamic(name)
    if js.isUndefined(value) || (value eq null) then None
    else
      val text = value.asInstanceOf[js.Any].toString
      if text.isEmpty then None else Some(text)

  private def pgConfig(
      defaultTimeout: Option[Duration] = None,
      listener: SqlListener = SqlListener.noop,
      poolSize: Int = 4,
  ): PgConfig =
    PgConfig(
      host = env("PGHOST").getOrElse("localhost"),
      port = env("PGPORT").flatMap(_.toIntOption).getOrElse(5432),
      database = env("PGDATABASE").getOrElse("postgres"),
      user = env("PGUSER").getOrElse("postgres"),
      password = env("PGPASSWORD").getOrElse(""),
      poolSize = poolSize,
      defaultTimeout = defaultTimeout,
      listener = listener,
    )

  private def session(config: PgConfig): ZLayer[Any, SaferisError, SqlSession] =
    ZLayer.succeed(config) >>> NodeSession.layer

  private val open  = session(pgConfig())
  private val timed = session(pgConfig(defaultTimeout = Some(30.seconds)))

  private final class Recording(events: Ref[Chunk[String]]) extends SqlListener:
    def executed(event: SqlExecuted): UIO[Unit] =
      events.update(_ :+ event.sql)

  private def isTimeout(exit: Exit[SaferisError, ?]): Boolean =
    exit match
      case Exit.Failure(cause) =>
        cause.failureOption match
          case Some(_: SaferisError.Timeout) => true
          case _                             => false
      case _ => false

  private val reads =
    suite("reads raw DateStyle=ISO text")(
      test("Int8 past 2^53-1 round-trips through $n::int8"):
        for
          _        <- sql"drop table if exists pg_int8".dml
          _        <- sql"create table pg_int8 (n bigint)".dml
          selected <- sql"select ${pastInt8}".queryValue[Long]
          _        <- sql"insert into pg_int8 (n) values (${pastInt8})".dml
          stored   <- sql"select n from pg_int8".queryValue[Long]
        yield assertTrue(selected.contains(pastInt8), stored.contains(pastInt8))
      ,
      test("Numeric past 2^53-1 round-trips through $n::numeric"):
        for
          _        <- sql"drop table if exists pg_numeric".dml
          _        <- sql"create table pg_numeric (n numeric)".dml
          selected <- sql"select ${pastNumeric}".queryValue[BigDecimal]
          _        <- sql"insert into pg_numeric (n) values (${pastNumeric})".dml
          stored   <- sql"select n from pg_numeric".queryValue[BigDecimal]
        yield assertTrue(
          selected.exists(_.compare(pastNumeric) == 0),
          stored.exists(_.compare(pastNumeric) == 0),
        )
      ,
      test("timestamptz keeps microseconds"):
        for
          bound   <- sql"select ${micros}".queryValue[Instant]
          literal <- sql"select '2024-09-23 15:04:05.123456+00'::timestamptz".queryValue[Instant]
        yield assertTrue(bound.contains(micros), literal.contains(micros))
      ,
      test("bool false is the text f"):
        for
          literal <- sql"select false".queryValue[Boolean]
          bound   <- sql"select ${false}".queryValue[Boolean]
        yield assertTrue(literal.contains(false), bound.contains(false))
      ,
      test("JSON null is jsonb text and SQL null is Null"):
        for
          command <- sql"select 'null'::jsonb, null::jsonb".toCommand
          rows    <- ZIO.serviceWithZIO[SqlSession](_.query(command)(row => Right(row)))
          row = rows.head
        yield assertTrue(
          row.at(0) == Right(SqlValue.Jsonb("null")),
          row.at(1) == Right(SqlValue.Null(PgType.Jsonb)),
        )
      ,
      test("bpchar round-trips padded"):
        for
          _   <- sql"drop table if exists pg_bpchar".dml
          _   <- sql"create table pg_bpchar (v char(4))".dml
          _   <- sql"insert into pg_bpchar (v) values (${"ab"})".dml
          got <- sql"select v from pg_bpchar".queryValue[String]
        yield assertTrue(got.contains("ab  "))
      ,
      test("duplicate labels keep the first cell"):
        for
          command <- sql"select ${1} as a, ${2} as a".toCommand
          rows    <- ZIO.serviceWithZIO[SqlSession](_.query(command)(row => Right(row)))
        yield assertTrue(rows.head.get("a") == Right(SqlValue.Int4(1)))
      ,
      test("an unknown oid fails the cell with the oid"):
        for exit <- sql"select '1 day'::interval".queryValue[String].exit
        yield assertTrue:
          exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(SaferisError.DecodingError(_, _, detail)) => detail.contains("1186")
                case _                                              => false
            case _ => false
      ,
      test("stream emits the buffered rows"):
        val read: SqlRow => Either[SaferisError, Long] = row =>
          summon[RowDecoder[Long]]
            .decode(row)
            .left
            .map(err => SaferisError.DecodingError("value", "Long", err.detail))
        for
          command <- sql"select ${pastInt8}".toCommand
          session <- ZIO.service[SqlSession]
          rows    <- session.stream(command)(read).runCollect
        yield assertTrue(rows == Chunk(pastInt8)),
    )

  private val writes =
    suite("transactions")(
      test("unique violation message is exactly unique violation"):
        for
          _    <- sql"drop table if exists pg_uniq".dml
          _    <- sql"create table pg_uniq (id integer primary key)".dml
          _    <- sql"insert into pg_uniq (id) values (1)".dml
          exit <- sql"insert into pg_uniq (id) values (1)".dml.exit
        yield assertTrue:
          exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(SaferisError.UniqueViolation(_, message, _)) => message == "unique violation"
                case _                                                 => false
            case _ => false
      ,
      test("a failed transact rolls back"):
        for
          _    <- sql"drop table if exists pg_rollback".dml
          _    <- sql"create table pg_rollback (id integer primary key)".dml
          exit <- transact(
            for
              _ <- sql"insert into pg_rollback (id) values (1)".dml
              _ <- ZIO.fail(SaferisError.Unexpected("rollback"))
            yield ()
          ).exit
          count <- sql"select count(*) from pg_rollback".queryValue[Long]
        yield assertTrue(exit.isFailure, count.contains(0L))
      ,
      test("a pool statement timeout is Timeout and is not BEGIN"):
        for
          heard <- Ref.make(Chunk.empty[String])
          exit  <- sql"select pg_sleep(5)"
            .withTimeout(1.second)
            .queryValue[Int]
            .exit
            .provide(
              session(pgConfig(listener = new Recording(heard)))
            )
          sqls <- heard.get
        yield assertTrue(
          isTimeout(exit),
          sqls.exists(_.contains("pg_sleep")),
          sqls.forall(sql => !sql.contains("BEGIN") && !sql.contains("SET LOCAL") && !sql.contains("COMMIT")),
        )
      ,
      test("defaultTimeout cancels a statement inside transact"):
        for exit <- transact(sql"select pg_sleep(5)".queryValue[Int]).exit.provide(
            session(pgConfig(defaultTimeout = Some(1.second)))
          )
        yield assertTrue(isTimeout(exit)),
    )

  private val joined =
    suite("nested transact with defaultTimeout")(
      test("the outer body sees the inner write and a later failure rolls it back"):
        for
          _    <- sql"drop table if exists pg_nested".dml
          _    <- sql"create table pg_nested (id integer primary key)".dml
          seen <- Ref.make[Option[Long]](None)
          exit <- transact(
            for
              _ <- transact(sql"insert into pg_nested (id) values (1)".dml)
              n <- sql"select count(*) from pg_nested".queryValue[Long]
              _ <- seen.set(n)
              _ <- ZIO.fail(SaferisError.Unexpected("still open"))
            yield ()
          ).exit
          captured <- seen.get
          count    <- sql"select count(*) from pg_nested".queryValue[Long]
          outerFailed = exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(SaferisError.Unexpected("still open")) => true
                case _                                           => false
            case _ => false
        yield assertTrue(captured.contains(1L), count.contains(0L), outerFailed)
      ,
      test("catching a statement failure rolls back and the next command is 25P02 and is not sent"):
        for
          heard  <- Ref.make(Chunk.empty[String])
          result <- (
            for
              _    <- sql"drop table if exists pg_abort".dml
              _    <- sql"create table pg_abort (id integer primary key)".dml
              _    <- heard.set(Chunk.empty)
              seen <- Ref.make[Option[Either[SaferisError, Option[Long]]]](None)
              exit <- transact(
                for
                  _     <- sql"insert into pg_abort (id) values (1)".dml
                  _     <- sql"deli meat from pg_abort".dml.catchAll(_ => ZIO.succeed(0L))
                  later <- sql"select count(*) as later_count from pg_abort".queryValue[Long].either
                  _     <- seen.set(Some(later))
                yield ()
              ).exit
              captured <- seen.get
              sqls     <- heard.get
              count    <- sql"select count(*) from pg_abort".queryValue[Long]
            yield (exit, captured, sqls, count)
          ).provide(session(pgConfig(defaultTimeout = Some(30.seconds), listener = new Recording(heard))))
          (exit, captured, sqls, count) = result
          aborted                       = captured match
            case Some(Left(SaferisError.QueryError(Some("25P02"), _, sql))) =>
              sql.exists(_.contains("later_count"))
            case _ => false
          recorded = exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(_: SaferisError.SyntaxError) => true
                case _                                 => false
            case _ => false
          sentLater = sqls.exists(_.contains("later_count"))
        yield assertTrue(aborted, recorded, !sentLater, count.contains(0L)),
    )

  private def onSession[A](config: PgConfig)(use: SqlSession => ZIO[Any, SaferisError, A]): ZIO[Any, SaferisError, A] =
    ZIO.serviceWithZIO[SqlSession](use).provide(session(config))

  private def run[A](db: SqlSession)(f: ZIO[SqlSession, SaferisError, A]): ZIO[Any, SaferisError, A] =
    f.provideEnvironment(ZEnvironment(db))

  private def untilActive(pid: Int): ZIO[SqlSession, SaferisError, Unit] =
    (ZIO.sleep(20.millis) *>
      sql"select count(*)::int from pg_stat_activity where pid = ${pid} and state = 'active'".queryValue[Int])
      .repeatUntil(_.contains(1))
      .timeout(5.seconds)
      .flatMap:
        case Some(_) => ZIO.unit
        case None    => ZIO.fail(SaferisError.Unexpected("backend did not become active"))

  private val review =
    suite("review")(
      test("shutdown sqlstates break the client and a connection-shaped message does not"):
        val shutdown = List("57P01", "57P02", "57P03").map: code =>
          PgErrors.broken(
            SaferisError.QueryError(Some(code), "terminating connection due to administrator command", None)
          )
        val unique    = PgErrors.broken(SaferisError.UniqueViolation(Some("pg_uniq_pkey"), "unique violation", None))
        val mentioned = PgErrors.broken(SaferisError.QueryError(None, "the connection is still usable", None))
        assertTrue(shutdown.forall(identity), !unique, !mentioned)
      ,
      test("a script of two selects fails and one select still returns the row"):
        onSession(pgConfig()): db =>
          for
            script <- run(db)(sql"select 1; select 2".queryValue[Int].exit)
            one    <- run(db)(sql"select 1".queryValue[Int])
          yield assertTrue(script.isFailure, one.contains(1))
      ,
      test("a unique violation keeps the same backend"):
        onSession(pgConfig(poolSize = 1)): db =>
          for
            _    <- run(db)(sql"drop table if exists pg_uniq_keep".dml)
            _    <- run(db)(sql"create table pg_uniq_keep (id integer primary key)".dml)
            pid1 <- run(db)(sql"select pg_backend_pid()".queryValue[Int])
            _    <- run(db)(sql"insert into pg_uniq_keep (id) values (1)".dml)
            exit <- run(db)(sql"insert into pg_uniq_keep (id) values (1)".dml.exit)
            pid2 <- run(db)(sql"select pg_backend_pid()".queryValue[Int])
            kept = exit match
              case Exit.Failure(cause) =>
                cause.failureOption match
                  case Some(SaferisError.UniqueViolation(_, "unique violation", _)) => true
                  case _                                                            => false
              case _ => false
          yield assertTrue(kept, pid1 == pid2, pid1.isDefined)
      ,
      test("interrupting pg_sleep does not leave a transaction for the next checkout"):
        ZIO
          .scoped:
            for
              sleeperEnv <- NodeSession.layer.build.provideSome[Scope](ZLayer.succeed(pgConfig(poolSize = 1)))
              watcherEnv <- NodeSession.layer.build.provideSome[Scope](ZLayer.succeed(pgConfig(poolSize = 1)))
              sleeper = sleeperEnv.get[SqlSession]
              watcher = watcherEnv.get[SqlSession]
              pid   <- run(sleeper)(sql"select pg_backend_pid()".queryValue[Int])
              id    <- ZIO.fromOption(pid).orElseFail(SaferisError.Unexpected("no backend pid"))
              fiber <- run(sleeper)(sql"select pg_sleep(30)".queryValue[Int]).fork
              _     <- run(watcher)(untilActive(id))
              _     <- fiber.interrupt
              tx    <- run(sleeper)(sql"select txid_current_if_assigned()::text".queryValue[Option[String]])
            yield assertTrue(tx.forall(_.isEmpty))
          .timeoutFail(SaferisError.Unexpected("interrupted sleep left the checkout busy"))(8.seconds)
      ,
      test("an interrupted checkout does not hang pool.end"):
        val close =
          ZIO.scoped:
            for
              held <- NodeSession.layer.build.provideSome[Scope](ZLayer.succeed(pgConfig(poolSize = 1)))
              db = held.get[SqlSession]
              hold   <- run(db)(sql"select pg_sleep(30)".queryValue[Int]).fork
              _      <- ZIO.sleep(400.millis)
              second <- run(db)(sql"select 1".queryValue[Int]).fork
              _      <- ZIO.sleep(200.millis)
              _      <- second.interrupt
              _      <- hold.interrupt
            yield ()
        close.timeout(8.seconds).map(done => assertTrue(done.isDefined))
      ,
      test("terminating the backend fails the sleep and the pool still serves"):
        ZIO.scoped:
          for
            sleeperEnv <- NodeSession.layer.build.provideSome[Scope](ZLayer.succeed(pgConfig(poolSize = 1)))
            killerEnv  <- NodeSession.layer.build.provideSome[Scope](ZLayer.succeed(pgConfig(poolSize = 1)))
            sleeper = sleeperEnv.get[SqlSession]
            killer  = killerEnv.get[SqlSession]
            pid   <- run(sleeper)(sql"select pg_backend_pid()".queryValue[Int])
            id    <- ZIO.fromOption(pid).orElseFail(SaferisError.Unexpected("no backend pid"))
            fiber <- run(sleeper)(sql"select pg_sleep(30)".queryValue[Int]).fork
            _     <- run(killer)(untilActive(id))
            _     <- run(killer)(sql"select pg_terminate_backend(${id})".queryValue[Boolean])
            exit  <- fiber.join.exit
            next  <- run(sleeper)(sql"select 1".queryValue[Int])
            failed = exit match
              case Exit.Failure(cause) => cause.failureOption.isDefined
              case _                   => false
          yield assertTrue(failed, next.contains(1)),
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(45.seconds) @@ TestAspect.sequential

  private val live =
    suite("Node pg")(
      reads.provideShared(open),
      writes.provideShared(open),
      joined.provideShared(timed),
      review,
    ) @@ TestAspect.sequential

  def spec =
    env("PGHOST") match
      case None =>
        suite("Node pg")(
          test("skips when PGHOST is unset")(assertTrue(true))
        ) @@ TestAspect.ignore
      case Some(_) => live
end PgSessionSpecs
