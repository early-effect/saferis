package saferis

import zio.Cause
import zio.Chunk
import zio.Clock
import zio.Duration
import zio.Exit
import zio.IO
import zio.Ref
import zio.Scope
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

import scala.scalajs.js

/** Node `pg` interpreter of `SqlSession`. Promises complete with `ZIO.async`. There is no `ZIO.blocking`. */
object NodeSession:
  def layer(using Trace): ZLayer[PgConfig, SaferisError, SqlSession] =
    ZLayer.scoped:
      for
        config <- ZIO.service[PgConfig]
        pool   <- open(config)
        _      <- ZIO.addFinalizer(PgPromises.idle(pool.end()).ignore)
      yield new NodeSession(pool, config, None)

  private def open(config: PgConfig)(using Trace): IO[SaferisError, PgPool] =
    ZIO.attempt(new PgPool(PgWire.poolConfig(config))).mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))

  private val Aborted =
    "current transaction is aborted, commands ignored until end of transaction block"
end NodeSession

private final class PgTxn(val lease: PgLease, val failure: Ref[Option[SaferisError]])

private final class PgLease(val client: PgClient, val destroy: Ref[Boolean], val busy: Ref[Boolean])

private final class NodeSession(
    pool: PgPool,
    config: PgConfig,
    txn: Option[PgTxn],
) extends SqlSession:

  def defaultTimeout: Option[Duration] = config.defaultTimeout

  def exec(command: SqlCommand): IO[SaferisError, Long] =
    val sql     = PgWire.render(command)
    val timeout = applied(command)
    dispatch(sql, timeout): lease =>
      observed(sql, timeout, runExec(lease, command, sql), identity)

  def query[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): IO[SaferisError, Chunk[A]] =
    val sql     = PgWire.render(command)
    val timeout = applied(command)
    dispatch(sql, timeout): lease =>
      observed(sql, timeout, runQuery(lease, command, sql, read), rows => rows.length.toLong)

  def queryAtMostOne[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): IO[SaferisError, Option[A]] =
    val sql     = PgWire.render(command)
    val timeout = applied(command)
    dispatch(sql, timeout): lease =>
      observed(sql, timeout, runOne(lease, command, sql, read), row => if row.isDefined then 1L else 0L)

  def stream[A](command: SqlCommand)(read: SqlRow => Either[SaferisError, A]): ZStream[Any, SaferisError, A] =
    val sql     = PgWire.render(command)
    val timeout = applied(command)
    ZStream.unwrap:
      dispatch(sql, timeout): lease =>
        observed(sql, timeout, runQuery(lease, command, sql, read), rows => rows.length.toLong).map: rows =>
          ZStream.fromChunk(rows)

  def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A] =
    txn match
      case Some(_) => body.provideSomeLayer[R](ZLayer.succeed[SqlSession](this))
      case None    =>
        ZIO.scoped:
          for
            lease     <- checkout
            _         <- protocol(lease, "BEGIN").tapError(err => markBroken(lease, err))
            failure   <- Ref.make[Option[SaferisError]](None)
            committed <- Ref.make(false)
            _         <- ZIO.addFinalizer(finish(lease, committed))
            child = new NodeSession(pool, config, Some(new PgTxn(lease, failure)))
            exit   <- body.provideSomeLayer[R](ZLayer.succeed[SqlSession](child)).exit
            result <- exit match
              case Exit.Success(value) =>
                failure.get.flatMap:
                  case Some(err) => ZIO.fail(err)
                  case None      =>
                    ZIO.uninterruptible(protocol(lease, "COMMIT") *> committed.set(true)).as(value)
              case Exit.Failure(cause) => ZIO.failCause(cause)
          yield result

  private def dispatch[A](sql: String, timeout: Option[Duration])(
      use: PgLease => IO[SaferisError, A]
  )(using Trace): IO[SaferisError, A] =
    txn match
      case Some(state) =>
        guard(state, sql) *>
          protocol(state.lease, s"SET LOCAL statement_timeout = ${childMillis(timeout)}")
            .tapError(err => record(state, err)) *>
          use(state.lease).tapError(err => record(state, err))
      case None =>
        ZIO.scoped:
          for
            lease  <- checkout
            result <- timeout match
              case None    => use(lease)
              case Some(d) => statementTransaction(lease, d, use)
          yield result

  /** The statement owns the checkout, so `SET LOCAL` needs a transaction that dies with it. */
  private def statementTransaction[A](
      lease: PgLease,
      timeout: Duration,
      use: PgLease => IO[SaferisError, A],
  )(using Trace): IO[SaferisError, A] =
    ZIO.scoped:
      for
        _         <- protocol(lease, "BEGIN").tapError(err => markBroken(lease, err))
        committed <- Ref.make(false)
        _         <- ZIO.addFinalizer(finish(lease, committed))
        _         <- protocol(lease, s"SET LOCAL statement_timeout = ${PgWire.millis(timeout)}")
        a         <- use(lease)
        _         <- ZIO.uninterruptible(protocol(lease, "COMMIT") *> committed.set(true))
      yield a

  /** In-flight query: `release(true)`. The socket close aborts the server transaction. A finished statement rolls back
    * with `ROLLBACK`, and a failed rollback does not replace the original error.
    */
  private def finish(lease: PgLease, committed: Ref[Boolean])(using Trace): UIO[Unit] =
    lease.busy.get.flatMap: inFlight =>
      if inFlight then lease.destroy.set(true)
      else
        committed.get.flatMap: done =>
          if done then ZIO.unit
          else protocol(lease, "ROLLBACK").ignore

  private def runExec(lease: PgLease, command: SqlCommand, sql: String)(using Trace): IO[SaferisError, Long] =
    queryResult(lease, sql, command).map(PgWire.rowCount)

  private def runQuery[A](
      lease: PgLease,
      command: SqlCommand,
      sql: String,
      read: SqlRow => Either[SaferisError, A],
  )(using Trace): IO[SaferisError, Chunk[A]] =
    queryResult(lease, sql, command).flatMap: result =>
      ZIO
        .fromEither(PgWire.readRows(result))
        .flatMap: rows =>
          ZIO.fromEither(decodeRows(rows, read))

  private def runOne[A](
      lease: PgLease,
      command: SqlCommand,
      sql: String,
      read: SqlRow => Either[SaferisError, A],
  )(using Trace): IO[SaferisError, Option[A]] =
    queryResult(lease, sql, command).flatMap: result =>
      ZIO
        .fromEither(PgWire.readFirst(result))
        .flatMap:
          case None      => ZIO.succeed(None)
          case Some(row) =>
            read(row) match
              case Left(err)    => ZIO.fail(err)
              case Right(value) => ZIO.succeed(Some(value))

  private def queryResult(lease: PgLease, sql: String, command: SqlCommand)(using Trace): IO[SaferisError, PgResult] =
    val values = PgWire.parameters(command.pieces)
    promise(lease, Some(sql), lease.client.query(PgWire.queryConfig(sql, values)))

  private def protocol(lease: PgLease, statement: String)(using Trace): IO[SaferisError, Unit] =
    promise(lease, None, lease.client.query(PgWire.queryConfig(statement, js.Array()))).unit

  private def promise[A](lease: PgLease, sql: Option[String], thunk: => js.Promise[A])(using
      Trace
  ): IO[SaferisError, A] =
    lease.busy.set(true).uninterruptible *>
      PgPromises.task(lease.busy, thunk).mapError(t => classify(t, sql)).tapError(err => markBroken(lease, err))

  private def checkout(using Trace): ZIO[Scope, SaferisError, PgLease] =
    for
      destroy <- Ref.make(false)
      busy    <- Ref.make(false)
      client  <- ZIO.acquireRelease(
        PgPromises.idle(pool.connect()).mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))
      )(client => destroy.get.flatMap(drop => ZIO.attempt(client.release(drop)).ignore))
    yield new PgLease(client, destroy, busy)

  private def observed[A](
      sql: String,
      timeout: Option[Duration],
      effect: IO[SaferisError, A],
      rows: A => Long,
  )(using Trace): IO[SaferisError, A] =
    for
      start <- Clock.nanoTime
      exit  <- effect.exit
      end   <- Clock.nanoTime
      outcome = exit match
        case Exit.Success(value) => Right(rows(value))
        case Exit.Failure(cause) => Left(failureOf(cause))
      _ <- config.listener.executed(
        SqlExecuted(sql, Duration.fromNanos(math.max(0L, end - start)), timeout, outcome)
      )
      value <- exit.foldExit(ZIO.failCause, ZIO.succeed)
    yield value

  private def failureOf(cause: Cause[Any]): SaferisError =
    cause.failureOption match
      case Some(err: SaferisError) => err
      case _                       => SaferisError.Unexpected("interrupted")

  private def guard(state: PgTxn, sql: String)(using Trace): IO[SaferisError, Unit] =
    state.failure.get.flatMap:
      case None    => ZIO.unit
      case Some(_) => ZIO.fail(SaferisError.QueryError(Some("25P02"), NodeSession.Aborted, Some(sql)))

  private def record(state: PgTxn, err: SaferisError)(using Trace): UIO[Unit] =
    err match
      case SaferisError.QueryError(Some("25P02"), _, _) => ZIO.unit
      case SaferisError.DecodingError(_, _, _)          => ZIO.unit
      case other                                        => state.failure.update(prev => prev.orElse(Some(other)))

  private def markBroken(lease: PgLease, err: SaferisError)(using Trace): UIO[Unit] =
    ZIO.when(PgErrors.broken(err))(lease.destroy.set(true)).unit

  private def classify(t: Throwable, sql: Option[String]): SaferisError =
    val info     = PgErrors.info(t)
    val timedOut = info.code.contains("57014")
    val vendor   = !timedOut && config.retry(info)
    SqlState.classify(info.code, info.message, info.constraint, sql, vendor, timedOut)

  private def applied(command: SqlCommand): Option[Duration] =
    command.timeout.orElse(config.defaultTimeout)

  /** Absence on a child clears the previous cap. `0` is no timeout. A present cap never becomes `0`. */
  private def childMillis(timeout: Option[Duration]): Int =
    timeout.fold(0)(PgWire.millis)

  private def decodeRows[A](
      rows: Chunk[SqlRow],
      read: SqlRow => Either[SaferisError, A],
  ): Either[SaferisError, Chunk[A]] =
    rows.foldLeft[Either[SaferisError, Chunk[A]]](Right(Chunk.empty)):
      case (Left(err), _)    => Left(err)
      case (Right(acc), row) => read(row).map(acc :+ _)
end NodeSession
