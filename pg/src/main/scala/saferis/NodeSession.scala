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
import scala.scalajs.js.timers.setTimeout
import scala.util.control.NonFatal

/** Node `pg` interpreter of `SqlSession`. Promises complete with `ZIO.async`. There is no `ZIO.blocking`. */
object NodeSession:
  def layer(using Trace): ZLayer[PgConfig, SaferisError, SqlSession] =
    ZLayer.scoped:
      for
        config <- ZIO.service[PgConfig]
        pool   <- open(config)
        _      <- ZIO.addFinalizer(PgPromises.complete(None, pool.end(), _ => ()).ignore)
      yield new NodeSession(pool, config, None)

  private def open(config: PgConfig)(using Trace): IO[SaferisError, PgPool] =
    ZIO
      .attempt:
        val pool = new PgPool(PgWire.poolConfig(config))
        pool.on("error", PgPromises.swallow)
        pool
      .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))

  private val Aborted =
    "current transaction is aborted, commands ignored until end of transaction block"
end NodeSession

private final class PgTxn(val lease: PgLease, val failure: Ref[Option[SaferisError]])

private final class PgLease(
    val client: PgClient,
    val destroy: Ref[Boolean],
    val busy: Ref[Boolean],
    val began: Ref[Boolean],
    val committed: Ref[Boolean],
    val released: Ref[Boolean],
)

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
    ZStream.unwrapScoped:
      for
        start <- Clock.nanoTime
        count <- Ref.make(0L)
        lease <- acquireCursor(sql, timeout)
        _     <- lease.busy.set(true)
      yield cursorRows(lease, sql, command)
        .mapZIO: row =>
          read(row) match
            case Left(err)    => ZIO.fail(err)
            case Right(value) => count.update(_ + 1).as(value)
        .ensuringWith: exit =>
          finishStream(lease, sql, timeout, start, count, exit)
  end stream

  def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A] =
    txn match
      case Some(_) => body.provideSomeLayer[R](ZLayer.succeed[SqlSession](this))
      case None    =>
        ZIO.scoped:
          for
            lease   <- checkout
            _       <- lease.began.set(true)
            _       <- protocol(lease, "BEGIN").tapError(err => markBroken(lease, err))
            failure <- Ref.make[Option[SaferisError]](None)
            child = new NodeSession(pool, config, Some(new PgTxn(lease, failure)))
            exit   <- body.provideSomeLayer[R](ZLayer.succeed[SqlSession](child)).exit
            result <- exit match
              case Exit.Success(value) =>
                failure.get.flatMap:
                  case Some(err) => ZIO.fail(err)
                  case None      =>
                    ZIO.uninterruptible(protocol(lease, "COMMIT") *> lease.committed.set(true)).as(value)
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

  /** The statement owns the checkout, so `SET LOCAL` needs a transaction that dies with it. The checkout finalizer is
    * already installed.
    */
  private def statementTransaction[A](
      lease: PgLease,
      timeout: Duration,
      use: PgLease => IO[SaferisError, A],
  )(using Trace): IO[SaferisError, A] =
    for
      _ <- lease.began.set(true)
      _ <- protocol(lease, "BEGIN").tapError(err => markBroken(lease, err))
      _ <- protocol(lease, s"SET LOCAL statement_timeout = ${PgWire.millis(timeout)}")
      a <- use(lease)
      _ <- ZIO.uninterruptible(protocol(lease, "COMMIT") *> lease.committed.set(true))
    yield a

  private def runExec(lease: PgLease, command: SqlCommand, sql: String)(using Trace): IO[SaferisError, Long] =
    queryResult(lease, sql, command).flatMap(result => ZIO.fromEither(PgWire.rowCount(result)))

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
        .fromEither(PgWire.readRows(result).map(_.headOption))
        .flatMap:
          case None      => ZIO.succeed(None)
          case Some(row) =>
            read(row) match
              case Left(err)    => ZIO.fail(err)
              case Right(value) => ZIO.succeed(Some(value))

  private def queryResult(lease: PgLease, sql: String, command: SqlCommand)(using Trace): IO[SaferisError, PgResult] =
    val values = PgWire.parameters(command.pieces)
    promise(lease, Some(sql), lease.client.query(PgWire.queryConfig(sql, values))).flatMap: value =>
      ZIO.fromEither(PgWire.ensureSingle(value))

  private def protocol(lease: PgLease, statement: String)(using Trace): IO[SaferisError, Unit] =
    promise(lease, None, lease.client.query(PgWire.queryConfig(statement, js.Array()))).unit

  private def promise[A](lease: PgLease, sql: Option[String], thunk: => js.Promise[A])(using
      Trace
  ): IO[SaferisError, A] =
    lease.busy.set(true).uninterruptible *>
      PgPromises
        .complete(Some(lease.busy), thunk, _ => ())
        .mapError(t => classify(t, sql))
        .tapError(err => markBroken(lease, err))

  /** The abort finalizer is installed before the client is returned, so `BEGIN` and every user statement, timed or not,
    * already have it. In flight: `release(true)`. An open transaction rolls back, and that failure is ignored.
    */
  private def checkout(using Trace): ZIO[Scope, SaferisError, PgLease] =
    ZIO.uninterruptibleMask: restore =>
      restore(connect).flatMap: client =>
        for
          destroy   <- Ref.make(false)
          busy      <- Ref.make(false)
          began     <- Ref.make(false)
          committed <- Ref.make(false)
          released  <- Ref.make(false)
          _         <- ZIO.attempt(arm(client)).ignore
          lease = new PgLease(client, destroy, busy, began, committed, released)
          _ <- ZIO.addFinalizer(abort(lease))
        yield lease

  private def connect(using Trace): IO[SaferisError, PgClient] =
    PgPromises
      .complete(None, pool.connect(), releaseAbandoned)
      .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))

  /** One listener per client. A reused checkout must not stack another `'error'` handler. */
  private def arm(client: PgClient): Unit =
    val dyn  = client.asInstanceOf[js.Dynamic]
    val mark = dyn.selectDynamic("__saferisErrorListener")
    if js.isUndefined(mark) then
      dyn.updateDynamic("__saferisErrorListener")(true)
      client.on("error", PgPromises.swallow)

  /** Client arrived after this fiber was gone. One `release(true)`. The success path never calls this. */
  private def releaseAbandoned(client: PgClient): Unit =
    try arm(client)
    catch case NonFatal(_) => ()
    try client.release(true)
    catch case NonFatal(_) => ()

  private def abort(lease: PgLease)(using Trace): UIO[Unit] =
    lease.busy.get.flatMap: inFlight =>
      if inFlight then release(lease, true)
      else
        lease.began.get
          .zip(lease.committed.get)
          .flatMap: (opened, done) =>
            val rollback =
              if opened && !done then protocol(lease, "ROLLBACK").ignore
              else ZIO.unit
            rollback *> lease.destroy.get.flatMap(drop => release(lease, drop))

  private def release(lease: PgLease, destroy: Boolean): UIO[Unit] =
    lease.released
      .modify:
        case true  => (false, true)
        case false => (true, true)
      .flatMap: should =>
        ZIO.when(should)(ZIO.attempt(lease.client.release(destroy)).ignore).unit

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
        case Exit.Success(value) => StatementOutcome.Completed(rows(value))
        case Exit.Failure(cause) =>
          if cause.isInterruptedOnly then StatementOutcome.Interrupted
          else
            cause.failureOption match
              case Some(err: SaferisError) => StatementOutcome.Failed(err)
              case _                       => StatementOutcome.Died
      _ <- config.listener.executed(
        SqlExecuted(sql, Duration.fromNanos(math.max(0L, end - start)), timeout, outcome)
      )
      value <- exit.foldExit(ZIO.failCause, ZIO.succeed)
    yield value

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
    val info  = PgErrors.info(t)
    val error =
      if info.sqlState.contains("57014") then info
      else info
    SqlState.classify(error, sql, config.retry)

  private def applied(command: SqlCommand): Option[Duration] =
    command.timeout.orElse(config.defaultTimeout)

  /** Absence on a child clears the previous cap. `0` is no timeout. A present cap never becomes `0`. */
  private def childMillis(timeout: Option[Duration]): Int =
    timeout.fold(0)(PgWire.millis)

  /** Pool streams open a transaction so the portal keeps one snapshot, matching the JDBC read transaction. A child uses
    * the transaction it already has.
    */
  private def acquireCursor(sql: String, timeout: Option[Duration])(using Trace): ZIO[Scope, SaferisError, PgLease] =
    txn match
      case Some(state) =>
        guard(state, sql) *>
          protocol(state.lease, s"SET LOCAL statement_timeout = ${childMillis(timeout)}")
            .tapError(err => record(state, err))
            .as(state.lease)
      case None =>
        for
          lease <- checkout
          _     <- protocol(lease, "BEGIN").tapError(err => markBroken(lease, err))
          _     <- lease.began.set(true)
          _ <- ZIO.foreachDiscard(timeout)(d => protocol(lease, s"SET LOCAL statement_timeout = ${PgWire.millis(d)}"))
        yield lease

  private def cursorRows(lease: PgLease, sql: String, command: SqlCommand)(using
      Trace
  ): ZStream[Any, SaferisError, SqlRow] =
    ZStream.asyncInterrupt[Any, SaferisError, SqlRow] { emit =>
      val query   = new PgQueryStream(sql, PgWire.parameters(command.pieces), PgWire.cursorConfig)
      val opened  = lease.client.submit(query)
      var closed  = false
      var settled = false
      var ended   = false
      def stop(effect: UIO[Unit], err: SaferisError): Unit =
        if !settled then
          settled = true
          emit(effect *> ZIO.fail(Some(err)))
      def note(err: SaferisError): UIO[Unit] =
        markBroken(lease, err) *> txn.fold(ZIO.unit)(state => record(state, err))
      opened.on(
        "data",
        (raw: js.Any) =>
          later:
            if !settled then
              val fields = cursorFields(opened)
              val row    = raw.asInstanceOf[js.Array[js.Any]]
              PgWire.readCursorRow(fields, row) match
                case Left(err)    => stop(note(err), err)
                case Right(value) => emit.single(value),
      )
      opened.on(
        "end",
        (_: js.Any) =>
          later:
            if !settled then
              settled = true
              ended = true
              emit.end,
      )
      opened.on(
        "error",
        (raw: js.Any) =>
          later:
            val err = classify(PgPromises.asThrowable(raw), Some(sql))
            stop(note(err), err),
      )
      Left:
        ZIO
          .succeed:
            val done = ended
            if !closed then
              closed = true
              if !done then
                val _ = opened.destroy()
            done
          .flatMap: done =>
            if done then lease.busy.set(false) else lease.busy.set(true)
    }

  /** `ZStream.asyncInterrupt` runs the callback with `runtime.unsafe.run`. A portal emits rows on the same turn, so
    * delivering them inline re-enters that run and the runtime never reaches `end`.
    */
  private def later(body: => Unit): Unit =
    val _ = setTimeout(0)(body)

  private def cursorFields(query: PgQueryStream): js.Array[PgField] =
    val result = query.asInstanceOf[js.Dynamic].selectDynamic("cursor").selectDynamic("_result")
    result.selectDynamic("fields").asInstanceOf[js.Array[PgField]]

  /** Runs after the portal finalizer. A finished pool stream commits, and that commit is interruptible under the live
    * clock so a stuck socket cannot pin the scope. Anything else leaves the checkout finalizer to drop or roll back the
    * client. The listener runs either way.
    */
  private def finishStream(
      lease: PgLease,
      sql: String,
      timeout: Option[Duration],
      start: Long,
      count: Ref[Long],
      exit: Exit[Any, Any],
  )(using Trace): UIO[Unit] =
    for
      inFlight <- lease.busy.get
      outcome  <-
        if txn.isDefined || inFlight || !exit.isSuccess then streamOutcome(count, exit)
        else
          protocol(lease, "COMMIT").interruptible.foldZIO(
            err => ZIO.succeed(StatementOutcome.Failed(err)),
            _ => lease.committed.set(true) *> count.get.map(n => StatementOutcome.Completed(n)),
          )
      _ <- reportCursor(sql, timeout, start, outcome)
    yield ()

  private def streamOutcome(count: Ref[Long], exit: Exit[Any, Any])(using Trace): UIO[StatementOutcome] =
    exit match
      case Exit.Success(_)     => count.get.map(StatementOutcome.Completed(_))
      case Exit.Failure(cause) =>
        ZIO.succeed:
          if cause.isInterruptedOnly then StatementOutcome.Interrupted
          else
            cause.failureOption match
              case Some(err: SaferisError) => StatementOutcome.Failed(err)
              case _                       => StatementOutcome.Died

  private def reportCursor(
      sql: String,
      timeout: Option[Duration],
      start: Long,
      outcome: StatementOutcome,
  )(using Trace): UIO[Unit] =
    Clock.nanoTime.flatMap: end =>
      config.listener.executed(
        SqlExecuted(sql, Duration.fromNanos(math.max(0L, end - start)), timeout, outcome)
      )

  private def decodeRows[A](
      rows: Chunk[SqlRow],
      read: SqlRow => Either[SaferisError, A],
  ): Either[SaferisError, Chunk[A]] =
    rows.foldLeft[Either[SaferisError, Chunk[A]]](Right(Chunk.empty)):
      case (Left(err), _)    => Left(err)
      case (Right(acc), row) => read(row).map(acc :+ _)
end NodeSession
