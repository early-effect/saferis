package saferis.pg

import saferis.*

import zio.Chunk
import zio.Duration
import zio.durationInt
import zio.IO
import zio.Ref
import zio.Scope
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.util.control.NonFatal

/** Node `pg` connection. The session around it is [[SqlSession.pooled]]. */
object NodeSession:
  private[pg] val Batch = 256

  /** One successful cursor batch. Production leaves this as [[zio.ZIO.unit]]. The stream test counts batches with it.
    */
  private val batchHook = new AtomicReference[UIO[Unit]](ZIO.unit)

  private[pg] def onBatch: UIO[Unit] = batchHook.get()

  private[pg] def setOnBatch(hook: UIO[Unit]): Unit =
    val _ = batchHook.set(hook)

  private val swallow: js.Function1[PgDatabaseError, Unit] = _ => ()

  def layer(using Trace): ZLayer[PgConfig, SaferisError, SqlSession] =
    ZLayer.scoped:
      for
        config <- ZIO.service[PgConfig]
        pool   <- open(config)
        _      <- ZIO.addFinalizer(
          ZIO
            .fromPromiseJS(pool.end())
            .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))
            .ignore
        )
      yield SqlSession.pooled(checkout(pool, config), config.defaultTimeout)

  private def open(config: PgConfig)(using Trace): IO[SaferisError, PgPool] =
    ZIO
      .attempt:
        val pool = new PgPool(PgWire.poolConfig(config))
        pool.onConnect("connect", (client: PgClient) => client.on("error", swallow))
        pool.onError("error", swallow)
        pool
      .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))

  private def checkout(pool: PgPool, config: PgConfig)(using Trace): ZIO[Scope, SaferisError, SqlConnection] =
    ZIO.uninterruptibleMask: restore =>
      restore(connect(pool)).flatMap: client =>
        for
          destroy     <- Ref.make(false)
          busy        <- Ref.make(false)
          began       <- Ref.make(false)
          finished    <- Ref.make(false)
          released    <- Ref.make(false)
          lastTimeout <- Ref.make(Option.empty[Duration])
          lease = new PgLease(client, destroy, busy, began, finished, released)
          _ <- ZIO.addFinalizer(abort(lease))
        yield new NodeConnection(lease, config, lastTimeout)

  private def connect(pool: PgPool)(using Trace): IO[SaferisError, PgClient] =
    awaitSettled(pool.connectSettled(), releaseAbandoned, ZIO.unit).mapError: failure =>
      SaferisError.ConnectionError(failureMessage(failure))

  private def releaseAbandoned(client: PgClient): Unit =
    try client.release(true)
    catch case NonFatal(_) => ()

  private def abort(lease: PgLease)(using Trace): UIO[Unit] =
    lease.busy.get.flatMap: inFlight =>
      if inFlight then release(lease, true)
      else
        lease.began.get
          .zip(lease.finished.get)
          .flatMap: (opened, done) =>
            val rollback =
              if opened && !done then
                ZIO
                  .fromPromiseJS(lease.client.query(PgWire.queryConfig("ROLLBACK", js.Array())))
                  .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))
                  .ignore
              else ZIO.unit
            rollback *> lease.destroy.get.flatMap(drop => release(lease, drop))

  private def release(lease: PgLease, destroy: Boolean): UIO[Unit] =
    lease.released
      .modify:
        case true  => (false, true)
        case false => (true, true)
      .flatMap: should =>
        ZIO.when(should)(ZIO.attempt(lease.client.release(destroy)).ignore).unit

  /** Attach handlers before the promise can settle. `before` runs only while this fiber is still waiting, so an
    * interrupt leaves a query `busy` and a late `connect` is `onLate`.
    */
  private[pg] def awaitSettled[A](
      settled: => PgSettled[A],
      onLate: A => Unit,
      before: UIO[Unit],
  )(using Trace): IO[AwaitFailure, A] =
    ZIO.asyncInterrupt[Any, AwaitFailure, A]: register =>
      var cancelled = false
      try
        val running                     = settled
        val onOk: js.Function1[A, Unit] = value =>
          if cancelled then onLate(value)
          else register(before *> ZIO.succeed(value))
        val onErr: js.Function1[PgDatabaseError, Unit] =
          error => if !cancelled then register(before *> ZIO.fail(AwaitFailure.Rejected(error)))
        running.`then`(onOk, onErr)
        Left(ZIO.succeed { cancelled = true })
      catch
        case NonFatal(t) =>
          register(before *> ZIO.fail(AwaitFailure.Thrown(t)))
          Left(ZIO.unit)
      end try

  private def failureMessage(failure: AwaitFailure): String = failure match
    case AwaitFailure.Rejected(error) => PgErrors.from(error).message
    case AwaitFailure.Thrown(cause)   => PgErrors.message(cause)

end NodeSession

private enum AwaitFailure:
  case Rejected(error: PgDatabaseError)
  case Thrown(cause: Throwable)

private final class PgLease(
    val client: PgClient,
    val destroy: Ref[Boolean],
    val busy: Ref[Boolean],
    val began: Ref[Boolean],
    val finished: Ref[Boolean],
    val released: Ref[Boolean],
)

private final class NodeConnection(
    lease: PgLease,
    config: PgConfig,
    lastTimeout: Ref[Option[Duration]],
) extends SqlConnection:

  def execute(command: SqlCommand): IO[SaferisError, Long] =
    scoped(command)(runExec(command))

  def query(command: SqlCommand, columns: ResultColumns): IO[SaferisError, Chunk[SqlRow]] =
    scoped(command)(runRows(command, columns))

  def queryAtMostOne(command: SqlCommand, columns: ResultColumns): IO[SaferisError, Option[SqlRow]] =
    scoped(command)(runRows(command, columns).map(_.headOption))

  def cursor(command: SqlCommand, columns: ResultColumns): ZStream[Any, SaferisError, SqlRow] =
    ZStream.unwrapScoped:
      for
        inside <- lease.began.get
        _      <- ZIO.unless(inside)(begin)
        _      <- applyTimeout(command)
        portal <- ZIO.attempt(openCursor(command)).mapError(t => SaferisError.Unexpected(PgErrors.message(t)))
        _      <- ZIO.addFinalizer(closeCursor(portal))
      yield
        val pulls = ZStream.repeatZIOChunkOption:
          readBatch(portal, command, columns)
            .mapError(Some(_))
            .flatMap: rows =>
              if rows.isEmpty then ZIO.fail(None) else ZIO.succeed(rows)
        val commit =
          if inside then ZStream.empty
          else ZStream.execute(this.commit)
        pulls ++ commit

  def begin: IO[SaferisError, Unit] =
    lease.began.set(true) *>
      protocol("BEGIN").tapError(err => markBroken(err) *> lease.finished.set(true))

  def commit: IO[SaferisError, Unit] =
    protocolResult("COMMIT").flatMap: result =>
      lease.finished.set(true) *>
        ZIO
          .when(result.command == "ROLLBACK"):
            ZIO.fail(
              SaferisError.Aborted(
                ServerDetail(
                  "commit reported ROLLBACK",
                  Some(SqlText("COMMIT")),
                  None,
                  None,
                  None,
                )
              )
            )
          .unit

  def rollback: UIO[Unit] =
    lease.began.get
      .zip(lease.finished.get)
      .flatMap: (opened, done) =>
        if opened && !done then protocol("ROLLBACK").ignore *> lease.finished.set(true)
        else ZIO.unit

  /** Outside a transaction a timeout needs `BEGIN` so `SET LOCAL` has a scope. Inside one, skip an unchanged cap. */
  private def scoped[A](command: SqlCommand)(body: IO[SaferisError, A]): IO[SaferisError, A] =
    lease.began.get.flatMap: inside =>
      if inside then applyTimeout(command) *> body
      else
        command.timeout match
          case None    => body
          case Some(_) =>
            begin *> applyTimeout(command) *> body.tapError(_ => rollback).flatMap(value => commit.as(value))

  private def applyTimeout(command: SqlCommand): IO[SaferisError, Unit] =
    lastTimeout.get.flatMap: previous =>
      val want = command.timeout
      if previous == want then ZIO.unit
      else
        val statement =
          want match
            case None    => "SET LOCAL statement_timeout TO DEFAULT"
            case Some(d) => s"SET LOCAL statement_timeout = ${PgWire.millis(d)}"
        protocol(statement) *> lastTimeout.set(want)

  private def runExec(command: SqlCommand): IO[SaferisError, Long] =
    queryResult(command).flatMap(result => ZIO.fromEither(PgWire.rowCount(result)))

  private def runRows(command: SqlCommand, columns: ResultColumns): IO[SaferisError, Chunk[SqlRow]] =
    queryResult(command).flatMap(result => ZIO.fromEither(PgWire.readRows(result, columns)))

  private def queryResult(command: SqlCommand): IO[SaferisError, PgResult] =
    val sql    = PgWire.render(command)
    val values = PgWire.parameters(command.pieces)
    promise(Some(sql), lease.client.querySettled(PgWire.queryConfig(sql, values))).flatMap: value =>
      ZIO.fromEither(PgWire.ensureSingle(value))

  private def openCursor(command: SqlCommand): PgCursor =
    val sql    = PgWire.render(command)
    val values = PgWire.parameters(command.pieces)
    lease.client.submit(new PgCursor(sql, values, PgWire.cursorConfig))

  private def readBatch(
      cursor: PgCursor,
      command: SqlCommand,
      columns: ResultColumns,
  ): IO[SaferisError, Chunk[SqlRow]] =
    val sql = command.inspection
    lease.busy.set(true) *>
      ZIO.asyncInterrupt[Any, SaferisError, Chunk[SqlRow]]: register =>
        var cancelled = false
        cursor.read(
          NodeSession.Batch,
          (
              err: js.UndefOr[PgDatabaseError],
              rows: js.Array[js.Array[js.UndefOr[String]]],
              result: PgResult,
          ) =>
            if !cancelled then
              val effect =
                // pg-cursor passes null, not undefined, when the batch succeeded.
                err.toOption.filter(_ != null) match
                  case None =>
                    (ZIO.fromEither(cursorRows(result, rows, columns)) <* NodeSession.onBatch)
                      .ensuring(lease.busy.set(false))
                  case Some(error) =>
                    // The portal is dead. Close would wait for a readyForQuery that already arrived.
                    lease.destroy.set(true) *> lease.busy.set(true) *>
                      ZIO.fail(classify(PgErrors.from(error), Some(sql)))
              register(effect)
            else (),
        )
        Left(ZIO.succeed { cancelled = true })
  end readBatch

  private def cursorRows(
      result: PgResult,
      rows: js.Array[js.Array[js.UndefOr[String]]],
      columns: ResultColumns,
  ): Either[SaferisError, Chunk[SqlRow]] =
    if rows == null || js.isUndefined(rows) || rows.length == 0 then Right(Chunk.empty)
    else
      val fields = result.fields
      (0 until rows.length).foldLeft[Either[SaferisError, Chunk[SqlRow]]](Right(Chunk.empty)):
        case (Left(err), _)  => Left(err)
        case (Right(acc), i) => PgWire.readCursorRow(fields, rows(i), columns).map(acc :+ _)

  /** A missing `readyForQuery` must not pin the uninterruptible finalizer. Two seconds, on the caller clock. */
  private def closeCursor(cursor: PgCursor): UIO[Unit] =
    lease.destroy.get.flatMap: drop =>
      if drop then ZIO.unit
      else
        val close =
          ZIO.asyncInterrupt[Any, Nothing, Unit]: register =>
            var closed = false
            cursor.close: (_: js.UndefOr[PgDatabaseError]) =>
              if !closed then
                closed = true
                register(ZIO.unit)
            Left(ZIO.succeed { closed = true })
        close
          .timeout(2.seconds)
          .interruptible
          .flatMap:
            case Some(_) => ZIO.unit
            case None    => lease.busy.set(true) *> lease.destroy.set(true)

  private def protocol(statement: String): IO[SaferisError, Unit] =
    protocolResult(statement).unit

  private def protocolResult(statement: String): IO[SaferisError, PgResult] =
    promise(None, lease.client.querySettled(PgWire.queryConfig(statement, js.Array()))).flatMap: value =>
      ZIO.fromEither(PgWire.ensureSingle(value))

  private def promise[A](sql: Option[SqlText], settled: => PgSettled[A]): IO[SaferisError, A] =
    lease.busy.set(true).uninterruptible *>
      NodeSession
        .awaitSettled(settled, _ => (), lease.busy.set(false))
        .mapError(failure => failureError(failure, sql))
        .tapError(markBroken)

  private def failureError(failure: AwaitFailure, sql: Option[SqlText]): SaferisError = failure match
    case AwaitFailure.Rejected(error) => classify(PgErrors.from(error), sql)
    case AwaitFailure.Thrown(cause)   => SaferisError.Unexpected(PgErrors.message(cause))

  private def markBroken(err: SaferisError): UIO[Unit] =
    ZIO.when(PgErrors.broken(err))(lease.destroy.set(true)).unit

  private def classify(error: ServerError, sql: Option[SqlText]): SaferisError =
    SqlState.classify(error, sql, config.retry)
end NodeConnection
