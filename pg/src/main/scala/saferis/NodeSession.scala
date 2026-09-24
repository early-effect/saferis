package saferis.pg

import saferis.*

import zio.Chunk
import zio.Duration
import zio.durationInt
import zio.FiberRef
import zio.IO
import zio.Ref
import zio.Scope
import zio.Trace
import zio.UIO
import zio.Unsafe
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

import scala.scalajs.js
import scala.util.control.NonFatal

/** Node `pg` connection. The session around it is [[SqlSession.pooled]]. */
object NodeSession:
  /** How many `pg-cursor` `read` calls this fiber has made. A short stream stays at one batch. */
  private[pg] val cursorReads: FiberRef[Int] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(0))

  private[pg] val Batch = 256

  def layer(using Trace): ZLayer[PgConfig, SaferisError, SqlSession] =
    ZLayer.scoped:
      for
        config <- ZIO.service[PgConfig]
        pool   <- open(config)
        _      <- ZIO.addFinalizer(PgPromises.complete(None, pool.end(), _ => ()).ignore)
      yield SqlSession.pooled(checkout(pool, config), config.defaultTimeout)

  private def open(config: PgConfig)(using Trace): IO[SaferisError, PgPool] =
    ZIO
      .attempt:
        val pool = new PgPool(PgWire.poolConfig(config))
        pool.on(
          "connect",
          (client: js.Any) => client.asInstanceOf[PgClient].on("error", PgPromises.swallow),
        )
        pool.on("error", PgPromises.swallow)
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
    PgPromises
      .complete(None, pool.connect(), releaseAbandoned)
      .mapError(t => SaferisError.ConnectionError(PgErrors.message(t)))

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
                PgPromises
                  .complete(None, lease.client.query(PgWire.queryConfig("ROLLBACK", js.Array())), _ => ())
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
end NodeSession

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

  def query(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
    scoped(command)(runRows(command))

  def queryAtMostOne(command: SqlCommand): IO[SaferisError, Option[SqlRow]] =
    scoped(command)(runRows(command).map(_.headOption))

  def cursor(command: SqlCommand): ZStream[Any, SaferisError, SqlRow] =
    ZStream.unwrapScoped:
      for
        inside <- lease.began.get
        _      <- ZIO.unless(inside)(begin)
        _      <- applyTimeout(command)
        portal <- ZIO.attempt(openCursor(command)).mapError(t => SaferisError.Unexpected(PgErrors.message(t)))
        _      <- ZIO.addFinalizer(closeCursor(portal))
      yield
        val pulls = ZStream.repeatZIOChunkOption:
          readBatch(portal, command)
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
              SaferisError.QueryError(
                Some("25P02"),
                "commit reported ROLLBACK",
                Some("COMMIT"),
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

  private def runRows(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
    queryResult(command).flatMap(result => ZIO.fromEither(PgWire.readRows(result)))

  private def queryResult(command: SqlCommand): IO[SaferisError, PgResult] =
    val sql    = PgWire.render(command)
    val values = PgWire.parameters(command.pieces)
    promise(Some(sql), lease.client.query(PgWire.queryConfig(sql, values))).flatMap: value =>
      ZIO.fromEither(PgWire.ensureSingle(value))

  private def openCursor(command: SqlCommand): PgCursor =
    val sql    = PgWire.render(command)
    val values = PgWire.parameters(command.pieces)
    lease.client.submit(new PgCursor(sql, values, PgWire.cursorConfig))

  private def readBatch(cursor: PgCursor, command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
    val sql = command.inspection
    lease.busy.set(true) *>
      ZIO.asyncInterrupt[Any, SaferisError, Chunk[SqlRow]]: register =>
        var cancelled = false
        cursor.read(
          NodeSession.Batch,
          (err: js.Any, rows: js.Any, result: PgResult) =>
            if !cancelled then
              val effect =
                if js.isUndefined(err) || (err eq null) then
                  (ZIO.fromEither(cursorRows(result, rows)) <* NodeSession.cursorReads.update(_ + 1))
                    .ensuring(lease.busy.set(false))
                else
                  // The portal is dead. Close would wait for a readyForQuery that already arrived.
                  lease.destroy.set(true) *> lease.busy.set(true) *>
                    ZIO.fail(classify(PgPromises.asThrowable(err), Some(sql)))
              register(effect)
            else (),
        )
        Left(ZIO.succeed { cancelled = true })
  end readBatch

  private def cursorRows(result: PgResult, raw: js.Any): Either[SaferisError, Chunk[SqlRow]] =
    if raw == null || js.isUndefined(raw) then Right(Chunk.empty)
    else
      val rows = raw.asInstanceOf[js.Array[js.Array[js.Any]]]
      if rows.length == 0 then Right(Chunk.empty)
      else
        val fields = result.fields
        (0 until rows.length).foldLeft[Either[SaferisError, Chunk[SqlRow]]](Right(Chunk.empty)):
          case (Left(err), _)  => Left(err)
          case (Right(acc), i) => PgWire.readCursorRow(fields, rows(i)).map(acc :+ _)

  /** A missing `readyForQuery` must not pin the uninterruptible finalizer. Two seconds, on the caller clock. */
  private def closeCursor(cursor: PgCursor): UIO[Unit] =
    lease.destroy.get.flatMap: drop =>
      if drop then ZIO.unit
      else
        val close =
          ZIO.asyncInterrupt[Any, Nothing, Unit]: register =>
            var closed = false
            cursor.close: (_: js.Any) =>
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
    promise(None, lease.client.query(PgWire.queryConfig(statement, js.Array()))).flatMap: value =>
      ZIO.fromEither(PgWire.ensureSingle(value))

  private def promise[A](sql: Option[String], thunk: => js.Promise[A]): IO[SaferisError, A] =
    lease.busy.set(true).uninterruptible *>
      PgPromises
        .complete(Some(lease.busy), thunk, _ => ())
        .mapError(t => classify(t, sql))
        .tapError(markBroken)

  private def markBroken(err: SaferisError): UIO[Unit] =
    ZIO.when(PgErrors.broken(err))(lease.destroy.set(true)).unit

  private def classify(t: Throwable, sql: Option[String]): SaferisError =
    SqlState.classify(PgErrors.info(t), sql, config.retry)
end NodeConnection
