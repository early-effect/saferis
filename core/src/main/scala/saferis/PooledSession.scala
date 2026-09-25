package saferis

import zio.Chunk
import zio.Duration
import zio.IO
import zio.Ref
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

/** One implementation of nested `transact`, `25P02`, and row decoding. Drivers only move bytes. */
private final class PooledSession(
    checkout: ZIO[Scope, SaferisError, SqlConnection],
    defaultTimeout: Option[Duration],
    lease: Option[PooledSession.Lease],
) extends SqlSession:

  def exec(command: SqlCommand): IO[SaferisError, Long] =
    val cmd = applied(command)
    on(cmd)(_.execute(cmd))

  def query[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): IO[SaferisError, Chunk[A]] =
    val cmd = applied(command)
    on(cmd)(_.query(cmd)).flatMap(rows => ZIO.fromEither(decode(rows, read)))

  def queryAtMostOne[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): IO[SaferisError, Option[A]] =
    val cmd = applied(command)
    on(cmd)(_.queryAtMostOne(cmd)).flatMap:
      case None      => ZIO.succeed(None)
      case Some(row) => ZIO.fromEither(read(row).map(Some(_)))

  def stream[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): ZStream[Any, SaferisError, A] =
    val cmd = applied(command)
    ZStream.unwrapScoped:
      lease match
        case Some(open) =>
          guard(open, cmd).as:
            open.connection
              .cursor(cmd)
              .tapError(err => record(open, err))
              .mapZIO(row => ZIO.fromEither(read(row)))
        case None =>
          checkout.map(connection => connection.cursor(cmd).mapZIO(row => ZIO.fromEither(read(row))))
  end stream

  def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A] =
    lease match
      case Some(_) => body.provideSomeLayer[R](ZLayer.succeed[SqlSession](this))
      case None    =>
        ZIO.scoped:
          for
            connection <- checkout
            // Interrupt skips the exit match. This finalizer is the rollback, not the driver's close.
            finished <- Ref.make(false)
            _        <- ZIO.addFinalizer(finished.get.flatMap(done => ZIO.unless(done)(connection.rollback).unit))
            _        <- connection.begin
            failure  <- Ref.make[Option[SaferisError]](None)
            child = new PooledSession(checkout, defaultTimeout, Some(PooledSession.Lease(connection, failure)))
            exit   <- body.provideSomeLayer[R](ZLayer.succeed[SqlSession](child)).exit
            result <- exit match
              case zio.Exit.Success(value) =>
                failure.get.flatMap:
                  case Some(err) => ZIO.fail(err)
                  case None      =>
                    // Once COMMIT is sent, wait for the answer. A later interrupt must not cut it off.
                    ZIO.uninterruptible(connection.commit *> finished.set(true)).as(value)
              case zio.Exit.Failure(cause) => ZIO.failCause(cause)
          yield result

  private def applied(command: SqlCommand): SqlCommand =
    if command.timeout.isDefined || defaultTimeout.isEmpty then command
    else command.withTimeout(defaultTimeout)

  private def on[A](command: SqlCommand)(use: SqlConnection => IO[SaferisError, A])(using Trace): IO[SaferisError, A] =
    lease match
      case Some(open) =>
        guard(open, command) *> use(open.connection).tapError(err => record(open, err))
      case None =>
        ZIO.scoped(checkout.flatMap(use))

  private def guard(open: PooledSession.Lease, command: SqlCommand): IO[SaferisError, Unit] =
    open.failure.get.flatMap:
      case None    => ZIO.unit
      case Some(_) =>
        ZIO.fail(
          SaferisError.QueryError(
            Some(SqlState.InFailedTransaction),
            "current transaction is aborted, commands ignored until end of transaction block",
            Some(command.inspection),
          )
        )

  /** A decode error did not abort the transaction. The first real failure stays: a later `25P02` does not replace it.
    */
  private def record(open: PooledSession.Lease, err: SaferisError): zio.UIO[Unit] =
    err match
      case SaferisError.DecodingError(_, _, _) => ZIO.unit
      case other                               => open.failure.update(prev => prev.orElse(Some(other)))

  private def decode[A](
      rows: Chunk[SqlRow],
      read: SqlRow => Either[SaferisError, A],
  ): Either[SaferisError, Chunk[A]] =
    rows.foldLeft[Either[SaferisError, Chunk[A]]](Right(Chunk.empty)):
      case (Left(err), _)    => Left(err)
      case (Right(acc), row) => read(row).map(acc :+ _)
end PooledSession

private object PooledSession:
  final case class Lease(connection: SqlConnection, failure: Ref[Option[SaferisError]])
