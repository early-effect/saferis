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

/** One implementation of nested `transact` and row decoding. Drivers only move bytes. A statement error does not poison
  * the transaction here: the server decides whether the next command can run.
  */
private final class PooledSession(
    checkout: ZIO[Scope, SaferisError, SqlConnection],
    defaultTimeout: Option[Duration],
    lease: Option[SqlConnection],
) extends SqlSession:

  def exec(command: SqlCommand): IO[SaferisError, Long] =
    val cmd = applied(command)
    on(_.execute(cmd))

  def query[A](command: SqlCommand)(read: RowRead[A]): IO[SaferisError, Chunk[A]] =
    val cmd = applied(command)
    on(_.query(cmd, read.columns)).flatMap(rows => ZIO.fromEither(decode(rows, read.decode)))

  def queryAtMostOne[A](command: SqlCommand)(read: RowRead[A]): IO[SaferisError, Option[A]] =
    val cmd = applied(command)
    on(_.queryAtMostOne(cmd, read.columns)).flatMap:
      case None      => ZIO.succeed(None)
      case Some(row) => ZIO.fromEither(read.decode(row).map(Some(_)))

  def stream[A](command: SqlCommand)(read: RowRead[A]): ZStream[Any, SaferisError, A] =
    val cmd = applied(command)
    ZStream.unwrapScoped:
      lease match
        case Some(connection) =>
          ZIO.succeed(connection.cursor(cmd, read.columns).mapZIO(row => ZIO.fromEither(read.decode(row))))
        case None =>
          checkout.map: connection =>
            connection.cursor(cmd, read.columns).mapZIO(row => ZIO.fromEither(read.decode(row)))
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
            child = new PooledSession(checkout, defaultTimeout, Some(connection))
            exit   <- body.provideSomeLayer[R](ZLayer.succeed[SqlSession](child)).exit
            result <- exit match
              case zio.Exit.Success(value) =>
                // Once COMMIT is sent, wait for the answer. A later interrupt must not cut it off.
                ZIO.uninterruptible(connection.commit *> finished.set(true)).as(value)
              case zio.Exit.Failure(cause) => ZIO.failCause(cause)
          yield result

  private def applied(command: SqlCommand): SqlCommand =
    if command.timeout.isDefined || defaultTimeout.isEmpty then command
    else command.withTimeout(defaultTimeout)

  private def on[A](use: SqlConnection => IO[SaferisError, A])(using Trace): IO[SaferisError, A] =
    lease match
      case Some(connection) => use(connection)
      case None             => ZIO.scoped(checkout.flatMap(use))

  private def decode[A](
      rows: Chunk[SqlRow],
      read: SqlRow => Either[SaferisError, A],
  ): Either[SaferisError, Chunk[A]] =
    rows.foldLeft[Either[SaferisError, Chunk[A]]](Right(Chunk.empty)):
      case (Left(err), _)    => Left(err)
      case (Right(acc), row) => read(row).map(acc :+ _)
end PooledSession
