package saferis

import zio.Chunk
import zio.Duration
import zio.IO
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.stream.ZStream

/** The interpreter fragments require. JDBC and later drivers implement this. There is no `run`. */
trait SqlSession:
  def exec(command: SqlCommand): IO[SaferisError, Long]

  def query[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): IO[SaferisError, Chunk[A]]

  /** Execute, decode at most one row, and close. A later row is not read. */
  def queryAtMostOne[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): IO[SaferisError, Option[A]]

  def stream[A](command: SqlCommand)(
      read: SqlRow => Either[SaferisError, A]
  ): ZStream[Any, SaferisError, A]

  /** Run `body` on this transaction. A nested call joins: no second BEGIN and no inner commit. */
  def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A]
end SqlSession

/** One checked-out connection. A driver implements this. It does not join transactions or call listeners. */
trait SqlConnection:
  def execute(command: SqlCommand): IO[SaferisError, Long]
  def query(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]]
  def queryAtMostOne(command: SqlCommand): IO[SaferisError, Option[SqlRow]]
  def cursor(command: SqlCommand): ZStream[Any, SaferisError, SqlRow]
  def begin: IO[SaferisError, Unit]
  def commit: IO[SaferisError, Unit]
  def rollback: UIO[Unit]

object SqlSession:
  /** Transaction joining, the abort flag, and decoding. `checkout` is one connection, closed when the scope ends. */
  def pooled(
      checkout: ZIO[zio.Scope, SaferisError, SqlConnection],
      defaultTimeout: Option[Duration] = None,
  ): SqlSession =
    PooledSession(checkout, defaultTimeout, None)

/** Run `body` on the session in the environment. */
def transact[R, A](
    body: ZIO[SqlSession & R, SaferisError, A]
)(using Trace): ZIO[SqlSession & R, SaferisError, A] =
  ZIO.serviceWithZIO[SqlSession](_.transact(body))
