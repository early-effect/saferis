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

/** One checked-out connection. A driver implements this. It does not join transactions or call listeners.
  *
  * `SqlSession.pooled` depends on the following. The compiler cannot check it:
  *
  *   - `command.timeout` is already resolved. The connection enforces it however it likes.
  *   - `begin`, `commit`, and `rollback` are called only by the session. A `cursor` outside a transaction provides its
  *     own read consistency.
  *   - Releasing the checkout leaves the connection reusable: no open transaction, and no leaked session state
  *     (autocommit, `statement_timeout`, settings applied at checkout).
  *   - `commit` fails when the server rolls the transaction back instead of committing.
  *   - A failure that leaves the connection unusable destroys it rather than returning it to a pool. A statement error
  *     inside an open transaction does not: the session still rolls that transaction back.
  */
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
