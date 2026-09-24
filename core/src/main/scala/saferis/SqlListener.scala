package saferis

import zio.Clock
import zio.Duration
import zio.Exit
import zio.Ref
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.ZLayer
import zio.stream.ZStream

/** What happened to one user statement. Interruption and defects are not database errors. */
enum StatementOutcome:
  case Completed(rows: Long)
  case Failed(error: SaferisError)
  case Interrupted
  case Died

/** Statement hook. `sql` is the `$n` inspection form, never bound values and never a driver's placeholder spelling. */
trait SqlListener:
  def executed(event: SqlExecuted): UIO[Unit]

final case class SqlExecuted(
    sql: String,
    duration: Duration,
    timeout: Option[Duration],
    outcome: StatementOutcome,
)

object SqlListener:
  val noop: SqlListener = _ => ZIO.unit

  /** Reports user commands only. `transact` on the result reports statements inside the transaction too. */
  def observe(listener: SqlListener)(session: SqlSession): SqlSession =
    if listener eq noop then session else new Observed(session, listener)

  def observe(listener: SqlListener): ZLayer[SqlSession, Nothing, SqlSession] =
    ZLayer.fromFunction((session: SqlSession) => observe(listener)(session))

  private def outcomeOf(exit: Exit[Any, Any], rows: Long): StatementOutcome =
    exit match
      case Exit.Success(_)     => StatementOutcome.Completed(rows)
      case Exit.Failure(cause) =>
        cause.failureOption match
          case Some(error: SaferisError)    => StatementOutcome.Failed(error)
          case _ if cause.isInterruptedOnly => StatementOutcome.Interrupted
          case _ if cause.defects.nonEmpty  => StatementOutcome.Died
          case _                            => StatementOutcome.Interrupted

  private final class Observed(inner: SqlSession, listener: SqlListener) extends SqlSession:
    def exec(command: SqlCommand): zio.IO[SaferisError, Long] =
      timed(command, inner.exec(command), identity)

    def query[A](command: SqlCommand)(
        read: SqlRow => Either[SaferisError, A]
    ): zio.IO[SaferisError, zio.Chunk[A]] =
      timed(command, inner.query(command)(read), rows => rows.length.toLong)

    def queryAtMostOne[A](command: SqlCommand)(
        read: SqlRow => Either[SaferisError, A]
    ): zio.IO[SaferisError, Option[A]] =
      timed(command, inner.queryAtMostOne(command)(read), row => if row.isDefined then 1L else 0L)

    def stream[A](command: SqlCommand)(
        read: SqlRow => Either[SaferisError, A]
    ): ZStream[Any, SaferisError, A] =
      ZStream.unwrap:
        for
          start <- Clock.nanoTime
          count <- Ref.make(0L)
        yield inner
          .stream(command)(read)
          .tap(_ => count.update(_ + 1))
          .ensuringWith(exit => report(command, start, count, exit))

    def transact[R, A](body: ZIO[SqlSession & R, SaferisError, A]): ZIO[R, SaferisError, A] =
      inner.transact:
        ZIO.serviceWithZIO[SqlSession]: child =>
          body.provideSomeLayer[R](ZLayer.succeed[SqlSession](Observed(child, listener)))

    private def timed[A](command: SqlCommand, effect: zio.IO[SaferisError, A], rows: A => Long)(using
        Trace
    ): zio.IO[SaferisError, A] =
      for
        start <- Clock.nanoTime
        exit  <- effect.exit
        _     <- reportExit(
          command,
          start,
          exit,
          exit match
            case Exit.Success(value) => rows(value)
            case Exit.Failure(_)     => 0L,
        )
        value <- exit.foldExit(ZIO.failCause, ZIO.succeed)
      yield value

    private def report(command: SqlCommand, start: Long, count: Ref[Long], exit: Exit[Any, Any]): UIO[Unit] =
      count.get.flatMap: rows =>
        reportExit(command, start, exit, rows)

    private def reportExit(command: SqlCommand, start: Long, exit: Exit[Any, Any], rows: Long): UIO[Unit] =
      for
        end <- Clock.nanoTime
        _   <- listener.executed(
          SqlExecuted(
            command.inspection,
            Duration.fromNanos(math.max(0L, end - start)),
            command.timeout,
            outcomeOf(exit, rows),
          )
        )
      yield ()
  end Observed
end SqlListener
