package saferis

import zio.Ref
import zio.Task
import zio.Trace
import zio.ZIO

import scala.scalajs.js
import scala.util.control.NonFatal

/** Completes a `pg` promise. `Task` exists only here. Callers map the rejection to `SaferisError`.
  *
  * `busy` is cleared only when the promise settles and the fiber is still waiting. `onLate` runs when the value arrives
  * after cancellation, so `connect` can `release(true)` a client this fiber no longer owns.
  */
private[saferis] object PgPromises:
  val swallow: js.Function1[js.Any, Unit] = (_: js.Any) => ()

  def complete[A](busy: Option[Ref[Boolean]], thunk: => js.Promise[A], onLate: A => Unit)(using Trace): Task[A] =
    ZIO.asyncInterrupt[Any, Throwable, A]: register =>
      var cancelled                        = false
      def settle(effect: Task[A]): Task[A] =
        busy match
          case Some(ref) => ref.set(false) *> effect
          case None      => effect
      try
        val promise                     = thunk
        val onOk: js.Function1[A, Unit] = value =>
          if cancelled then onLate(value)
          else register(settle(ZIO.succeed(value)))
        val onErr: js.Function1[Any, Unit] = err => if !cancelled then register(settle(ZIO.fail(toThrowable(err))))
        val _                              = promise.`then`[Unit](onOk, onErr)
        Left(ZIO.succeed { cancelled = true })
      catch
        case NonFatal(t) =>
          register(settle(ZIO.fail(t)))
          Left(ZIO.unit)
      end try

  private def toThrowable(err: Any): Throwable =
    err match
      case t: Throwable => t
      case other        => js.JavaScriptException(other.asInstanceOf[js.Any])
end PgPromises
