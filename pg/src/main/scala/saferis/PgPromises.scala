package saferis

import zio.Ref
import zio.Task
import zio.Trace
import zio.ZIO

import scala.scalajs.js
import scala.util.control.NonFatal

/** Completes a `pg` promise. `Task` exists only here. Callers map the rejection to `SaferisError`. */
private[saferis] object PgPromises:
  def task[A](busy: Ref[Boolean], thunk: => js.Promise[A])(using Trace): Task[A] =
    ZIO.async[Any, Throwable, A]: register =>
      try
        val promise                     = thunk
        val onOk: js.Function1[A, Unit] =
          (value: A) => register(busy.set(false) *> ZIO.succeed(value))
        val onErr: js.Function1[Any, Unit] =
          (err: Any) => register(busy.set(false) *> ZIO.fail(toThrowable(err)))
        val _ = promise.`then`[Unit](onOk, onErr)
        ()
      catch case NonFatal(t) => register(busy.set(false) *> ZIO.fail(t))

  /** No query is in flight. Used for `pool.connect` and `pool.end`. */
  def idle[A](thunk: => js.Promise[A])(using Trace): Task[A] =
    ZIO.async[Any, Throwable, A]: register =>
      try
        val promise                     = thunk
        val onOk: js.Function1[A, Unit] =
          (value: A) => register(ZIO.succeed(value))
        val onErr: js.Function1[Any, Unit] =
          (err: Any) => register(ZIO.fail(toThrowable(err)))
        val _ = promise.`then`[Unit](onOk, onErr)
        ()
      catch case NonFatal(t) => register(ZIO.fail(t))

  private def toThrowable(err: Any): Throwable =
    err match
      case t: Throwable => t
      case other        => js.JavaScriptException(other.asInstanceOf[js.Any])
end PgPromises
