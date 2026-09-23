package saferis

import zio.*

/** Package-level utilities for Saferis.
  *
  * Currently exposes the [[queryTimeout]] aspect, which scopes a statement timeout to any Saferis fragments executed
  * inside the decorated effect.
  */
object Saferis:

  /** FiberRef holding the aspect-set query timeout. Read by `SqlFragment` at execution time; written via
    * `Saferis.queryTimeout`'s [[ZIOAspect]].
    */
  private[saferis] val timeoutFiberRef: FiberRef[Option[Duration]] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make[Option[Duration]](None))

  /** ZIO aspect that bounds every Saferis query inside the decorated effect by `d`.
    *
    * Resolution order at execution time (highest priority first):
    *   1. per-fragment `sql"...".withTimeout(d)`
    *   1. this aspect (`@@ Saferis.queryTimeout(d)`)
    *   1. session `defaultTimeout`
    *   1. no timeout
    *
    * The fragment sends the first two as `SqlCommand.timeout`. The driver applies
    * `command.timeout.orElse(defaultTimeout)`.
    *
    * Example:
    * {{{
    * sql"SELECT pg_sleep(5)".queryValue[Int] @@ Saferis.queryTimeout(1.second)
    * }}}
    */
  def queryTimeout(d: Duration): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
      def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        timeoutFiberRef.locally(Some(d))(zio)

end Saferis
