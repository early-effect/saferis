package saferis

import zio.Duration
import zio.UIO
import zio.ZIO

/** Statement hook. `sql` is placeholder text, never bound values. Not part of `SqlSession`. */
trait SqlListener:
  def executed(event: SqlExecuted): UIO[Unit]

final case class SqlExecuted(
    sql: String,
    duration: Duration,
    timeout: Option[Duration],
    outcome: Either[SaferisError, Long],
)

object SqlListener:
  val noop: SqlListener = _ => ZIO.unit
