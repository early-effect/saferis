package saferis.pg

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** CommonJS `require("pg").Pool`. Compile does not need the npm package installed. */
@js.native
@JSImport("pg", "Pool")
private[pg] class PgPool(@unused config: js.Object) extends js.Object:
  def connect(): js.Promise[PgClient]                               = js.native
  def end(): js.Promise[Unit]                                       = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

@js.native
private[pg] trait PgClient extends js.Object:
  def query(query: js.Object): js.Promise[PgResult] = js.native

  /** `pg-cursor` is a Submittable. `query` returns the cursor, not a promise. */
  @js.annotation.JSName("query")
  def submit(cursor: PgCursor): PgCursor                            = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

  /** `true` drops the connection. A SQL error is `false`: the pool may reuse the client. */
  def release(destroy: Boolean): Unit = js.native
end PgClient

@js.native
private[pg] trait PgResult extends js.Object:
  def rows: js.Array[js.Array[js.Any]] = js.native
  def fields: js.Array[PgField]        = js.native

  /** Command tag. `COMMIT` on an aborted transaction is `ROLLBACK`, with no error. */
  def command: String = js.native

@js.native
private[pg] trait PgField extends js.Object:
  def name: String       = js.native
  def dataTypeID: Double = js.native

/** `require("pg-cursor")`. `read` pulls one batch. `JSImport.Default` matches both CommonJS and ESModule. */
@js.native
@JSImport("pg-cursor", JSImport.Default)
private[pg] class PgCursor(
    @unused text: String,
    @unused values: js.Array[js.Any],
    @unused config: js.Object,
) extends js.Object:
  def read(rows: Int, cb: js.Function3[js.Any, js.Any, PgResult, Unit]): Unit = js.native
  def close(cb: js.Function1[js.Any, Unit]): Unit                             = js.native
