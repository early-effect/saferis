package saferis

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** CommonJS `require("pg").Pool`. Compile does not need the npm package installed. */
@js.native
@JSImport("pg", "Pool")
private[saferis] class PgPool(@unused config: js.Object) extends js.Object:
  def connect(): js.Promise[PgClient]                               = js.native
  def end(): js.Promise[Unit]                                       = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

@js.native
private[saferis] trait PgClient extends js.Object:
  def query(query: js.Object): js.Promise[PgResult] = js.native

  /** `pg-query-stream` is a Submittable. `query` returns the stream, not a promise. */
  @js.annotation.JSName("query")
  def submit(query: PgQueryStream): PgQueryStream                   = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

  /** `true` drops the connection. A SQL error is `false`: the pool may reuse the client. */
  def release(destroy: Boolean): Unit = js.native
end PgClient

@js.native
private[saferis] trait PgResult extends js.Object:
  def rows: js.Array[js.Array[js.Any]] = js.native
  def fields: js.Array[PgField]        = js.native

@js.native
private[saferis] trait PgField extends js.Object:
  def name: String       = js.native
  def dataTypeID: Double = js.native

/** CommonJS `require("pg-query-stream")`. A server portal. `batchSize` is the fetch size. */
@js.native
@JSImport("pg-query-stream", JSImport.Namespace)
private[saferis] class PgQueryStream(
    @unused text: String,
    @unused values: js.Array[js.Any],
    @unused config: js.Object,
) extends js.Object:
  def on(event: String, listener: js.Function1[js.Any, Unit]): PgQueryStream = js.native
  def destroy(): PgQueryStream                                               = js.native
