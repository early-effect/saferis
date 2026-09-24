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
  def query(query: js.Object): js.Promise[PgResult]                 = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

  /** `true` drops the connection. A SQL error is `false`: the pool may reuse the client. */
  def release(destroy: Boolean): Unit = js.native

@js.native
private[saferis] trait PgResult extends js.Object:
  def rows: js.Array[js.Array[js.Any]] = js.native
  def fields: js.Array[PgField]        = js.native

@js.native
private[saferis] trait PgField extends js.Object:
  def name: String       = js.native
  def dataTypeID: Double = js.native
