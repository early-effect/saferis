package saferis.pg

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSName

/** Values this driver builds and passes into `pg`. */
private[pg] class PgTypeParsers(
    val getTypeParser: js.Function2[js.Any, js.Any, js.Function1[js.Any, js.Any]]
) extends js.Object

private[pg] class PgSslOptions(
    val rejectUnauthorized: Boolean = false,
    val ca: js.UndefOr[String] = js.undefined,
    val checkServerIdentity: js.UndefOr[js.Function2[js.Any, js.Any, js.UndefOr[js.Any]]] = js.undefined,
) extends js.Object

private[pg] class PgPoolOptions(
    val max: Double,
    val host: js.UndefOr[String] = js.undefined,
    val port: js.UndefOr[Double] = js.undefined,
    val database: js.UndefOr[String] = js.undefined,
    val user: js.UndefOr[String] = js.undefined,
    val password: js.UndefOr[String] = js.undefined,
    val ssl: js.UndefOr[Boolean | PgSslOptions] = js.undefined,
    val connectionTimeoutMillis: js.UndefOr[Double] = js.undefined,
    val options: js.UndefOr[String] = js.undefined,
    val keepAlive: js.UndefOr[Boolean] = js.undefined,
    val keepAliveInitialDelayMillis: js.UndefOr[Double] = js.undefined,
    val allowExitOnIdle: js.UndefOr[Boolean] = js.undefined,
    val types: js.UndefOr[PgTypeParsers] = js.undefined,
) extends js.Object

private[pg] class PgQuery(
    val text: String,
    val values: js.Array[js.Any],
    val rowMode: String,
) extends js.Object

private[pg] class PgCursorOptions(
    val rowMode: String,
    val types: PgTypeParsers,
) extends js.Object

/** A `pg` promise whose rejection is a [[PgDatabaseError]]. `then` is attached inside the interrupt wrapper. */
@js.native
private[pg] trait PgSettled[A] extends js.Object:
  def `then`(
      onFulfilled: js.Function1[A, Unit],
      onRejected: js.Function1[PgDatabaseError, Unit],
  ): Unit = js.native

/** CommonJS `require("pg").Pool`. Compile does not need the npm package installed. */
@js.native
@JSImport("pg", "Pool")
private[pg] class PgPool(@unused config: PgPoolOptions) extends js.Object:
  def connect(): js.Promise[PgClient] = js.native

  @JSName("connect")
  def connectSettled(): PgSettled[PgClient] = js.native

  def end(): js.Promise[Unit] = js.native

  @JSName("on")
  def onConnect(event: "connect", listener: js.Function1[PgClient, Unit]): Unit = js.native

  @JSName("on")
  def onError(event: "error", listener: js.Function1[PgDatabaseError, Unit]): Unit = js.native
end PgPool

@js.native
private[pg] trait PgClient extends js.Object:
  def query(query: PgQuery): js.Promise[PgResult] = js.native

  @JSName("query")
  def querySettled(query: PgQuery): PgSettled[PgResult] = js.native

  /** `pg-cursor` is a Submittable. `query` returns the cursor, not a promise. */
  @JSName("query")
  def submit(cursor: PgCursor): PgCursor = js.native

  def on(event: String, listener: js.Function1[PgDatabaseError, Unit]): Unit = js.native

  /** `true` drops the connection. A SQL error is `false`: the pool may reuse the client. */
  def release(destroy: Boolean): Unit = js.native
end PgClient

@js.native
private[pg] trait PgResult extends js.Object:
  def rows: js.Array[js.Array[js.UndefOr[String]]] = js.native
  def fields: js.Array[PgField]                    = js.native

  /** Command tag. `COMMIT` on an aborted transaction is `ROLLBACK`, with no error. */
  def command: String = js.native

  /** `null` for a statement that does not count rows. Undefined when `pg` omitted it. */
  def rowCount: js.UndefOr[Double | Null] = js.native
end PgResult

@js.native
private[pg] trait PgField extends js.Object:
  def name: String       = js.native
  def dataTypeID: Double = js.native

/** The error object `pg` rejects with. A transport code is `code` and is not a SQLSTATE. */
@js.native
private[pg] trait PgDatabaseError extends js.Object:
  def code: js.UndefOr[String]       = js.native
  def message: js.UndefOr[String]    = js.native
  def constraint: js.UndefOr[String] = js.native

/** `require("pg-cursor")`. `read` pulls one batch. `JSImport.Default` matches both CommonJS and ESModule. */
@js.native
@JSImport("pg-cursor", JSImport.Default)
private[pg] class PgCursor(
    @unused text: String,
    @unused values: js.Array[js.Any],
    @unused config: PgCursorOptions,
) extends js.Object:
  def read(
      rows: Int,
      cb: js.Function3[
        js.UndefOr[PgDatabaseError],
        js.Array[js.Array[js.UndefOr[String]]],
        PgResult,
        Unit,
      ],
  ): Unit = js.native

  def close(cb: js.Function1[js.UndefOr[PgDatabaseError], Unit]): Unit = js.native
end PgCursor
