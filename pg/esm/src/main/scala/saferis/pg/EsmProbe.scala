package saferis.pg

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Linked as an ES module and loaded by Node. A wrong `pg` import fails here, not at link time. */
object EsmProbe:
  @JSExportTopLevel("saferisPgLoaded")
  def loaded(): String =
    val _ = PgWire.cursorConfig
    "pg"

  def main(args: Array[String]): Unit =
    // Reachable so the linker keeps `@JSImport("pg")` and `@JSImport("pg-cursor")`.
    // Not called: constructing a pool would hold the Node event loop.
    if args.contains("--open") then
      val pool   = new PgPool(js.Dynamic.literal("max" -> 1.0))
      val cursor = new PgCursor("select 1", js.Array(), PgWire.cursorConfig)
      val _      = (pool, cursor)
    println(loaded())
end EsmProbe
