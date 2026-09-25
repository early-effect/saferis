package saferis.tests

import saferis.*
import saferis.SpecializedDML.*
import saferis.ddl.*
import saferis.dml.*
import saferis.postgres.given
import saferis.tests.DataSourceProvider
import zio.*
import zio.test.*

/** Adversarial tests that attempt real SQL injection through the DDL identifier surfaces flagged by the Semgrep
  * `tainted-sql-string` rule.
  *
  * The premise under test: index/table/column names are interpolated into DDL strings (they cannot be JDBC bind
  * parameters). If that interpolation were unsafe, a malicious identifier containing `"; drop table ...; --` would
  * execute a stacked query and destroy the victim table. Each test creates a victim table, feeds a classic injection
  * payload as an identifier, and asserts the victim table is still intact afterwards.
  *
  * Contract: every public DDL entry point must be injection-safe against caller-supplied identifier strings. Each such
  * API escapes its user-supplied identifiers at the trust boundary via `dialect.escapeIdentifier`:
  *   - `createIndex` / `dropIndex` -> `Dialect.createIndexSql` / `dropIndexSql`, which escape internally.
  *   - `createIndexIfNotExists` -> escapes `indexName`/`columnNames` in the public wrapper before handing them to
  *     `IndexIfNotExistsSupport.createIndexIfNotExistsSql` (that builder is also called internally with schema-derived
  *     labels, which are emitted idiomatically, so escaping lives at the public boundary).
  */
object SqlInjectionDdlSpecs extends ZIOSpecDefault:
  val xaLayer = DataSourceProvider.default

  @tableName("victim_table")
  final case class VictimTable(@key id: Int, name: String) derives Table

  final case class CountResult(count: Long) derives Table

  /** Classic stacked-query payload: if concatenated raw into a DDL string, this closes the current statement and drops
    * the victim table.
    */
  val dropPayload = """x"; drop table victim_table; --"""

  /** Quote-free stacked-query payload. Crucially this needs NO `"` to break out, so it does not rely on the target
    * escaping quotes. If `createIndexIfNotExists` did not escape its index name, this would yield:
    * `create index if not exists idx_x on victim_table (name); drop table victim_table; -- on victim_table (name)` i.e.
    * a real, syntactically valid stacked DROP. This is the payload that would actually succeed if the path were
    * exploitable, so it is the true test.
    */
  val quoteFreeDropPayload = "idx_x on victim_table (name); drop table victim_table; --"

  /** Recreate the victim table fresh and confirm it exists. */
  def freshVictim =
    for
      _ <- (dropTable[VictimTable](ifExists = true))
      _ <- (Schema[VictimTable].ddl().execute)
      _ <- (insert(VictimTable(1, "alice")))
    yield ()

  def victimCount =
    (
      sql"select count(*) as count from information_schema.tables where table_name = ${"victim_table"}"
        .queryOne[CountResult]
      )
      .map(_.map(_.count).getOrElse(0L))

  def spec = suite("SQL injection attempts on DDL identifiers")(
    test("malicious index name via createIndex (escaped path) cannot drop the victim table"):
      for
        _ <- freshVictim
        // Attempt injection through the index name. escapeIdentifier should neutralize it; even if the DB
        // rejects the resulting statement, the victim table must survive.
        _     <- (createIndex[VictimTable](IndexName(dropPayload), Seq(ColumnName("name")))).either
        count <- victimCount
      yield assertTrue(count == 1L) // victim_table still exists regardless of whether the create succeeded/failed
    ,
    test("malicious column name via createIndex (escaped path) cannot drop the victim table"):
      for
        _     <- freshVictim
        _     <- (createIndex[VictimTable](IndexName("idx_safe"), Seq(ColumnName(dropPayload)))).either
        count <- victimCount
      yield assertTrue(count == 1L)
    ,
    test("malicious index name via dropIndex (escaped path) cannot drop the victim table"):
      for
        _     <- freshVictim
        _     <- (dropIndex(IndexName(dropPayload), ifExists = true)).either
        count <- victimCount
      yield assertTrue(count == 1L)
    ,
    test("malicious index name via createIndexIfNotExists cannot drop the victim table"):
      // createIndexIfNotExists escapes its user-supplied index name at the public boundary, so this quote-bearing
      // payload becomes an inert quoted identifier and the victim survives.
      for
        _     <- freshVictim
        _     <- (createIndexIfNotExists[VictimTable](IndexName(dropPayload), Seq(ColumnName("name")))).either
        count <- victimCount
      yield assertTrue(count == 1L)
    ,
    test("malicious column name via createIndexIfNotExists cannot drop the victim table"):
      for
        _     <- freshVictim
        _     <- (createIndexIfNotExists[VictimTable](IndexName("idx_safe"), Seq(ColumnName(dropPayload)))).either
        count <- victimCount
      yield assertTrue(count == 1L)
    ,
    test("QUOTE-FREE stacked-query index name via createIndexIfNotExists cannot drop the victim"):
      // The sharpest test. This payload needs no `"` to escape, so it does not depend on quote-escaping quirks.
      // Without escaping it would produce a syntactically valid stacked statement ending in `drop table
      // victim_table`. Escaping the user-supplied index name at the public boundary makes it inert.
      for
        _     <- freshVictim
        _     <- (createIndexIfNotExists[VictimTable](IndexName(quoteFreeDropPayload), Seq(ColumnName("name")))).either
        count <- victimCount
      yield assertTrue(count == 1L)
    ,
    test("QUOTE-FREE stacked-query index name via createIndex (escaped path) cannot drop the victim"):
      for
        _     <- freshVictim
        _     <- (createIndex[VictimTable](IndexName(quoteFreeDropPayload), Seq(ColumnName("name")))).either
        count <- victimCount
      yield assertTrue(count == 1L),
  ).provideShared(xaLayer) @@ TestAspect.sequential
end SqlInjectionDdlSpecs
