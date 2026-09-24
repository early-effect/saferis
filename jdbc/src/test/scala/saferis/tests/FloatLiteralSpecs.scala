package saferis.tests

import saferis.*
import saferis.ddl.*
import saferis.tests.DataSourceProvider
import zio.test.*

object FloatLiteralSpecs extends ZIOSpecDefault:

  @tableName("sql_float_literals")
  final case class SqlFloatLiterals(
      @key id: Int,
      fnan: Float = Float.NaN,
      fpos: Float = Float.PositiveInfinity,
      fneg: Float = Float.NegativeInfinity,
      dnan: Double = Double.NaN,
      dpos: Double = Double.PositiveInfinity,
      dneg: Double = Double.NegativeInfinity,
  ) derives Table

  final case class IndexDef(indexname: String, indexdef: String) derives Table

  private val schema = Schema[SqlFloatLiterals]
    .withIndex(_.fnan)
    .where(_.fnan)
    .eql(Float.NaN)
    .named("idx_fnan")
    .withIndex(_.fpos)
    .where(_.fpos)
    .eql(Float.PositiveInfinity)
    .named("idx_fpos")
    .withIndex(_.fneg)
    .where(_.fneg)
    .eql(Float.NegativeInfinity)
    .named("idx_fneg")
    .withIndex(_.dnan)
    .where(_.dnan)
    .eql(Double.NaN)
    .named("idx_dnan")
    .withIndex(_.dpos)
    .where(_.dpos)
    .eql(Double.PositiveInfinity)
    .named("idx_dpos")
    .withIndex(_.dneg)
    .where(_.dneg)
    .eql(Double.NegativeInfinity)
    .named("idx_dneg")
    .build

  def spec = suite("Float and double SQL literals")(
    test("Postgres accepts NaN and infinity defaults and partial-index predicates") {
      for
        _       <- dropTable[SqlFloatLiterals](ifExists = true)
        _       <- createTable(schema)
        indexes <- sql"""select indexname, indexdef from pg_indexes
                         where tablename = ${"sql_float_literals"}""".query[IndexDef]
        byName = indexes.map(row => row.indexname -> row.indexdef).toMap
      yield assertTrue(
        byName.get("idx_fnan").exists(_.contains("'NaN'::real")),
        byName.get("idx_fpos").exists(_.contains("'Infinity'::real")),
        byName.get("idx_fneg").exists(_.contains("'-Infinity'::real")),
        byName.get("idx_dnan").exists(_.contains("'NaN'::double precision")),
        byName.get("idx_dpos").exists(_.contains("'Infinity'::double precision")),
        byName.get("idx_dneg").exists(_.contains("'-Infinity'::double precision")),
      )
    }
  ).provideShared(DataSourceProvider.default) @@ TestAspect.sequential

end FloatLiteralSpecs
