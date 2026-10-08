package saferis.jdbc

import saferis.*

import zio.Chunk
import zio.test.*

import java.sql.ResultSet
import java.sql.Types
import scala.annotation.unused

/** Proves a label read never calls the adapter for a column the row type did not name. The adapter fails if `noise` is
  * read, and the result set is never touched.
  */
object JdbcReadsSpecs extends ZIOSpecDefault:
  private val noise = ColumnName("noise")

  private object FailOnNoise extends StandardJdbcAdapter:
    override def read(@unused rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue] =
      if column.label.matchesLabel(noise) then Left(SaferisError.Unexpected("read noise"))
      else if column.label.matchesLabel(ColumnName("id")) then Right(SqlValue.Integer(7))
      else Right(SqlValue.VarChar("ada"))

  private val described = Chunk(
    JdbcColumn(1, ColumnName("id"), TypeName("integer"), Types.INTEGER),
    JdbcColumn(2, noise, TypeName("varchar"), Types.VARCHAR),
    JdbcColumn(3, ColumnName("name"), TypeName("varchar"), Types.VARCHAR),
  )

  private val untouched: ResultSet = null

  def spec = suite("JdbcReads.readRow")(
    test("a label read never asks the adapter for an unused column"):
      val read = JdbcReads.readRow(
        FailOnNoise,
        untouched,
        described,
        ResultColumns.Labels(Chunk(ColumnName("ID"), ColumnName("name"))),
      )
      assertTrue(
        read == Right(
          SqlRow(
            Chunk(ColumnName("id"), ColumnName("name")),
            Chunk(SqlValue.Integer(7), SqlValue.VarChar("ada")),
          )
        )
      )
    ,
    test("reading every column reaches the unused one"):
      val read = JdbcReads.readRow(FailOnNoise, untouched, described, ResultColumns.All)
      assertTrue(read == Left(SaferisError.Unexpected("read noise"))),
  )
end JdbcReadsSpecs
