package saferis.jdbc

import saferis.ColumnName
import saferis.ServerError
import saferis.SaferisError
import saferis.SqlValue
import saferis.TypeName

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException

/** How a cursor asks the driver for rows. */
enum CursorStrategy:
  /** Leave the driver's fetch size alone. Rows are still pulled one at a time, but the driver may buffer them all. */
  case Buffered

  /** `setFetchSize(size)` wherever the statement runs. */
  case Fetch(size: Int)

  /** `setFetchSize(size)`, and a pool cursor opens a read transaction. pgjdbc only honors a fetch size with autocommit
    * off.
    */
  case FetchInTransaction(size: Int)
end CursorStrategy

/** One result column as the driver describes it. `typeName` is lower case. `jdbcType` is a `java.sql.Types` code. */
final case class JdbcColumn(index: Int, label: ColumnName, typeName: TypeName, jdbcType: Int)

/** What a database does through `java.sql` that the SQL dialect does not: bind, read, server errors, and cursors.
  *
  * Implement this to bring a database Saferis does not ship. [[StandardJdbcAdapter]] covers what `java.sql` specifies,
  * so most adapters override only the types and errors their driver treats differently.
  */
trait JdbcAdapter:
  /** Bind one parameter. A value the database cannot take (an array on a database without arrays) is a `Left`. */
  def bind(ps: PreparedStatement, index: Int, value: SqlValue): Either[SaferisError, Unit]

  def read(rs: ResultSet, column: JdbcColumn): Either[SaferisError, SqlValue]

  /** The fields `SqlState.classify` reads. Normalize vendor codes to these SQLSTATEs so every database classifies the
    * same way: `23505` unique, `23503` foreign key, `23502` not null, `23514` check, `40P01` deadlock, `40001`
    * serialization failure, `57014` canceled, `08xxx` connection, `42xxx` syntax. Keep the vendor code on `vendorCode`.
    */
  def serverError(e: SQLException): ServerError

  /** `Some` when the server has already aborted the transaction, before `Connection.commit` is sent. */
  def commitRejected(conn: Connection): Option[ServerError] = None

  def cursor: CursorStrategy = CursorStrategy.Buffered
end JdbcAdapter
