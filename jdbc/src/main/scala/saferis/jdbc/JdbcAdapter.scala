package saferis.jdbc

import saferis.ServerError
import saferis.SaferisError
import saferis.SqlValue

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException

/** How a cursor is read. pgjdbc only honors a fetch size inside a transaction. Other drivers buffer. */
enum CursorStrategy:
  case Buffered
  case Fetch(size: Int, inTransaction: Boolean)

/** What a database does through `java.sql` that the SQL dialect does not. Bind, read, server errors, and cursors. */
trait JdbcAdapter:
  def bind(ps: PreparedStatement, index: Int, value: SqlValue): Unit
  def read(rs: ResultSet, index: Int, typeName: String): Either[SaferisError, SqlValue]
  def serverError(e: SQLException): ServerError

  /** `Some` when the server has already aborted the transaction, before `Connection.commit` is sent. */
  def commitRejected(conn: Connection): Option[ServerError]
  def cursor: CursorStrategy
