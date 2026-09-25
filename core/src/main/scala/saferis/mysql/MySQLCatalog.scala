package saferis.mysql

import saferis.*
import zio.Trace
import zio.ZIO

/** `Schema.verify` on MySQL, through `information_schema`. Names are bound, never spliced.
  *
  * MySQL column, index, and constraint names are case-insensitive, so the catalog returns them folded to lower case,
  * the same form `Schema.verify` compares. The table is found case-insensitively in the current database (or the
  * database named before the dot), then read by the name MySQL stored.
  */
private[saferis] object MySQLCatalog:
  import CatalogRows.*

  def introspect(rawName: TableName)(using Trace): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    val parsed = parsedName(rawName)
    run(tableQuery(parsed))(readTable).flatMap: found =>
      found.headOption match
        case None         => ZIO.succeed(None)
        case Some(stored) =>
          val table = parsed.copy(table = stored)
          for
            columnRows <- run(columnQuery(table))(readColumn)
            keyRows    <- run(primaryKeyQuery(table))(readOrdinal)
            uniqueRows <- run(uniqueQuery(table))(readConstraint)
            fkRows     <- run(foreignKeyQuery(table))(readForeignKey)
            indexRows  <- run(indexQuery(table))(readIndex)
          yield Some(CatalogRows.table(stored, columnRows, keyRows, uniqueRows, fkRows, indexRows))
  end introspect

  private final case class ParsedName(schema: Option[String], table: String)

  private def parsedName(raw: String): ParsedName =
    val dot = raw.lastIndexOf('.')
    if dot < 0 then ParsedName(None, raw)
    else ParsedName(Some(raw.substring(0, dot)), raw.substring(dot + 1))

  private def schemaIs(alias: String, name: ParsedName): SqlFragment =
    name.schema match
      case None         => SqlFragment.text(s"$alias.table_schema = database()")
      case Some(schema) => SqlFragment.text(s"$alias.table_schema = ").append(sql"$schema")

  /** The stored table name, exactly. */
  private def tableIs(alias: String, name: ParsedName): SqlFragment =
    schemaIs(alias, name).append(SqlFragment.text(s" and $alias.table_name = ")).append(sql"${name.table}")

  private def query(body: String, condition: SqlFragment, orderBy: String = ""): SqlFragment =
    SqlFragment
      .text(body.stripMargin)
      .append(SqlFragment.text(" where "))
      .append(condition)
      .append(SqlFragment.text(orderBy))

  private def tableQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select t.table_name as table_name
        |  from information_schema.tables as t
        """,
      SqlFragment
        .text("t.table_type = 'BASE TABLE' and ")
        .append(schemaIs("t", name))
        .append(SqlFragment.text(" and lower(t.table_name) = lower("))
        .append(sql"${name.table}")
        .append(SqlFragment.text(")")),
    )

  private def columnQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select lower(c.column_name) as column_name,
        |       lower(c.data_type) as data_type,
        |       c.is_nullable as is_nullable,
        |       c.column_default as column_default,
        |       c.ordinal_position as ordinal_position
        |  from information_schema.columns as c
        """,
      tableIs("c", name),
      " order by c.ordinal_position",
    )

  private def primaryKeyQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select lower(k.column_name) as column_name,
        |       k.ordinal_position as ordinal_position
        |  from information_schema.key_column_usage as k
        """,
      SqlFragment.text("k.constraint_name = 'PRIMARY' and ").append(tableIs("k", name)),
      " order by k.ordinal_position",
    )

  private def uniqueQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select lower(tc.constraint_name) as constraint_name,
        |       lower(k.column_name) as column_name,
        |       k.ordinal_position as ordinal_position
        |  from information_schema.table_constraints as tc
        |  join information_schema.key_column_usage as k
        |    on k.constraint_schema = tc.constraint_schema
        |   and k.constraint_name = tc.constraint_name
        |   and k.table_name = tc.table_name
        """,
      SqlFragment.text("tc.constraint_type = 'UNIQUE' and ").append(tableIs("tc", name)),
      " order by tc.constraint_name, k.ordinal_position",
    )

  private def foreignKeyQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select lower(k.constraint_name) as constraint_name,
        |       lower(k.column_name) as from_column,
        |       lower(k.referenced_table_name) as to_table,
        |       lower(k.referenced_column_name) as to_column,
        |       rc.update_rule as update_rule,
        |       rc.delete_rule as delete_rule,
        |       k.ordinal_position as ordinal_position
        |  from information_schema.key_column_usage as k
        |  join information_schema.referential_constraints as rc
        |    on rc.constraint_schema = k.constraint_schema
        |   and rc.constraint_name = k.constraint_name
        |   and rc.table_name = k.table_name
        """,
      SqlFragment.text("k.referenced_table_name is not null and ").append(tableIs("k", name)),
      " order by k.constraint_name, k.ordinal_position",
    )

  /** MySQL has no partial indexes, so there is never a predicate. Expression indexes have no column and are skipped. */
  private def indexQuery(name: ParsedName): SqlFragment =
    query(
      """
        |select lower(s.index_name) as index_name,
        |       lower(s.column_name) as column_name,
        |       (s.non_unique = 0) as is_unique,
        |       cast(null as char) as where_clause,
        |       s.seq_in_index as ordinal_position
        |  from information_schema.statistics as s
        """,
      SqlFragment.text("s.index_name <> 'PRIMARY' and s.column_name is not null and ").append(tableIs("s", name)),
      " order by s.index_name, s.seq_in_index",
    )
end MySQLCatalog
