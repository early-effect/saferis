package saferis.postgres

import saferis.*
import zio.Trace
import zio.ZIO

private[saferis] object PostgresCatalog:
  import CatalogRows.*

  def introspect(rawName: TableName)(using Trace): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    // One transaction session is one connection. Do not overlap these reads.
    run(tableQuery(rawName))(readTable).flatMap: found =>
      found.headOption match
        case None         => ZIO.succeed(None)
        case Some(stored) =>
          for
            columnRows <- run(columnQuery(rawName))(readColumn)
            keyRows    <- run(primaryKeyQuery(rawName))(readOrdinal)
            uniqueRows <- run(uniqueQuery(rawName))(readConstraint)
            fkRows     <- run(foreignKeyQuery(rawName))(readForeignKey)
            indexRows  <- run(indexQuery(rawName))(readIndex)
          yield Some(CatalogRows.table(stored, columnRows, keyRows, uniqueRows, fkRows, indexRows))

  private final case class ParsedName(schema: Option[String], table: String)

  private def parsedName(raw: String): ParsedName =
    val dot = raw.lastIndexOf('.')
    if dot < 0 then ParsedName(None, raw)
    else ParsedName(Some(raw.substring(0, dot)), raw.substring(dot + 1))

  private def equalsLower(column: String, value: String): SqlFragment =
    SqlFragment.text(s"$column = lower(").append(sql"$value").append(SqlFragment.text(")"))

  private def and(left: SqlFragment, right: SqlFragment): SqlFragment =
    left.append(SqlFragment.text(" and ")).append(right)

  private def infoName(alias: String, raw: String): SqlFragment =
    val parsed    = parsedName(raw)
    val schemaCol = s"$alias.table_schema"
    val tableCol  = s"$alias.table_name"
    parsed.schema match
      case None =>
        and(SqlFragment.text(s"$schemaCol = current_schema()"), equalsLower(tableCol, parsed.table))
      case Some(schema) =>
        and(equalsLower(schemaCol, schema), equalsLower(tableCol, parsed.table))

  private def relationName(raw: String): SqlFragment =
    val parsed = parsedName(raw)
    parsed.schema match
      case None =>
        and(SqlFragment.text("n.nspname = current_schema()"), equalsLower("t.relname::text", parsed.table))
      case Some(schema) =>
        and(equalsLower("n.nspname::text", schema), equalsLower("t.relname::text", parsed.table))

  private def selectWhere(body: String, condition: SqlFragment, orderBy: String = ""): SqlFragment =
    val order = if orderBy.isEmpty then SqlFragment.empty else SqlFragment.text(orderBy)
    SqlFragment.text(body.stripMargin).append(SqlFragment.text(" where ")).append(condition).append(order)

  private def tableQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select t.table_name::text as table_name
        |  from information_schema.tables as t
        """,
      SqlFragment.text("t.table_type = 'BASE TABLE' and ").append(infoName("t", raw)),
    )

  private def columnQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select c.column_name::text as column_name,
        |       c.udt_name::text as data_type,
        |       c.is_nullable::text as is_nullable,
        |       c.column_default::text as column_default,
        |       c.ordinal_position::int as ordinal_position
        |  from information_schema.columns as c
        """,
      infoName("c", raw),
      " order by c.ordinal_position",
    )

  private def primaryKeyQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select kcu.column_name::text as column_name,
        |       kcu.ordinal_position::int as ordinal_position
        |  from information_schema.table_constraints as tc
        |  join information_schema.key_column_usage as kcu
        |    on kcu.constraint_schema = tc.constraint_schema
        |   and kcu.constraint_name = tc.constraint_name
        |   and kcu.table_schema = tc.table_schema
        |   and kcu.table_name = tc.table_name
        """,
      SqlFragment.text("tc.constraint_type = 'PRIMARY KEY' and ").append(infoName("tc", raw)),
      " order by kcu.ordinal_position",
    )

  private def uniqueQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select tc.constraint_name::text as constraint_name,
        |       kcu.column_name::text as column_name,
        |       kcu.ordinal_position::int as ordinal_position
        |  from information_schema.table_constraints as tc
        |  join information_schema.key_column_usage as kcu
        |    on kcu.constraint_schema = tc.constraint_schema
        |   and kcu.constraint_name = tc.constraint_name
        |   and kcu.table_schema = tc.table_schema
        |   and kcu.table_name = tc.table_name
        """,
      SqlFragment.text("tc.constraint_type = 'UNIQUE' and ").append(infoName("tc", raw)),
      " order by tc.constraint_name, kcu.ordinal_position",
    )

  private def foreignKeyQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select con.conname::text as constraint_name,
        |       src_att.attname::text as from_column,
        |       dst.relname::text as to_table,
        |       dst_att.attname::text as to_column,
        |       case con.confupdtype
        |         when 'a' then 'NO ACTION'
        |         when 'r' then 'RESTRICT'
        |         when 'c' then 'CASCADE'
        |         when 'n' then 'SET NULL'
        |         when 'd' then 'SET DEFAULT'
        |       end::text as update_rule,
        |       case con.confdeltype
        |         when 'a' then 'NO ACTION'
        |         when 'r' then 'RESTRICT'
        |         when 'c' then 'CASCADE'
        |         when 'n' then 'SET NULL'
        |         when 'd' then 'SET DEFAULT'
        |       end::text as delete_rule,
        |       cols.ord::int as ordinal_position
        |  from pg_catalog.pg_constraint as con
        |  join pg_catalog.pg_class as t on t.oid = con.conrelid
        |  join pg_catalog.pg_namespace as n on n.oid = t.relnamespace
        |  join pg_catalog.pg_class as dst on dst.oid = con.confrelid
        |  join lateral unnest(con.conkey, con.confkey) with ordinality as cols(src_attnum, dst_attnum, ord)
        |    on true
        |  join pg_catalog.pg_attribute as src_att
        |    on src_att.attrelid = t.oid
        |   and src_att.attnum = cols.src_attnum
        |  join pg_catalog.pg_attribute as dst_att
        |    on dst_att.attrelid = dst.oid
        |   and dst_att.attnum = cols.dst_attnum
        """,
      SqlFragment.text("con.contype = 'f' and ").append(relationName(raw)),
      " order by con.conname, cols.ord",
    )

  private def indexQuery(raw: String): SqlFragment =
    selectWhere(
      """
        |select i.relname::text as index_name,
        |       a.attname::text as column_name,
        |       ix.indisunique as is_unique,
        |       pg_get_expr(ix.indpred, ix.indrelid)::text as where_clause,
        |       cols.ord::int as ordinal_position
        |  from pg_catalog.pg_index as ix
        |  join pg_catalog.pg_class as t on t.oid = ix.indrelid
        |  join pg_catalog.pg_namespace as n on n.oid = t.relnamespace
        |  join pg_catalog.pg_class as i on i.oid = ix.indexrelid
        |  join lateral (
        |        select u.attnum, u.ord
        |          from unnest(string_to_array(ix.indkey::text, ' ')::int[]) with ordinality as u(attnum, ord)
        |         where u.ord <= ix.indnkeyatts
        |           and u.attnum > 0
        |       ) as cols on true
        |  join pg_catalog.pg_attribute as a
        |    on a.attrelid = t.oid
        |   and a.attnum = cols.attnum
        |   and not a.attisdropped
        """,
      SqlFragment.text("not ix.indisprimary and ").append(relationName(raw)),
      " order by i.relname, cols.ord",
    )

end PostgresCatalog
