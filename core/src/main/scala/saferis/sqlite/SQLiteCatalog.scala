package saferis.sqlite

import saferis.*
import zio.Trace
import zio.ZIO

/** `Schema.verify` on SQLite, through the table-valued pragma functions. Names are bound, never spliced.
  *
  * SQLite does not keep the names of unique or foreign-key constraints: a unique constraint is reported under its
  * automatic index name, and a foreign key as `fk_<id>`. Verification by columns works as everywhere else, but
  * `strictNameMatching` cannot match those names. SQLite has partial indexes, but the predicate is not read back.
  */
private[saferis] object SQLiteCatalog:
  import CatalogRows.*

  def introspect(rawName: String)(using Trace): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
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

  /** `main` unless the name says `schema.table`. */
  private final case class ParsedName(schema: String, table: String)

  private def parsedName(raw: String): ParsedName =
    val dot = raw.lastIndexOf('.')
    if dot < 0 then ParsedName("main", raw)
    else ParsedName(raw.substring(0, dot), raw.substring(dot + 1))

  private def tableQuery(name: ParsedName): SqlFragment =
    sql"""select name as table_name
            from pragma_table_list
           where type = 'table' and schema = ${name.schema} and lower(name) = lower(${name.table})"""

  /** A primary key column is not nullable, whatever SQLite's legacy `notnull` flag says. */
  private def columnQuery(name: ParsedName): SqlFragment =
    sql"""select lower(name) as column_name,
                 lower(type) as data_type,
                 case when "notnull" = 1 or pk > 0 then 'NO' else 'YES' end as is_nullable,
                 dflt_value as column_default,
                 cid + 1 as ordinal_position
            from pragma_table_info(${name.table}, ${name.schema})
           order by cid"""

  private def primaryKeyQuery(name: ParsedName): SqlFragment =
    sql"""select lower(name) as column_name, pk as ordinal_position
            from pragma_table_info(${name.table}, ${name.schema})
           where pk > 0
           order by pk"""

  private def uniqueQuery(name: ParsedName): SqlFragment =
    sql"""select lower(il.name) as constraint_name,
                 lower(ii.name) as column_name,
                 ii.seqno + 1 as ordinal_position
            from pragma_index_list(${name.table}, ${name.schema}) as il
            join pragma_index_info(il.name, ${name.schema}) as ii
           where il.origin = 'u'
           order by il.name, ii.seqno"""

  private def foreignKeyQuery(name: ParsedName): SqlFragment =
    sql"""select 'fk_' || id as constraint_name,
                 lower("from") as from_column,
                 lower("table") as to_table,
                 lower("to") as to_column,
                 on_update as update_rule,
                 on_delete as delete_rule,
                 seq + 1 as ordinal_position
            from pragma_foreign_key_list(${name.table}, ${name.schema})
           order by id, seq"""

  /** Indexes created with `create index`. Unique-constraint and primary-key indexes are reported by the queries above.
    */
  private def indexQuery(name: ParsedName): SqlFragment =
    sql"""select lower(il.name) as index_name,
                 lower(ii.name) as column_name,
                 il."unique" as is_unique,
                 cast(null as text) as where_clause,
                 ii.seqno + 1 as ordinal_position
            from pragma_index_list(${name.table}, ${name.schema}) as il
            join pragma_index_info(il.name, ${name.schema}) as ii
           where il.origin = 'c' and ii.name is not null
           order by il.name, ii.seqno"""
end SQLiteCatalog
