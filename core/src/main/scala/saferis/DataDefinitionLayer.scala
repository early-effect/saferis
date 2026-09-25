package saferis

import zio.Trace
import zio.ZIO

val ddl = DataDefinitionLayer // short alias

object DataDefinitionLayer:

  inline def createTable[A](
      ifNotExists: Boolean = true,
      createIndexes: Boolean = true,
  )(using table: Table[A], dialect: Dialect)(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    // Separate key columns for compound key handling
    val keyColumns     = table.columns.filter(_.isKey)
    val hasCompoundKey = keyColumns.length > 1

    val columnDefs = table.columns.map { col =>
      val baseType      = col.columnType
      val notNullClause = if !col.isNullable then " not null" else ""
      val defaultClause = col.defaultClause.map(d => s" $d").getOrElse("")
      val autoIncrement = dialect.autoIncrementClause(col.isGenerated, col.isKey, hasCompoundKey)
      s"${col.label} $baseType$notNullClause$defaultClause$autoIncrement"
    }

    // Add compound primary key constraint if needed
    val primaryKeyConstraint = Option.when(hasCompoundKey) {
      val keyColumnNames = keyColumns.map(_.label)
      dialect.compoundPrimaryKeyClause(keyColumnNames)
    }

    val allConstraints = columnDefs ++ primaryKeyConstraint.toSeq
    val tableName      = table.name
    val createClause   = dialect.createTableClause(ifNotExists)
    val sql            = SqlFragment.text(s"$createClause $tableName (${allConstraints.mkString(", ")})")

    for
      result <- sql.dml
      _      <- if createIndexes then DataDefinitionLayer.createIndexes[A]().unit else ZIO.unit
    yield result
  end createTable

  /** Create a table with foreign key constraints from an Instance.
    *
    * Usage:
    * {{{
    *   import saferis.TableAspects.*
    *
    *   val orders = Table[Order]
    *     @@ foreignKey[Order, Int](_.userId).references[User](_.id).onDelete(Cascade)
    *
    *   createTable(orders)
    * }}}
    */
  def createTable[A](
      instance: Instance[A]
  )(using dialect: Dialect)(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    createTable(instance, ifNotExists = true, createIndexes = true)

  def createTable[A](
      instance: Instance[A],
      ifNotExists: Boolean,
      createIndexes: Boolean,
  )(using dialect: Dialect)(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    // Use instance's columns (dealiased for DDL)
    val cols           = instance.columns.map(_.withTableAlias(None))
    val keyColumns     = cols.filter(_.isKey)
    val hasCompoundKey = keyColumns.length > 1

    val columnDefs = cols.map { col =>
      val baseType      = col.columnType
      val notNullClause = if !col.isNullable then " not null" else ""
      val defaultClause = col.defaultClause.map(d => s" $d").getOrElse("")
      val autoIncrement = dialect.autoIncrementClause(col.isGenerated, col.isKey, hasCompoundKey)
      s"${col.label} $baseType$notNullClause$defaultClause$autoIncrement"
    }

    val primaryKeyConstraint = Option.when(hasCompoundKey) {
      val keyColumnNames = keyColumns.map(_.label)
      dialect.compoundPrimaryKeyClause(keyColumnNames)
    }

    // Add unique constraints from Schema DSL
    val uniqueConstraintsSql = instance.uniqueConstraintsSql

    // Add foreign key constraints from instance
    val foreignKeyConstraints = instance.foreignKeyConstraints

    val allConstraints = columnDefs ++ primaryKeyConstraint.toSeq ++ uniqueConstraintsSql ++ foreignKeyConstraints
    val tableName      = instance.tableName
    val createClause   = dialect.createTableClause(ifNotExists)
    val sql            = SqlFragment.text(s"$createClause $tableName (${allConstraints.mkString(", ")})")

    for
      result <- sql.dml
      _      <- if createIndexes then createIndexesFromInstance(instance).unit else ZIO.unit
    yield result
  end createTable

  /** Create indexes for an Instance from Schema-defined IndexSpecs */
  private def createIndexesFromInstance[A](instance: Instance[A])(using
      dialect: Dialect
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Seq[Long]] =
    val tableName      = instance.tableName
    val cols           = instance.columns.map(_.withTableAlias(None))
    val keyColumns     = cols.filter(_.isKey)
    val hasCompoundKey = keyColumns.length > 1

    // Create indexes from Schema-defined IndexSpecs
    val aspectIndexes = instance.indexes.map { spec =>
      val columnLabels = spec.columns.map(instance.fieldToLabel)
      val createSql    = spec.toCreateSql(tableName, instance.fieldToLabel)
      val sql          = dialect match
        case d: IndexIfNotExistsSupport =>
          val indexName = spec.name.getOrElse(IndexName.default(tableName, columnLabels))
          SqlFragment.text(
            d.createIndexIfNotExistsSql(
              indexName.sql,
              tableName.sql,
              columnLabels.map(_.sql),
              unique = spec.unique,
              where = spec.where,
            )
          )
        case _ => SqlFragment.text(createSql)
      sql.dml
    }

    val compoundKeyIndex = Option.when(hasCompoundKey)(compoundKeyIndexSql(tableName, keyColumns).dml)

    ZIO.collectAll(aspectIndexes ++ compoundKeyIndex.toSeq)
  end createIndexesFromInstance

  private def compoundKeyIndexSql(tableName: TableName, keyColumns: Seq[Column[?]])(using
      dialect: Dialect
  ): SqlFragment =
    val keyColumnNames = keyColumns.map(_.label)
    val indexName      = IndexName.compoundKey(tableName)
    SqlFragment.text:
      dialect match
        case d: IndexIfNotExistsSupport =>
          d.createIndexIfNotExistsSql(indexName.sql, tableName.sql, keyColumnNames.map(_.sql))
        case _ => dialect.createIndexSql(indexName, tableName, keyColumnNames, false)
  end compoundKeyIndexSql

  inline def dropTable[A](ifExists: Boolean = false)(using
      table: Table[A],
      dialect: Dialect,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName = table.name
    val sql       = SqlFragment.text(dialect.dropTableSql(tableName, ifExists))
    sql.dml

  inline def truncateTable[A]()(using
      table: Table[A],
      dialect: Dialect,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName = table.name
    val sql       = SqlFragment.text(dialect.truncateTableSql(tableName))
    sql.dml

  inline def addColumn[A, T](columnName: ColumnName)(using
      table: Table[A],
      encoder: Encoder[T],
      dialect: Dialect,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName  = table.name
    val columnType = encoder.columnType
    val sql        = SqlFragment.text(dialect.addColumnSql(tableName, columnName, columnType))
    sql.dml
  end addColumn

  inline def dropColumn[A](columnName: ColumnName)(using
      table: Table[A],
      dialect: Dialect,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName = table.name
    val sql       = SqlFragment.text(dialect.dropColumnSql(tableName, columnName))
    sql.dml

  inline def createIndex[A](
      indexName: IndexName,
      columnNames: Seq[ColumnName],
      unique: Boolean = false,
      where: Option[SqlText] = None,
  )(using table: Table[A], dialect: Dialect)(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val tableName = table.name
    val sql       =
      if unique then SqlFragment.text(dialect.createUniqueIndexSql(indexName, tableName, columnNames, where = where))
      else SqlFragment.text(dialect.createIndexSql(indexName, tableName, columnNames, where = where))
    sql.dml
  end createIndex

  /** Returns CREATE INDEX SQL for compound key indexes only. Use Instance-based createTable with @@ index aspects for
    * custom indexes.
    */
  inline def createIndexesSql[A]()(using table: Table[A], dialect: Dialect): SqlText =
    val keyColumns = table.columns.filter(_.isKey)
    if keyColumns.length > 1 then compoundKeyIndexSql(table.name, keyColumns).sql else SqlText.empty

  /** Creates compound key indexes only. Use Instance-based createTable with @@ index aspects for custom indexes.
    */
  inline def createIndexes[A]()(using
      table: Table[A],
      dialect: Dialect,
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Seq[Long]] =
    val keyColumns       = table.columns.filter(_.isKey)
    val compoundKeyIndex = Option.when(keyColumns.length > 1)(compoundKeyIndexSql(table.name, keyColumns).dml)
    ZIO.collectAll(compoundKeyIndex.toSeq)
  end createIndexes

  inline def dropIndex(indexName: IndexName, ifExists: Boolean = false)(using
      dialect: Dialect
  )(using trace: Trace): ZIO[SqlSession, SaferisError, Long] =
    val sql = SqlFragment.text(dialect.dropIndexSql(indexName, ifExists))
    sql.dml

end DataDefinitionLayer
