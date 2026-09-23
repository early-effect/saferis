package saferis

import zio.*

import scala.collection.mutable.ListBuffer

/** Schema introspection and validation.
  *
  * JDBC metadata is reached only through `JdbcMetadataProbe`, implemented by `JdbcSession`. Dialects can implement
  * SchemaIntrospectionSupport for richer metadata.
  */
object SchemaIntrospection:

  /** Introspect a table's schema from the database. */
  def introspect(tableName: String)(using
      dialect: Dialect
  )(using
      Trace
  ): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    dialect match
      case d: SchemaIntrospectionSupport => d.introspectTable(tableName)
      case _                             =>
        ZIO.serviceWithZIO[SqlSession]:
          case probe: JdbcMetadataProbe => probe.introspect(tableName)
          case _                        =>
            ZIO.fail(SaferisError.Unsupported(s"${dialect.name} schema introspection requires a JDBC session"))

  /** Verify a schema against the database using default options. */
  def verify[A](instance: Instance[A])(using
      dialect: Dialect
  )(using
      Trace
  ): ZIO[SqlSession, SaferisError, Unit] =
    verifyWith(instance, VerifyOptions.default)

  /** Verify a schema against the database with custom options. */
  def verifyWith[A](instance: Instance[A], options: VerifyOptions)(using
      dialect: Dialect
  )(using
      Trace
  ): ZIO[SqlSession, SaferisError, Unit] =
    for
      dbTableOpt <- introspect(instance.tableName)
      issues = dbTableOpt match
        case None          => List(SchemaIssue.TableNotFound(instance.tableName))
        case Some(dbTable) => compare(instance, dbTable, options)
      _ <- ZIO.when(issues.nonEmpty)(ZIO.fail(SaferisError.SchemaValidation(issues)))
    yield ()

  /** Compare expected schema against actual database table. */
  private def compare[A](
      instance: Instance[A],
      dbTable: DatabaseTable,
      options: VerifyOptions,
  )(using dialect: Dialect): List[SchemaIssue] =
    val tableName = instance.tableName
    val issues    = ListBuffer.empty[SchemaIssue]

    // Check each expected column
    instance.columns.foreach { col =>
      val columnLabel = col.label
      dbTable.columns.find(_.name.equalsIgnoreCase(columnLabel)) match
        case None =>
          issues += SchemaIssue.MissingColumn(tableName, columnLabel, col.columnType)
        case Some(dbCol) =>
          if options.checkTypes then
            if !isTypeCompatible(col.columnType, dbCol.dataType, options.strictTypeMatching) then
              issues += SchemaIssue.TypeMismatch(tableName, columnLabel, col.columnType, dbCol.dataType)
          if options.checkNullability then
            if col.isNullable != dbCol.isNullable then
              issues += SchemaIssue.NullabilityMismatch(tableName, columnLabel, col.isNullable, dbCol.isNullable)
      end match
    }

    // Check primary key
    val expectedKeys = instance.columns.filter(_.isKey).map(_.label).sorted
    if expectedKeys.nonEmpty && expectedKeys != dbTable.primaryKeyColumns.map(_.toLowerCase).sorted.map { k =>
        instance.columns.find(_.label.equalsIgnoreCase(k)).map(_.label).getOrElse(k)
      }
    then
      val actualKeys = dbTable.primaryKeyColumns
      if expectedKeys.map(_.toLowerCase).sorted != actualKeys.map(_.toLowerCase).sorted then
        issues += SchemaIssue.PrimaryKeyMismatch(tableName, expectedKeys, actualKeys)

    // Check for extra columns
    if options.checkExtraColumns then
      val expectedColumnNames = instance.columns.map(_.label.toLowerCase).toSet
      dbTable.columns.foreach { dbCol =>
        if !expectedColumnNames.contains(dbCol.name.toLowerCase) then
          issues += SchemaIssue.ExtraColumn(tableName, dbCol.name, dbCol.dataType)
      }

    // Check indexes
    if options.checkIndexes then
      instance.indexes.foreach { indexSpec =>
        val columnLabels = indexSpec.columns.map(instance.fieldToLabel)
        val expectedName = indexSpec.name
        findMatchingIndex(dbTable.indexes, columnLabels, indexSpec.unique) match
          case None =>
            issues += SchemaIssue.MissingIndex(tableName, expectedName, columnLabels, indexSpec.unique)
          case Some(dbIdx) if options.strictNameMatching && expectedName.isDefined =>
            if !expectedName.get.equalsIgnoreCase(dbIdx.indexName) then
              issues += SchemaIssue.IndexNameMismatch(tableName, expectedName.get, dbIdx.indexName, columnLabels)
          case _ => // OK
      }
    end if

    // Check unique constraints
    // Note: JDBC doesn't expose unique constraints separately - they appear as unique indexes.
    // So we check both uniqueConstraints and unique indexes.
    if options.checkUniqueConstraints then
      instance.uniqueConstraints.foreach { ucSpec =>
        val columnLabels = ucSpec.columns.map(instance.fieldToLabel)
        val expectedName = ucSpec.constraintName
        // First check dedicated unique constraints, then fall back to unique indexes
        findMatchingUniqueConstraint(dbTable.uniqueConstraints, columnLabels)
          .orElse(findMatchingIndex(dbTable.indexes, columnLabels, expectedUnique = true)) match
          case None =>
            issues += SchemaIssue.MissingUniqueConstraint(tableName, expectedName, columnLabels)
          case Some(dbUc: DatabaseUniqueConstraint) if options.strictNameMatching && expectedName.isDefined =>
            if !expectedName.get.equalsIgnoreCase(dbUc.constraintName) then
              issues += SchemaIssue.UniqueConstraintNameMismatch(
                tableName,
                expectedName.get,
                dbUc.constraintName,
                columnLabels,
              )
          case Some(dbIdx: DatabaseIndex) if options.strictNameMatching && expectedName.isDefined =>
            if !expectedName.get.equalsIgnoreCase(dbIdx.indexName) then
              issues += SchemaIssue.UniqueConstraintNameMismatch(
                tableName,
                expectedName.get,
                dbIdx.indexName,
                columnLabels,
              )
          case _ => // OK
        end match
      }
    end if

    // Check foreign keys
    if options.checkForeignKeys then
      instance.foreignKeys.foreach { fkSpec =>
        val fromLabels   = fkSpec.fromColumns.map(instance.fieldToLabel)
        val toLabels     = fkSpec.toColumns.map(fn => fkSpec.toColumnMap.get(fn).map(_.label).getOrElse(fn))
        val expectedName = fkSpec.constraintName
        findMatchingForeignKey(dbTable.foreignKeys, fromLabels, fkSpec.toTable, toLabels) match
          case None =>
            issues += SchemaIssue.MissingForeignKey(tableName, expectedName, fromLabels, fkSpec.toTable, toLabels)
          case Some(dbFk) if options.strictNameMatching && expectedName.isDefined =>
            if !expectedName.get.equalsIgnoreCase(dbFk.constraintName) then
              issues += SchemaIssue.ForeignKeyNameMismatch(
                tableName,
                expectedName.get,
                dbFk.constraintName,
                fromLabels,
                fkSpec.toTable,
                toLabels,
              )
          case _ => // OK
        end match
      }
    end if

    issues.toList
  end compare

  private def findMatchingIndex(
      indexes: Seq[DatabaseIndex],
      expectedColumns: Seq[String],
      expectedUnique: Boolean,
  ): Option[DatabaseIndex] =
    indexes.find { idx =>
      idx.columns.map(_.toLowerCase) == expectedColumns.map(_.toLowerCase) &&
      idx.isUnique == expectedUnique
    }

  private def findMatchingUniqueConstraint(
      constraints: Seq[DatabaseUniqueConstraint],
      expectedColumns: Seq[String],
  ): Option[DatabaseUniqueConstraint] =
    constraints.find(_.columns.map(_.toLowerCase) == expectedColumns.map(_.toLowerCase))

  private def findMatchingForeignKey(
      foreignKeys: Seq[DatabaseForeignKey],
      fromColumns: Seq[String],
      toTable: String,
      toColumns: Seq[String],
  ): Option[DatabaseForeignKey] =
    foreignKeys.find { fk =>
      fk.fromColumns.map(_.toLowerCase) == fromColumns.map(_.toLowerCase) &&
      fk.toTable.equalsIgnoreCase(toTable) &&
      fk.toColumns.map(_.toLowerCase) == toColumns.map(_.toLowerCase)
    }

  // === Type Compatibility ===

  private def isTypeCompatible(expected: String, actual: String, strict: Boolean): Boolean =
    if strict then expected.equalsIgnoreCase(actual)
    else
      val normalizedExpected = normalizeType(expected)
      val normalizedActual   = normalizeType(actual)
      normalizedExpected == normalizedActual || areTypesInSameFamily(normalizedExpected, normalizedActual)

  private def normalizeType(t: String): String =
    t.toLowerCase.replaceAll("\\(.*\\)", "").trim

  private def areTypesInSameFamily(t1: String, t2: String): Boolean =
    val integerTypes   = Set("integer", "int", "int4", "serial", "bigint", "int8", "bigserial", "smallint", "int2")
    val textTypes      = Set("varchar", "text", "character varying", "char", "character", "bpchar")
    val numericTypes   = Set("numeric", "decimal", "real", "float", "float4", "float8", "double precision", "double")
    val boolTypes      = Set("boolean", "bool", "bit")
    val timestampTypes = Set("timestamp", "timestamptz", "timestamp with time zone", "timestamp without time zone")
    val jsonTypes      = Set("json", "jsonb")

    val families = Seq(integerTypes, textTypes, numericTypes, boolTypes, timestampTypes, jsonTypes)
    families.exists(family => family.contains(t1) && family.contains(t2))
  end areTypesInSameFamily
end SchemaIntrospection
