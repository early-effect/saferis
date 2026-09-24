package saferis

import zio.*

import java.util.Locale
import scala.collection.mutable.ListBuffer

/** Schema introspection and validation.
  *
  * Postgres reads `information_schema` and `pg_catalog` through `SqlSession`. A dialect without a catalog interpreter
  * fails with `SaferisError.Unsupported`.
  */
object SchemaIntrospection:

  /** Introspect a table's schema from the database. */
  def introspect(tableName: String)(using
      dialect: Dialect
  )(using
      Trace
  ): ZIO[SqlSession, SaferisError, Option[DatabaseTable]] =
    dialect match
      case support: SchemaIntrospectionSupport => support.introspectTable(tableName)
      case other                               =>
        ZIO.fail(SaferisError.Unsupported(s"${other.name} schema verification is not supported"))

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

    // Stored catalog name equals the folded label. Do not fold the catalog name.
    instance.columns.foreach { col =>
      val columnLabel = col.label
      dbTable.columns.find(_.name == folded(columnLabel)) match
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

    val expectedKeys = instance.columns.filter(_.isKey).map(_.label)
    if expectedKeys.nonEmpty && expectedKeys.map(folded).sorted != dbTable.primaryKeyColumns.sorted then
      issues += SchemaIssue.PrimaryKeyMismatch(tableName, expectedKeys, dbTable.primaryKeyColumns)

    if options.checkExtraColumns then
      val expectedColumnNames = instance.columns.map(col => folded(col.label)).toSet
      dbTable.columns.foreach { dbCol =>
        if !expectedColumnNames.contains(dbCol.name) then
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

    if options.checkUniqueConstraints then
      instance.uniqueConstraints.foreach { ucSpec =>
        val columnLabels = ucSpec.columns.map(instance.fieldToLabel)
        val expectedName = ucSpec.constraintName
        findMatchingUniqueConstraint(dbTable.uniqueConstraints, columnLabels) match
          case None =>
            issues += SchemaIssue.MissingUniqueConstraint(tableName, expectedName, columnLabels)
          case Some(dbUc) if options.strictNameMatching && expectedName.isDefined =>
            if !expectedName.get.equalsIgnoreCase(dbUc.constraintName) then
              issues += SchemaIssue.UniqueConstraintNameMismatch(
                tableName,
                expectedName.get,
                dbUc.constraintName,
                columnLabels,
              )
          case _ => ()
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

  /** Fold the Scala label. The catalog name is already the spelling Postgres stored. */
  private def folded(name: String): String =
    name.toLowerCase(Locale.ROOT)

  private def storedMatches(stored: Seq[String], expected: Seq[String]): Boolean =
    stored == expected.map(folded)

  private def findMatchingIndex(
      indexes: Seq[DatabaseIndex],
      expectedColumns: Seq[String],
      expectedUnique: Boolean,
  ): Option[DatabaseIndex] =
    indexes.find { idx =>
      storedMatches(idx.columns, expectedColumns) && idx.isUnique == expectedUnique
    }

  private def findMatchingUniqueConstraint(
      constraints: Seq[DatabaseUniqueConstraint],
      expectedColumns: Seq[String],
  ): Option[DatabaseUniqueConstraint] =
    constraints.find(uc => storedMatches(uc.columns, expectedColumns))

  private def findMatchingForeignKey(
      foreignKeys: Seq[DatabaseForeignKey],
      fromColumns: Seq[String],
      toTable: String,
      toColumns: Seq[String],
  ): Option[DatabaseForeignKey] =
    foreignKeys.find { fk =>
      storedMatches(fk.fromColumns, fromColumns) &&
      fk.toTable == folded(toTable) &&
      storedMatches(fk.toColumns, toColumns)
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
    val integerTypes =
      Set("integer", "int", "int4", "serial", "bigint", "int8", "bigserial", "smallint", "int2", "mediumint", "tinyint")
    val textTypes =
      Set("varchar", "text", "character varying", "char", "character", "bpchar", "tinytext", "mediumtext", "longtext")
    val numericTypes = Set("numeric", "decimal", "real", "float", "float4", "float8", "double precision", "double")
    // MySQL stores `boolean` as `tinyint(1)`.
    val boolTypes      = Set("boolean", "bool", "bit", "tinyint")
    val timestampTypes =
      Set("timestamp", "timestamptz", "timestamp with time zone", "timestamp without time zone", "datetime")
    val jsonTypes = Set("json", "jsonb")

    val families = Seq(integerTypes, textTypes, numericTypes, boolTypes, timestampTypes, jsonTypes)
    families.exists(family => family.contains(t1) && family.contains(t2))
  end areTypesInSameFamily
end SchemaIntrospection
