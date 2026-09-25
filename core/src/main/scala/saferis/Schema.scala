package saferis

import scala.quoted.*
import zio.*

/** Schema builder that captures the table type A for proper selector type inference. Provides a fluent API for defining
  * indexes and foreign keys on a table.
  *
  * Usage:
  * {{{
  *   import saferis.Schema.*
  *
  *   case class User(@generated @key id: Int, name: String, status: String) derives Table
  *   case class Order(@generated @key id: Int, userId: Int, amount: BigDecimal) derives Table
  *
  *   // Define schema with indexes and foreign keys
  *   val users = Schema[User]
  *     .withIndex(_.name)                                              // simple index
  *     .withIndex(_.status).where(_.status).eql("active")              // partial index
  *     .withIndex(_.name).and(_.status).named("idx_name_status")       // compound index
  *     .withUniqueIndex(_.email).where(_.status).eql("active")         // unique partial index
  *     .build
  *
  *   val orders = Schema[Order]
  *     .withForeignKey(_.userId).references[User](_.id).onDelete(Cascade)
  *     .withIndex(_.userId)
  *     .build
  *
  *   // Use with DDL
  *   ddl.createTable(users)
  *   ddl.createTable(orders)
  * }}}
  */
/** Schema wrapper that provides the fluent DSL for indexes and foreign keys. */
final case class Schema[A](instance: Instance[A]):
  /** Add an index on a column. */
  transparent inline def withIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.withIndexOn(instance, selector)

  /** Add a unique index on a column. */
  transparent inline def withUniqueIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.withUniqueIndexOn(instance, selector)

  /** Add a foreign key on a column. */
  transparent inline def withForeignKey[T](inline selector: A => T): InstanceFKBuilder[A, T] =
    Schema.withForeignKeyOn(instance, selector)

  /** Add a unique constraint on a column. */
  transparent inline def withUniqueConstraint[T](inline selector: A => T): InstanceUniqueBuilder[A] =
    Schema.withUniqueConstraintOn(instance, selector)

  /** Build the Instance with no additional schema elements. */
  def build: Instance[A] = instance

  /** Returns the complete DDL (CREATE TABLE + CREATE INDEX statements) as an SqlFragment.
    *
    * @param ifNotExists
    *   Whether to use IF NOT EXISTS clause
    */
  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    val tableName      = instance.tableName
    val cols           = instance.columns.map(_.withTableAlias(None))
    val keyColumns     = cols.filter(_.isKey)
    val hasCompoundKey = keyColumns.length > 1

    // Column definitions - use col.label directly (unquoted) for column names
    val columnDefs = cols.map { col =>
      val autoIncrement = dialect.autoIncrementClause(col.isGenerated, col.isKey, hasCompoundKey)
      val nullClause    = if col.isNullable then "" else " not null"
      val defaultClause = col.defaultClause.map(" " + _).getOrElse("")
      s"${col.label} ${col.columnType}$autoIncrement${nullClause}${defaultClause}"
    }

    // Primary key constraint - only needed for compound keys
    // For single keys, autoIncrementClause already adds " primary key"
    val pkConstraint = Option.when(hasCompoundKey) {
      s"primary key (${keyColumns.map(_.label).mkString(", ")})"
    }

    // Unique constraints from Schema DSL
    val uniqueConstraintsSql = instance.uniqueConstraintsSql

    // Foreign key constraints
    val fkConstraints = instance.foreignKeyConstraints

    // Combine all constraints
    val allConstraints = columnDefs ++ pkConstraint.toSeq ++ uniqueConstraintsSql ++ fkConstraints
    val createClause   = if ifNotExists then "create table if not exists" else "create table"
    val createTableSql = s"$createClause $tableName (${allConstraints.mkString(", ")})"

    // Index creation statements
    val indexStatements = instance.indexes.map { spec =>
      spec.toCreateSql(tableName, instance.fieldToLabel)
    }

    // Compound key index (if more than one key column)
    val compoundKeyIndex = Option.when(keyColumns.length > 1) {
      val keyColumnNames    = keyColumns.map(_.label)
      val compoundIndexName = IndexName.compoundKey(tableName)
      dialect match
        case d: IndexIfNotExistsSupport =>
          d.createIndexIfNotExistsSql(compoundIndexName.sql, tableName.sql, keyColumnNames.map(_.sql))
        case _ =>
          dialect.createIndexSql(compoundIndexName, tableName, keyColumnNames, ifNotExists)
    }

    // Combine all statements
    val allStatements = Seq(createTableSql) ++ indexStatements ++ compoundKeyIndex.toSeq
    SqlFragment.text(allStatements.mkString(";\n"))
  end ddl

  /** Verify this schema against the actual database schema.
    *
    * Succeeds with Unit if schema matches, fails with SaferisError.SchemaValidation containing all issues found.
    */
  def verify(using dialect: Dialect)(using Trace): ZIO[SqlSession, SaferisError, Unit] =
    SchemaIntrospection.verify(instance)

  /** Verify this schema against the actual database schema with custom options. */
  def verifyWith(options: VerifyOptions)(using
      dialect: Dialect
  )(using
      Trace
  ): ZIO[SqlSession, SaferisError, Unit] =
    SchemaIntrospection.verifyWith(instance, options)
end Schema

object Schema:
  // Re-export Encoder givens so they're available when importing Schema.*
  export Encoder.given

  // Re-export ForeignKeyAction for convenience
  export ForeignKeyAction.*

  /** Create a Schema wrapper for a table type, ready for schema configuration.
    *
    * @tparam A
    *   The table type (case class with derives Table)
    */
  inline def apply[A](using @scala.annotation.unused t: Table[A]): Schema[A] =
    ${ schemaApplyImpl[A] }

  private def schemaApplyImpl[A: Type](using Quotes): Expr[Schema[A]] =
    import quotes.reflect.*
    val table = Expr
      .summon[Table[A]]
      .getOrElse(
        report.errorAndAbort(s"Could not find Table instance for ${Type.show[A]}")
      )
    '{
      val inst = Instance[A](
        $table.name,
        $table.columns,
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
      )(using $table)
      Schema[A](inst)
    }
  end schemaApplyImpl

  // Top-level inline functions that Schema class methods delegate to

  inline def withIndexOn[A, T](instance: Instance[A], inline selector: A => T): InstanceIndexBuilder[A] =
    ${ withIndexImpl[A, T]('instance, 'selector, '{ false }) }

  inline def withUniqueIndexOn[A, T](
      instance: Instance[A],
      inline selector: A => T,
  ): InstanceIndexBuilder[A] =
    ${ withIndexImpl[A, T]('instance, 'selector, '{ true }) }

  inline def withForeignKeyOn[A, T](
      instance: Instance[A],
      inline selector: A => T,
  ): InstanceFKBuilder[A, T] =
    ${ withForeignKeyImpl[A, T]('instance, 'selector) }

  inline def withUniqueConstraintOn[A, T](
      instance: Instance[A],
      inline selector: A => T,
  ): InstanceUniqueBuilder[A] =
    ${ withUniqueConstraintImpl[A, T]('instance, 'selector) }

  inline def uniqueAnd[A, T](
      builder: InstanceUniqueBuilder[A],
      inline selector: A => T,
  ): InstanceUniqueBuilder[A] =
    ${ instanceUniqueAndImpl[A, T]('builder, 'selector) }

  inline def indexAnd[A, T](
      builder: InstanceIndexBuilder[A],
      inline selector: A => T,
  ): InstanceIndexBuilder[A] =
    ${ instanceIndexAndImpl[A, T]('builder, 'selector) }

  inline def indexWhere[A, T](
      builder: InstanceIndexBuilder[A],
      inline selector: A => T,
  ): InstanceWhereColumnBuilder[A, T] =
    ${ instanceIndexWhereImpl[A, T]('builder, 'selector) }

  inline def conditionAnd[A, T](
      builder: InstanceWhereConditionBuilder[A],
      inline selector: A => T,
  ): InstanceWhereColumnBuilder[A, T] =
    ${ instanceWhereAndImpl[A, T]('builder, 'selector) }

  inline def conditionOr[A, T](
      builder: InstanceWhereConditionBuilder[A],
      inline selector: A => T,
  ): InstanceWhereColumnBuilder[A, T] =
    ${ instanceWhereOrImpl[A, T]('builder, 'selector) }

  inline def fkAnd[A, T, T2](
      builder: InstanceFKBuilder[A, T],
      inline selector: A => T2,
  ): InstanceFKBuilder[A, T2] =
    ${ instanceFKAndImpl[A, T, T2]('builder, 'selector) }

  // Group WHERE macros
  inline def groupWhere[A, T](
      builder: InstanceWhereGroupBuilder[A],
      inline selector: A => T,
  ): InstanceWhereGroupColumnBuilder[A, T] =
    ${ groupWhereImpl[A, T]('builder, 'selector) }

  inline def groupResultAnd[A, T](
      builder: InstanceWhereGroupResult[A],
      inline selector: A => T,
  ): InstanceWhereGroupColumnBuilder[A, T] =
    ${ groupResultAndImpl[A, T]('builder, 'selector) }

  inline def groupResultOr[A, T](
      builder: InstanceWhereGroupResult[A],
      inline selector: A => T,
  ): InstanceWhereGroupColumnBuilder[A, T] =
    ${ groupResultOrImpl[A, T]('builder, 'selector) }

  inline def fkReferences[A, To, T2](
      builder: InstanceFKBuilder[A, ?],
      inline selector: To => T2,
  )(using @scala.annotation.unused t: Table[To]): InstanceFKRefBuilder[A, To, T2] =
    ${ instanceFKReferencesImpl[A, To, T2]('builder, 'selector) }

  inline def fkRefAnd[A, To](
      builder: InstanceFKRefBuilder[A, To, ?],
      inline selector: To => Any,
  ): InstanceFKRefBuilder[A, To, Any] =
    ${ instanceFKRefAndImpl[A, To]('builder, 'selector) }

  inline def chainIndexFromBuilder[A, T](
      builder: InstanceIndexBuilder[A],
      inline selector: A => T,
      unique: Boolean,
  ): InstanceIndexBuilder[A] =
    ${ chainIndexFromBuilderImpl[A, T]('builder, 'selector, '{ unique }) }

  inline def chainFKFromBuilder[A, T](
      builder: InstanceIndexBuilder[A],
      inline selector: A => T,
  ): InstanceFKBuilder[A, T] =
    ${ chainFKFromBuilderImpl[A, T]('builder, 'selector) }

  inline def chainIndexFromCondition[A, T](
      builder: InstanceWhereConditionBuilder[A],
      inline selector: A => T,
      unique: Boolean,
  ): InstanceIndexBuilder[A] =
    ${ chainIndexFromConditionImpl[A, T]('builder, 'selector, '{ unique }) }

  inline def chainFKFromCondition[A, T](
      builder: InstanceWhereConditionBuilder[A],
      inline selector: A => T,
  ): InstanceFKBuilder[A, T] =
    ${ chainFKFromConditionImpl[A, T]('builder, 'selector) }

  inline def chainIndexFromFKConfig[A, T](
      builder: InstanceFKConfigBuilder[A, ?],
      inline selector: A => T,
      unique: Boolean,
  ): InstanceIndexBuilder[A] =
    ${ chainIndexFromFKConfigImpl[A, T]('builder, 'selector, '{ unique }) }

  inline def chainFKFromFKConfig[A, T](
      builder: InstanceFKConfigBuilder[A, ?],
      inline selector: A => T,
  ): InstanceFKBuilder[A, T] =
    ${ chainFKFromFKConfigImpl[A, T]('builder, 'selector) }

  inline def chainIndexFromUnique[A, T](
      builder: InstanceUniqueBuilder[A],
      inline selector: A => T,
      unique: Boolean,
  ): InstanceIndexBuilder[A] =
    ${ chainIndexFromUniqueImpl[A, T]('builder, 'selector, '{ unique }) }

  inline def chainFKFromUnique[A, T](
      builder: InstanceUniqueBuilder[A],
      inline selector: A => T,
  ): InstanceFKBuilder[A, T] =
    ${ chainFKFromUniqueImpl[A, T]('builder, 'selector) }

  inline def chainUniqueFromUnique[A, T](
      builder: InstanceUniqueBuilder[A],
      inline selector: A => T,
  ): InstanceUniqueBuilder[A] =
    ${ chainUniqueFromUniqueImpl[A, T]('builder, 'selector) }

  // === Macro implementations ===

  /** Extract field name from a selector function like `_.fieldName` */
  private def extractFieldName[A: Type, T: Type](selector: Expr[A => T])(using Quotes): String =
    import quotes.reflect.*

    def extractFromBody(body: Term): String =
      body match
        case Select(_, name)      => name
        case Inlined(_, _, inner) => extractFromBody(inner)
        case Typed(inner, _)      => extractFromBody(inner)
        case Block(_, expr)       => extractFromBody(expr)
        case other                =>
          report.errorAndAbort(
            s"Expected a field access like _.fieldName, got: ${other.show} (class: ${other.getClass.getName})"
          )

    def extractFromTerm(term: Term): String =
      term match
        case Inlined(_, _, Block(List(DefDef(_, _, _, Some(body))), _)) =>
          extractFromBody(body)
        case Inlined(_, _, Lambda(_, body)) =>
          extractFromBody(body)
        case Inlined(_, _, inner) =>
          extractFromTerm(inner)
        case Lambda(_, body) =>
          extractFromBody(body)
        case Block(List(DefDef(_, _, _, Some(body))), _) =>
          extractFromBody(body)
        case other =>
          report.errorAndAbort(
            s"Expected a selector function, got: ${other.show}"
          )

    extractFromTerm(selector.asTerm)
  end extractFieldName

  private def withIndexImpl[A: Type, T: Type](
      instance: Expr[Instance[A]],
      selector: Expr[A => T],
      unique: Expr[Boolean],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      InstanceIndexBuilder[A](
        $instance,
        Seq(FieldName(${ Expr(columnName) })),
        $unique,
        None,
        Seq.empty,
      )
    }
  end withIndexImpl

  private def instanceIndexAndImpl[A: Type, T: Type](
      builder: Expr[InstanceIndexBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      b.copy(columns = b.columns :+ FieldName(${ Expr(columnName) }))
    }
  end instanceIndexAndImpl

  private def instanceIndexWhereImpl[A: Type, T: Type](
      builder: Expr[InstanceIndexBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{ InstanceWhereColumnBuilder[A, T]($builder, FieldName(${ Expr(columnName) })) }
  end instanceIndexWhereImpl

  private def instanceWhereAndImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereConditionBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b      = $builder
      val parent = InstanceIndexBuilder[A](b.instance, b.columns, b.unique, b.indexName, b.conditions)
      InstanceWhereColumnBuilder[A, T](parent, FieldName(${ Expr(columnName) }))
    }
  end instanceWhereAndImpl

  private def instanceWhereOrImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereConditionBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b      = $builder
      val parent = InstanceIndexBuilder[A](b.instance, b.columns, b.unique, b.indexName, b.conditions)
      InstanceWhereColumnBuilder[A, T](parent, FieldName(${ Expr(columnName) }), "or")
    }
  end instanceWhereOrImpl

  private def withForeignKeyImpl[A: Type, T: Type](
      instance: Expr[Instance[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceFKBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{ InstanceFKBuilder[A, T]($instance, Seq(FieldName(${ Expr(columnName) }))) }
  end withForeignKeyImpl

  // Chain macros that handle builder-to-instance conversion internally
  private def chainIndexFromBuilderImpl[A: Type, T: Type](
      builder: Expr[InstanceIndexBuilder[A]],
      selector: Expr[A => T],
      unique: Expr[Boolean],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceIndexBuilder[A](
        builtInst,
        Seq(FieldName(${ Expr(columnName) })),
        $unique,
        None,
        Seq.empty,
      )
    }
  end chainIndexFromBuilderImpl

  private def chainFKFromBuilderImpl[A: Type, T: Type](
      builder: Expr[InstanceIndexBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceFKBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceFKBuilder[A, T](builtInst, Seq(FieldName(${ Expr(columnName) })))
    }
  end chainFKFromBuilderImpl

  private def chainIndexFromConditionImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereConditionBuilder[A]],
      selector: Expr[A => T],
      unique: Expr[Boolean],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceIndexBuilder[A](
        builtInst,
        Seq(FieldName(${ Expr(columnName) })),
        $unique,
        None,
        Seq.empty,
      )
    }
  end chainIndexFromConditionImpl

  private def chainFKFromConditionImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereConditionBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceFKBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceFKBuilder[A, T](builtInst, Seq(FieldName(${ Expr(columnName) })))
    }
  end chainFKFromConditionImpl

  // Chain macros for FK config
  private def chainIndexFromFKConfigImpl[A: Type, T: Type](
      builder: Expr[InstanceFKConfigBuilder[A, ?]],
      selector: Expr[A => T],
      unique: Expr[Boolean],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.build
      InstanceIndexBuilder[A](
        builtInst,
        Seq(FieldName(${ Expr(columnName) })),
        $unique,
        None,
        Seq.empty,
      )
    }
  end chainIndexFromFKConfigImpl

  private def chainFKFromFKConfigImpl[A: Type, T: Type](
      builder: Expr[InstanceFKConfigBuilder[A, ?]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceFKBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.build
      InstanceFKBuilder[A, T](builtInst, Seq(FieldName(${ Expr(columnName) })))
    }
  end chainFKFromFKConfigImpl

  // Unique constraint macros
  private def withUniqueConstraintImpl[A: Type, T: Type](
      instance: Expr[Instance[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceUniqueBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      InstanceUniqueBuilder[A](
        $instance,
        Seq(FieldName(${ Expr(columnName) })),
        None,
      )
    }
  end withUniqueConstraintImpl

  private def instanceUniqueAndImpl[A: Type, T: Type](
      builder: Expr[InstanceUniqueBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceUniqueBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      b.copy(columns = b.columns :+ FieldName(${ Expr(columnName) }))
    }
  end instanceUniqueAndImpl

  private def chainIndexFromUniqueImpl[A: Type, T: Type](
      builder: Expr[InstanceUniqueBuilder[A]],
      selector: Expr[A => T],
      unique: Expr[Boolean],
  )(using Quotes): Expr[InstanceIndexBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceIndexBuilder[A](
        builtInst,
        Seq(FieldName(${ Expr(columnName) })),
        $unique,
        None,
        Seq.empty,
      )
    }
  end chainIndexFromUniqueImpl

  private def chainFKFromUniqueImpl[A: Type, T: Type](
      builder: Expr[InstanceUniqueBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceFKBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceFKBuilder[A, T](builtInst, Seq(FieldName(${ Expr(columnName) })))
    }
  end chainFKFromUniqueImpl

  private def chainUniqueFromUniqueImpl[A: Type, T: Type](
      builder: Expr[InstanceUniqueBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceUniqueBuilder[A]] =
    val columnName = extractFieldName(selector)
    '{
      val b         = $builder
      val builtInst = b.toInstance
      InstanceUniqueBuilder[A](builtInst, Seq(FieldName(${ Expr(columnName) })), None)
    }
  end chainUniqueFromUniqueImpl

  private def instanceFKAndImpl[A: Type, T: Type, T2: Type](
      builder: Expr[InstanceFKBuilder[A, T]],
      selector: Expr[A => T2],
  )(using Quotes): Expr[InstanceFKBuilder[A, T2]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      InstanceFKBuilder[A, T2](b.instance, b.fromColumns :+ FieldName(${ Expr(columnName) }))
    }
  end instanceFKAndImpl

  // Group WHERE macro implementations
  private def groupWhereImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereGroupBuilder[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereGroupColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{ InstanceWhereGroupColumnBuilder[A, T]($builder.instance, FieldName(${ Expr(columnName) }), Seq.empty, "and") }

  private def groupResultAndImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereGroupResult[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereGroupColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      InstanceWhereGroupColumnBuilder[A, T](b.instance, FieldName(${ Expr(columnName) }), b.conditions, "and")
    }

  private def groupResultOrImpl[A: Type, T: Type](
      builder: Expr[InstanceWhereGroupResult[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[InstanceWhereGroupColumnBuilder[A, T]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      InstanceWhereGroupColumnBuilder[A, T](b.instance, FieldName(${ Expr(columnName) }), b.conditions, "or")
    }

  private def instanceFKReferencesImpl[A: Type, To: Type, T2: Type](
      builder: Expr[InstanceFKBuilder[A, ?]],
      selector: Expr[To => T2],
  )(using Quotes): Expr[InstanceFKRefBuilder[A, To, T2]] =
    val toColumnName = extractFieldName(selector)
    val toTable      = Expr
      .summon[Table[To]]
      .getOrElse(
        quotes.reflect.report.errorAndAbort(s"Could not find Table instance for ${Type.show[To]}")
      )
    '{
      val b               = $builder
      val toTableInstance = $toTable
      InstanceFKRefBuilder[A, To, T2](
        b.instance,
        b.fromColumns,
        toTableInstance.name,
        Seq(FieldName(${ Expr(toColumnName) })),
        toTableInstance.columnMap,
      )
    }
  end instanceFKReferencesImpl

  private def instanceFKRefAndImpl[A: Type, To: Type](
      builder: Expr[InstanceFKRefBuilder[A, To, ?]],
      selector: Expr[To => Any],
  )(using Quotes): Expr[InstanceFKRefBuilder[A, To, Any]] =
    val columnName = extractFieldName(selector)
    '{
      val b = $builder
      InstanceFKRefBuilder[A, To, Any](
        b.instance,
        b.fromColumns,
        b.toTable,
        b.toColumns :+ FieldName(${ Expr(columnName) }),
        b.toColumnMap,
      )
    }
  end instanceFKRefAndImpl

end Schema

// Backwards compatibility alias
@deprecated("Use saferis.Schema instead", "0.2.0")
val TableAspects = Schema

// === Instance-bound builders for proper type inference ===

/** Builder for indexes on Instance with proper type inference. Note: Table[A] is not required here - the Instance
  * already contains table info.
  */
final case class InstanceIndexBuilder[A](
    instance: Instance[A],
    columns: Seq[FieldName],
    unique: Boolean,
    indexName: Option[IndexName],
    conditions: Seq[SqlText],
):
  /** Add another column to create a compound index. */
  transparent inline def and[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.indexAnd(this, selector)

  /** Start building a WHERE clause for a partial index. */
  transparent inline def where[T](inline selector: A => T): InstanceWhereColumnBuilder[A, T] =
    Schema.indexWhere(this, selector)

  /** Set a custom name for the index. */
  def named(name: IndexName): InstanceIndexBuilder[A] =
    copy(indexName = Some(name))

  /** Add another index (terminates current index and starts new one). */
  transparent inline def withIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromBuilder(this, selector, false)

  /** Add another unique index (terminates current index and starts new one). */
  transparent inline def withUniqueIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromBuilder(this, selector, true)

  /** Add a foreign key (terminates current index and starts FK). */
  transparent inline def withForeignKey[T](inline selector: A => T): InstanceFKBuilder[A, T] =
    Schema.chainFKFromBuilder(this, selector)

  /** Finalize and return the Instance with all indexes. */
  def build: Instance[A] = toInstance

  /** Returns the complete DDL (CREATE TABLE + CREATE INDEX statements) as an SqlFragment. */
  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    Schema(toInstance).ddl(ifNotExists)

  def toInstance: Instance[A] =
    val whereClause = Option.when(conditions.nonEmpty)(SqlText(conditions.mkString(" and ")))
    val spec        = IndexSpec[A](columns, indexName, unique, whereClause)
    // Access the table from the instance's implicit context
    given Table[A] = instance.tableEvidence
    Instance[A](
      instance.tableName,
      instance.columns,
      instance.alias,
      instance.foreignKeys,
      instance.indexes :+ spec,
      instance.uniqueConstraints,
    )
  end toInstance
end InstanceIndexBuilder

/** Builder for WHERE column selection on Instance indexes.
  *
  * Extends SchemaWhereOps to inherit all comparison operators for DDL literal SQL generation.
  */
final case class InstanceWhereColumnBuilder[A, T](
    parent: InstanceIndexBuilder[A],
    columnName: FieldName,
    operator: String = "and",
) extends SchemaWhereOps[InstanceWhereConditionBuilder[A], T]:
  // Look up the column label from the Instance (respects @label annotations)
  protected def schemaColumnName: ColumnName = parent.instance.fieldToLabel(columnName)

  protected def completeCondition(condition: SqlText): InstanceWhereConditionBuilder[A] =
    InstanceWhereConditionBuilder(parent, parent.conditions :+ condition, operator)
end InstanceWhereColumnBuilder

/** Builder for WHERE conditions on Instance indexes.
  */
final case class InstanceWhereConditionBuilder[A](
    instance: Instance[A],
    columns: Seq[FieldName],
    unique: Boolean,
    indexName: Option[IndexName],
    conditions: Seq[SqlText],
    operator: String,
):
  /** Chain another AND condition. */
  transparent inline def and[T](inline selector: A => T): InstanceWhereColumnBuilder[A, T] =
    Schema.conditionAnd(this, selector)

  /** Chain another OR condition. */
  transparent inline def or[T](inline selector: A => T): InstanceWhereColumnBuilder[A, T] =
    Schema.conditionOr(this, selector)

  /** Chain a grouped AND condition with explicit parentheses. Usage:
    * `.where(_.a).eql(1).andGroup(g => g.where(_.b).eql(2).or(_.c).eql(3))` Generates: `a = 1 AND (b = 2 OR c = 3)`
    */
  def andGroup(group: InstanceWhereGroupBuilder[A] => InstanceWhereGroupResult[A]): InstanceWhereConditionBuilder[A] =
    val groupBuilder = InstanceWhereGroupBuilder[A](instance)
    val result       = group(groupBuilder)
    val grouped      = SqlText(s"(${result.conditions.mkString(s" ${result.operator} ")})")
    copy(conditions = conditions :+ grouped, operator = "and")

  /** Chain a grouped OR condition with explicit parentheses. Usage:
    * `.where(_.a).eql(1).orGroup(g => g.where(_.b).eql(2).and(_.c).eql(3))` Generates: `a = 1 OR (b = 2 AND c = 3)`
    */
  def orGroup(group: InstanceWhereGroupBuilder[A] => InstanceWhereGroupResult[A]): InstanceWhereConditionBuilder[A] =
    val groupBuilder = InstanceWhereGroupBuilder[A](instance)
    val result       = group(groupBuilder)
    val grouped      = SqlText(s"(${result.conditions.mkString(s" ${result.operator} ")})")
    copy(conditions = conditions :+ grouped, operator = "or")

  /** Set a custom name for the index. */
  def named(name: IndexName): InstanceWhereConditionBuilder[A] =
    copy(indexName = Some(name))

  /** Add another index (terminates current index and starts new one). */
  transparent inline def withIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromCondition(this, selector, false)

  /** Add another unique index (terminates current index and starts new one). */
  transparent inline def withUniqueIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromCondition(this, selector, true)

  /** Add a foreign key (terminates current index and starts FK). */
  transparent inline def withForeignKey[T](inline selector: A => T): InstanceFKBuilder[A, T] =
    Schema.chainFKFromCondition(this, selector)

  /** Finalize and return the Instance with all indexes. */
  def build: Instance[A] = toInstance

  /** Returns the complete DDL (CREATE TABLE + CREATE INDEX statements) as an SqlFragment. */
  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    Schema(toInstance).ddl(ifNotExists)

  def toInstance: Instance[A] =
    val whereClause = Option.when(conditions.nonEmpty)(SqlText(conditions.mkString(s" $operator ")))
    val spec        = IndexSpec[A](columns, indexName, unique, whereClause)
    given Table[A]  = instance.tableEvidence
    Instance[A](
      instance.tableName,
      instance.columns,
      instance.alias,
      instance.foreignKeys,
      instance.indexes :+ spec,
      instance.uniqueConstraints,
    )
  end toInstance
end InstanceWhereConditionBuilder

object InstanceWhereConditionBuilder:
  def apply[A](
      parent: InstanceIndexBuilder[A],
      conditions: Seq[SqlText],
      operator: String,
  ): InstanceWhereConditionBuilder[A] =
    InstanceWhereConditionBuilder(
      parent.instance,
      parent.columns,
      parent.unique,
      parent.indexName,
      conditions,
      operator,
    )
end InstanceWhereConditionBuilder

/** Builder for foreign keys on Instance with proper type inference.
  */
final case class InstanceFKBuilder[A, T](
    instance: Instance[A],
    fromColumns: Seq[FieldName],
):
  /** Add another column to create a compound foreign key. */
  transparent inline def and[T2](inline selector: A => T2): InstanceFKBuilder[A, T2] =
    Schema.fkAnd(this, selector)

  /** Specify the referenced table and column. */
  transparent inline def references[To: Table](
      inline selector: To => Any
  ): InstanceFKRefBuilder[A, To, Any] =
    Schema.fkReferences[A, To, Any](this, selector)
end InstanceFKBuilder

/** Builder for FK configuration on Instance.
  */
final case class InstanceFKConfigBuilder[A, To](
    instance: Instance[A],
    fromColumns: Seq[FieldName],
    toTable: TableName,
    toColumns: Seq[FieldName],
    toColumnMap: Map[String, Column[?]] = Map.empty,
    onDeleteAction: ForeignKeyAction = ForeignKeyAction.NoAction,
    onUpdateAction: ForeignKeyAction = ForeignKeyAction.NoAction,
    constraintName: Option[ConstraintName] = None,
):
  def onDelete(action: ForeignKeyAction): InstanceFKConfigBuilder[A, To] =
    copy(onDeleteAction = action)

  def onUpdate(action: ForeignKeyAction): InstanceFKConfigBuilder[A, To] =
    copy(onUpdateAction = action)

  def named(name: ConstraintName): InstanceFKConfigBuilder[A, To] =
    copy(constraintName = Some(name))

  /** Add another index (terminates current FK and starts new index). */
  transparent inline def withIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromFKConfig(this, selector, false)

  /** Add another unique index (terminates current FK and starts new index). */
  transparent inline def withUniqueIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromFKConfig(this, selector, true)

  /** Add another foreign key (terminates current FK and starts new one). */
  transparent inline def withForeignKey[T](inline selector: A => T): InstanceFKBuilder[A, T] =
    Schema.chainFKFromFKConfig(this, selector)

  def build: Instance[A] =
    val spec = ForeignKeySpec[A, To](
      fromColumns = fromColumns,
      toTable = toTable,
      toColumns = toColumns,
      toColumnMap = toColumnMap,
      onDelete = onDeleteAction,
      onUpdate = onUpdateAction,
      constraintName = constraintName,
    )
    given Table[A] = instance.tableEvidence
    Instance[A](
      instance.tableName,
      instance.columns,
      instance.alias,
      instance.foreignKeys :+ spec,
      instance.indexes,
      instance.uniqueConstraints,
    )
  end build

  /** Returns the complete DDL (CREATE TABLE + CREATE INDEX statements) as an SqlFragment. */
  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    Schema(build).ddl(ifNotExists)
end InstanceFKConfigBuilder

/** Builder for FK reference column chaining (for compound FKs).
  */
final case class InstanceFKRefBuilder[A, To, T](
    instance: Instance[A],
    fromColumns: Seq[FieldName],
    toTable: TableName,
    toColumns: Seq[FieldName],
    toColumnMap: Map[String, Column[?]] = Map.empty,
):
  /** Add another referenced column (for compound foreign keys). */
  transparent inline def and(inline selector: To => Any): InstanceFKRefBuilder[A, To, Any] =
    Schema.fkRefAnd[A, To](this, selector)

  def onDelete(action: ForeignKeyAction): InstanceFKConfigBuilder[A, To] =
    toConfig.onDelete(action)

  def onUpdate(action: ForeignKeyAction): InstanceFKConfigBuilder[A, To] =
    toConfig.onUpdate(action)

  def named(name: ConstraintName): InstanceFKConfigBuilder[A, To] =
    toConfig.named(name)

  /** Add another index (terminates current FK and starts new index). */
  transparent inline def withIndex[T2](inline selector: A => T2): InstanceIndexBuilder[A] =
    Schema.chainIndexFromFKConfig(toConfig, selector, false)

  /** Add another unique index (terminates current FK and starts new index). */
  transparent inline def withUniqueIndex[T2](inline selector: A => T2): InstanceIndexBuilder[A] =
    Schema.chainIndexFromFKConfig(toConfig, selector, true)

  /** Add another foreign key (terminates current FK and starts new one). */
  transparent inline def withForeignKey[T2](inline selector: A => T2): InstanceFKBuilder[A, T2] =
    Schema.chainFKFromFKConfig(toConfig, selector)

  def build: Instance[A] = toConfig.build

  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    Schema(build).ddl(ifNotExists)

  private def toConfig: InstanceFKConfigBuilder[A, To] =
    InstanceFKConfigBuilder[A, To](instance, fromColumns, toTable, toColumns, toColumnMap)
end InstanceFKRefBuilder

/** Builder for unique constraints on Instance with proper type inference.
  */
final case class InstanceUniqueBuilder[A](
    instance: Instance[A],
    columns: Seq[FieldName],
    constraintName: Option[ConstraintName],
):
  /** Add another column to create a compound unique constraint. */
  transparent inline def and[T](inline selector: A => T): InstanceUniqueBuilder[A] =
    Schema.uniqueAnd(this, selector)

  /** Set a custom name for the constraint. */
  def named(name: ConstraintName): InstanceUniqueBuilder[A] =
    copy(constraintName = Some(name))

  /** Add another index (terminates current unique constraint and starts new index). */
  transparent inline def withIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromUnique(this, selector, false)

  /** Add another unique index (terminates current unique constraint and starts new index). */
  transparent inline def withUniqueIndex[T](inline selector: A => T): InstanceIndexBuilder[A] =
    Schema.chainIndexFromUnique(this, selector, true)

  /** Add a foreign key (terminates current unique constraint and starts FK). */
  transparent inline def withForeignKey[T](inline selector: A => T): InstanceFKBuilder[A, T] =
    Schema.chainFKFromUnique(this, selector)

  /** Add another unique constraint (terminates current unique constraint and starts new one). */
  transparent inline def withUniqueConstraint[T](inline selector: A => T): InstanceUniqueBuilder[A] =
    Schema.chainUniqueFromUnique(this, selector)

  /** Finalize and return the Instance with all unique constraints. */
  def build: Instance[A] = toInstance

  /** Returns the complete DDL (CREATE TABLE + CREATE INDEX statements) as an SqlFragment. */
  def ddl(ifNotExists: Boolean = true)(using dialect: Dialect): SqlFragment =
    Schema(toInstance).ddl(ifNotExists)

  def toInstance: Instance[A] =
    val spec       = UniqueConstraintSpec[A](columns, constraintName)
    given Table[A] = instance.tableEvidence
    Instance[A](
      instance.tableName,
      instance.columns,
      instance.alias,
      instance.foreignKeys,
      instance.indexes,
      instance.uniqueConstraints :+ spec,
    )
  end toInstance
end InstanceUniqueBuilder

// === Grouped WHERE condition builders ===

/** Builder for grouped WHERE conditions (for explicit parentheses). Used via `.andGroup(g => ...)` or
  * `.orGroup(g => ...)`.
  */
final case class InstanceWhereGroupBuilder[A](
    instance: Instance[A]
):
  /** Start a grouped WHERE clause. */
  transparent inline def where[T](inline selector: A => T): InstanceWhereGroupColumnBuilder[A, T] =
    Schema.groupWhere(this, selector)

/** Builder for WHERE column selection in a group.
  *
  * Extends SchemaWhereOps to inherit all comparison operators for DDL literal SQL generation.
  */
final case class InstanceWhereGroupColumnBuilder[A, T](
    instance: Instance[A],
    columnName: FieldName,
    previousConditions: Seq[SqlText],
    operator: String,
) extends SchemaWhereOps[InstanceWhereGroupResult[A], T]:
  // Look up the column label from the Instance (respects @label annotations)
  protected def schemaColumnName: ColumnName = instance.fieldToLabel(columnName)

  protected def completeCondition(condition: SqlText): InstanceWhereGroupResult[A] =
    InstanceWhereGroupResult(instance, previousConditions :+ condition, operator)
end InstanceWhereGroupColumnBuilder

/** Result of a grouped WHERE condition, can be chained with and/or.
  */
final case class InstanceWhereGroupResult[A](
    instance: Instance[A],
    conditions: Seq[SqlText],
    operator: String,
):
  /** Chain another AND condition in the group. */
  transparent inline def and[T](inline selector: A => T): InstanceWhereGroupColumnBuilder[A, T] =
    Schema.groupResultAnd(this, selector)

  /** Chain another OR condition in the group. */
  transparent inline def or[T](inline selector: A => T): InstanceWhereGroupColumnBuilder[A, T] =
    Schema.groupResultOr(this, selector)
end InstanceWhereGroupResult

// === JSON-specific WHERE operators (only available for Json[T] columns with JsonSupport dialect) ===

/** Extension methods for JSON columns in WHERE clause. These operators are only available when:
  *   1. The column type is Json[T]
  *   2. A JsonSupport dialect is in scope
  *   3. The type T has a JsonCodec
  */
extension [A, T](builder: InstanceWhereColumnBuilder[A, Json[T]])

  /** Check if JSON column contains the given value. PostgreSQL: `column @> '{"key": "value"}'` MySQL:
    * `JSON_CONTAINS(column, '{"key": "value"}')`
    */
  def jsonContains(
      value: T
  )(using codec: zio.json.JsonCodec[T], dialect: Dialect & JsonSupport): InstanceWhereConditionBuilder[A] =
    val jsonValue = JsonText(codec.encoder.encodeJson(value, None).toString)
    val condition = dialect.jsonContainsSql(builder.parent.instance.fieldToLabel(builder.columnName).sql, jsonValue)
    InstanceWhereConditionBuilder(builder.parent, builder.parent.conditions :+ condition, builder.operator)

  /** Check if JSON column has the specified key. PostgreSQL: `column ? 'key'` MySQL:
    * `JSON_CONTAINS_PATH(column, 'one', '$.key')`
    */
  def jsonHasKey(key: String)(using dialect: Dialect & JsonSupport): InstanceWhereConditionBuilder[A] =
    val condition = dialect.jsonHasKeySql(builder.parent.instance.fieldToLabel(builder.columnName).sql, key)
    InstanceWhereConditionBuilder(builder.parent, builder.parent.conditions :+ condition, builder.operator)

  /** Check if JSON column has any of the specified keys. PostgreSQL: `column ?| array['key1', 'key2']` MySQL:
    * `JSON_CONTAINS_PATH(column, 'one', '$.key1', '$.key2')`
    */
  def jsonHasAnyKey(keys: Seq[String])(using dialect: Dialect & JsonSupport): InstanceWhereConditionBuilder[A] =
    val condition = dialect.jsonHasAnyKeySql(builder.parent.instance.fieldToLabel(builder.columnName).sql, keys)
    InstanceWhereConditionBuilder(builder.parent, builder.parent.conditions :+ condition, builder.operator)

  /** Check if JSON column has all of the specified keys. PostgreSQL: `column ?& array['key1', 'key2']` MySQL:
    * `JSON_CONTAINS_PATH(column, 'all', '$.key1', '$.key2')`
    */
  def jsonHasAllKeys(keys: Seq[String])(using dialect: Dialect & JsonSupport): InstanceWhereConditionBuilder[A] =
    val condition = dialect.jsonHasAllKeysSql(builder.parent.instance.fieldToLabel(builder.columnName).sql, keys)
    InstanceWhereConditionBuilder(builder.parent, builder.parent.conditions :+ condition, builder.operator)

  /** Start building a JSON path comparison. Usage: `.where(_.data).jsonPath("user.email").eql("test@example.com")`
    */
  def jsonPath(path: String)(using dialect: Dialect & JsonSupport): JsonPathBuilder[A] =
    JsonPathBuilder(builder.parent, builder.parent.instance.fieldToLabel(builder.columnName), path, builder.operator)
end extension

/** Builder for JSON path comparisons. Created via `.jsonPath("field.path")` on a JSON column WHERE builder.
  */
final case class JsonPathBuilder[A](
    parent: InstanceIndexBuilder[A],
    columnName: ColumnName,
    path: String,
    operator: String,
)(using dialect: Dialect & JsonSupport):

  /** Equals comparison on extracted JSON path value. */
  def eql(value: String): InstanceWhereConditionBuilder[A] =
    where(s"= ${SqlValue.quote(value)}")

  /** Not equals comparison on extracted JSON path value. */
  def neql(value: String): InstanceWhereConditionBuilder[A] =
    where(s"<> ${SqlValue.quote(value)}")

  /** LIKE pattern matching on extracted JSON path value. */
  def like(pattern: String): InstanceWhereConditionBuilder[A] =
    where(s"like ${SqlValue.quote(pattern)}")

  /** IS NULL check on extracted JSON path value. */
  def isNull: InstanceWhereConditionBuilder[A] = where("is null")

  /** IS NOT NULL check on extracted JSON path value. */
  def isNotNull: InstanceWhereConditionBuilder[A] = where("is not null")

  private def where(predicate: String): InstanceWhereConditionBuilder[A] =
    val condition = SqlText(s"${dialect.jsonExtractSql(columnName.sql, path)} $predicate")
    InstanceWhereConditionBuilder(parent, parent.conditions :+ condition, operator)
end JsonPathBuilder
