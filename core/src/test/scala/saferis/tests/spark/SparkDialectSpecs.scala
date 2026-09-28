package saferis.tests.spark

import saferis.*
import saferis.spark.given
import zio.test.*

object SparkDialectSpecs extends ZIOSpecDefault:

  val spec = suite("Spark Dialect Support")(
    test("Spark dialect name") {
      val dialect = summon[Dialect]
      assertTrue(dialect.name == "Spark SQL")
    },
    test("Spark uses backticks for identifier quoting") {
      val dialect = summon[Dialect]
      assertTrue(dialect.identifierQuote == "`")
    },
    test("Spark escapes identifiers with backticks") {
      val dialect = summon[Dialect]
      assertTrue(
        dialect.escapeIdentifier(ColumnName("table_name")) == "`table_name`" &&
          dialect.escapeIdentifier(ColumnName("column-with-dash")) == "`column-with-dash`" &&
          dialect.escapeIdentifier(ColumnName("column with spaces")) == "`column with spaces`" &&
          dialect.escapeIdentifier(ColumnName("column`name")) == "`column``name`"
      )
    },
    test("Spark type mappings") {
      val dialect = summon[Dialect]
      assertTrue(
        dialect.columnType(SqlType.VarChar) == "string" &&
          dialect.columnType(SqlType.Text) == "string" &&
          dialect.columnType(SqlType.SmallInt) == "smallint" &&
          dialect.columnType(SqlType.Integer) == "int" &&
          dialect.columnType(SqlType.BigInt) == "bigint" &&
          dialect.columnType(SqlType.Real) == "float" &&
          dialect.columnType(SqlType.DoublePrecision) == "double" &&
          dialect.columnType(SqlType.Bool) == "boolean" &&
          dialect.columnType(SqlType.Date) == "date" &&
          dialect.columnType(SqlType.Timestamp) == "timestamp" &&
          dialect.columnType(SqlType.Binary) == "binary" &&
          dialect.columnType(SqlType.Uuid) == "string"
      )
    },
    test("Spark DDL uses IF NOT EXISTS") {
      val dialect = summon[Dialect]
      assertTrue(
        dialect.createTableClause(ifNotExists = true) == "create table if not exists" &&
          dialect.createTableClause(ifNotExists = false) == "create table" &&
          dialect.dropTableSql(TableName("my_table"), ifExists = true) == "drop table if exists `my_table`" &&
          dialect.dropTableSql(TableName("my_table"), ifExists = false) == "drop table `my_table`"
      )
    },
    test("Spark does not support indexes") {
      val dialect = summon[Dialect]
      // Just verify that these methods throw UnsupportedOperationException
      val createsIndex =
        try
          dialect.createIndexSql(IndexName("idx"), TableName("table"), Seq(ColumnName("col")))
          false
        catch case _: UnsupportedOperationException => true
      val createsUnique =
        try
          dialect.createUniqueIndexSql(IndexName("idx"), TableName("table"), Seq(ColumnName("col")))
          false
        catch case _: UnsupportedOperationException => true
      val dropsIndex =
        try
          dialect.dropIndexSql(IndexName("idx"))
          false
        catch case _: UnsupportedOperationException => true
      assertTrue(createsIndex && createsUnique && dropsIndex)
    },
    test("Spark JSON support") {
      summon[Dialect] match
        case dialect: JsonSupport =>
          assertTrue(
            dialect.jsonType == "string" &&
              dialect.jsonExtractSql(SqlText("data"), "field") == "get_json_object(data, '$.field')"
          )
        case _ => assertTrue(false)
    },
    test("Spark array support") {
      summon[Dialect] match
        case dialect: ArraySupport =>
          assertTrue(
            dialect.arrayType(ColumnType("int")) == "array<int>" &&
              dialect.arrayContainsSql(SqlText("tags"), SqlText("value")) == "array_contains(tags, value)"
          )
        case _ => assertTrue(false)
    },
    test("String literals use single quotes (Encoder default)") {
      val encoder = summon[Encoder[String]]

      assertTrue(
        encoder.literal("some_value") == "'some_value'" &&
          encoder.literal("value'with'quotes") == "'value''with''quotes'"
      )
    },
    test("Identifiers vs Literals - the key distinction") {
      val dialect    = summon[Dialect]
      val encoder    = summon[Encoder[String]]
      val columnName = ColumnName("column-with-dash")

      assertTrue(
        dialect.escapeIdentifier(columnName) == "`column-with-dash`" &&
          encoder.literal("some_value") == "'some_value'"
      )
    },
  )

end SparkDialectSpecs
