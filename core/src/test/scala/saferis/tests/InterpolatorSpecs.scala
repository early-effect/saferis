package saferis.tests

import saferis.*
import saferis.mysql.MySQLDialect
import saferis.postgres.given
import saferis.sqlite.SQLiteDialect
import zio.*
import zio.test.*

object InterpolatorSpecs extends ZIOSpecDefault:

  private def arrayLength(placeholder: Placeholder): Int =
    placeholder.pieces
      .collect:
        case SqlPiece.Param(SqlValue.Array(_, values)) => values.length
      .sum
  val spec =
    suiteAll("interpolator"):
      test("simple interpolation"):
        val name = "Bob"
        val sql  = sqlEcho"select * from test_table_no_key where name = $name"
        assertTrue(sql.sql == "select * from test_table_no_key where name = $1")
      test("multiple interpolations"):
        val name = "Bob"
        val age  = 42
        val sql  = sql"select * from test_table_no_key where name = $name and age = $age"
        assertTrue(sql.sql == "select * from test_table_no_key where name = $1 and age = $2") && assertTrue(
          sql.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2
        )
      test("interpolation with a fragment"):
        val name = "Bob"
        val age  = 42
        val id   = 1L
        val frag = sql"where name = $name and age = $age"
        val sql  = sql"select * from test_table_no_key $frag and id = $id"
        assertTrue(sql.sql == "select * from test_table_no_key where name = $1 and age = $2 and id = $3") && assertTrue(
          sql.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3
        )
      test("with margin"):
        val name = "Bob"
        val age  = 42

        val frag =
          sqlEcho"""|select *
                |from test_table_no_key
                |where name = $name and age = $age""".stripMargin
        assertTrue(
          frag.sql ==
            """|select *
               |from test_table_no_key
               |where name = $1 and age = $2""".stripMargin
        ) && assertTrue(
          frag.show ==
            """|select *
               |from test_table_no_key
               |where name = 'Bob' and age = 42""".stripMargin
        )
      test("with constant argument"):
        val sql = sql"select * from test_table_no_key where name = ${"foo"} and age = ${12} and id = 1"
        assertTrue(sql.sql == "select * from test_table_no_key where name = $1 and age = $2 and id = 1") && assertTrue(
          sql.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2
        )
      test("Placeholder.identifier with PostgreSQL dialect prevents SQL injection"):
        val pgDialect = summon[Dialect]
        // Test normal column name
        val col1 = Placeholder.identifier("user_id")(using pgDialect)
        assertTrue(col1.sql == "\"user_id\"") &&
        // Test column name with embedded quotes - should be escaped
        assertTrue(Placeholder.identifier("my\"column")(using pgDialect).sql == "\"my\"\"column\"") &&
        // Test SQL injection attempt
        assertTrue(
          Placeholder.identifier("\"; DROP TABLE users--")(using pgDialect).sql == "\"\"\"; DROP TABLE users--\""
        ) &&
        // Test in a query context
        assertTrue {
          val tableName = Placeholder.identifier("users")(using pgDialect)
          val colName   = Placeholder.identifier("name")(using pgDialect)
          val value     = "Alice"
          val query     = sql"SELECT * FROM $tableName WHERE $colName = $value"
          query.sql == "SELECT * FROM \"users\" WHERE \"name\" = $1"
        }
      test("Placeholder.identifier with MySQL dialect prevents SQL injection"):
        given Dialect = MySQLDialect
        // Test normal column name
        val col1 = Placeholder.identifier("user_id")
        assertTrue(col1.sql == "`user_id`") &&
        // Test column name with embedded backticks - should be escaped
        assertTrue(Placeholder.identifier("my`column").sql == "`my``column`") &&
        // Test SQL injection attempt
        assertTrue(Placeholder.identifier("`; DROP TABLE users--").sql == "```; DROP TABLE users--`") &&
        // Test in a query context
        assertTrue {
          val tableName = Placeholder.identifier("users")
          val colName   = Placeholder.identifier("name")
          val value     = "Alice"
          val query     = sql"SELECT * FROM $tableName WHERE $colName = $value"
          query.sql == "SELECT * FROM `users` WHERE `name` = $1"
        }
      test("Placeholder.identifier with SQLite dialect prevents SQL injection"):
        given Dialect = SQLiteDialect
        // Test normal column name
        val col1 = Placeholder.identifier("user_id")
        assertTrue(col1.sql == "\"user_id\"") &&
        // Test column name with embedded quotes - should be escaped
        assertTrue(Placeholder.identifier("my\"column").sql == "\"my\"\"column\"") &&
        // Test SQL injection attempt
        assertTrue(Placeholder.identifier("\"; DROP TABLE users--").sql == "\"\"\"; DROP TABLE users--\"") &&
        // Test in a query context
        assertTrue {
          val tableName = Placeholder.identifier("users")
          val colName   = Placeholder.identifier("name")
          val value     = "Alice"
          val query     = sql"SELECT * FROM $tableName WHERE $colName = $value"
          query.sql == "SELECT * FROM \"users\" WHERE \"name\" = $1"
        }

      // === Placeholder.list ===

      test("Placeholder.list is one array parameter"):
        val ph = Placeholder.list(List(1, 2, 3))
        assertTrue(
          ph.sql == "$1",
          ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(ph) == 3,
          ph.issues.isEmpty,
        )

      test("Placeholder.list single-element edge"):
        val ph = Placeholder.list(List("only"))
        assertTrue(ph.sql == "$1", ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1, arrayLength(ph) == 1)

      test("Placeholder.list varargs overload matches Iterable form"):
        val viaVarargs  = Placeholder.list(1, 2, 3)
        val viaIterable = Placeholder.list(List(1, 2, 3))
        assertTrue(viaVarargs.sql == viaIterable.sql, arrayLength(viaVarargs) == arrayLength(viaIterable))

      test("Placeholder.list deduplicates inside the array"):
        val ph = Placeholder.list(List("a", "a", "b", "b", "c"))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 3)

      // === array (top-level helper) ===

      test("array is one parameter and does not include the operator"):
        val ph = array(List("a", "b", "c"))
        assertTrue(
          ph.sql == "$1",
          ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(ph) == 3,
          ph.issues.isEmpty,
        )

      test("array with a single element"):
        val ph = array(List("only"))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 1)

      test("array varargs"):
        val ph = array("active", "pending")
        assertTrue(ph.sql == "$1", arrayLength(ph) == 2)

      test("array varargs single arg"):
        val ph = array(42)
        assertTrue(ph.sql == "$1", arrayLength(ph) == 1)

      test("array inside a fragment keeps the operator in the SQL string"):
        val frag = sql"select * from t where x = any(${array(List(1, 2))})"
        assertTrue(
          frag.sql == "select * from t where x = any($1)",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
        )

      test("array accepts Vector"):
        val ph = array(Vector(1, 2, 3))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 3)

      test("array accepts Set and keeps one parameter"):
        val ph = array(Set(1, 2, 3))
        assertTrue(ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1, arrayLength(ph) == 3, ph.sql == "$1")

      test("array preserves LinkedHashSet insertion order"):
        val lhs  = scala.collection.mutable.LinkedHashSet("a", "b", "c")
        val ph   = array(lhs)
        val show = sql"x = any($ph)".show
        assertTrue(show == "x = any(ARRAY['a', 'b', 'c'])")

      test("array with SortedSet emits sorted order"):
        val ss   = scala.collection.immutable.SortedSet(3, 1, 2)
        val ph   = array(ss)
        val show = sql"x = any($ph)".show
        assertTrue(show == "x = any(ARRAY[1, 2, 3])")

      test("array accepts a Range"):
        val ph = array(1 to 3)
        assertTrue(arrayLength(ph) == 3, ph.sql == "$1")

      test("array deduplicates"):
        val ph   = array(List(1, 1, 2, 2, 3))
        val show = sql"x = any($ph)".show
        assertTrue(ph.sql == "$1", arrayLength(ph) == 3, show == "x = any(ARRAY[1, 2, 3])")

      test("Placeholder.list dedupes single-element collapse"):
        val ph = Placeholder.list(List("a", "a"))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 1)

      test("array with all-duplicates collapses to one"):
        val ph = array(List(1, 1, 1))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 1, ph.issues.isEmpty)

      test("an empty array is one parameter and has no issues"):
        val ph = array(List.empty[String])
        assertTrue(ph.sql == "$1", ph.issues.isEmpty, arrayLength(ph) == 0)

      test("Placeholder.list of an empty collection is an empty array"):
        val ph = Placeholder.list(List.empty[String])
        assertTrue(ph.sql == "$1", ph.issues.isEmpty, arrayLength(ph) == 0)

      test("mixed splice keeps writes in argument order"):
        val name = "Bob"
        val ids  = List(10, 20, 30)
        val age  = 42
        val frag = sql"select * from t where a = $name and b = any(${array(ids)}) and c = $age"
        assertTrue(
          frag.sql == "select * from t where a = $1 and b = any($2) and c = $3",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3,
          frag.show == "select * from t where a = 'Bob' and b = any(ARRAY[10, 20, 30]) and c = 42",
        )

      test("two array splices keep ordering across argument positions"):
        val xs   = List(1, 2)
        val ys   = List(3, 4, 5)
        val frag = sql"... a = any(${array(xs)}) and b <> all(${array(ys)})"
        assertTrue(
          frag.sql == "... a = any($1) and b <> all($2)",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2,
          frag.show == "... a = any(ARRAY[1, 2]) and b <> all(ARRAY[3, 4, 5])",
        )

      test("SqlFragment.validate succeeds for valid fragments"):
        val frag = sql"select 1"
        for result <- frag.validate
        yield assertTrue(result == frag)

end InterpolatorSpecs
