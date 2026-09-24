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

      // === in (top-level helper) ===

      test("in is one array compared with ANY"):
        val ph = in(List("a", "b", "c"))
        assertTrue(
          ph.sql == "= ANY($1)",
          ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(ph) == 3,
          ph.issues.isEmpty,
        )

      test("in with single element"):
        val ph = in(List("only"))
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 1)

      test("in varargs"):
        val ph = in("active", "pending")
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 2)

      test("in varargs single arg"):
        val ph = in(42)
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 1)

      test("in inside a real fragment"):
        val frag = sql"select * from t where x ${in(List(1, 2))}"
        assertTrue(
          frag.sql == "select * from t where x = ANY($1)",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
        )

      // === collection-type variety ===

      test("in accepts Vector"):
        val ph = in(Vector(1, 2, 3))
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 3)

      test("in accepts Set and keeps one parameter"):
        val ph = in(Set(1, 2, 3))
        assertTrue(ph.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1, arrayLength(ph) == 3, ph.sql == "= ANY($1)")

      test("in preserves LinkedHashSet insertion order"):
        val lhs  = scala.collection.mutable.LinkedHashSet("a", "b", "c")
        val ph   = in(lhs)
        val show = sql"x $ph".show
        assertTrue(show == "x = ANY(ARRAY['a', 'b', 'c'])")

      test("in with SortedSet emits sorted order"):
        val ss   = scala.collection.immutable.SortedSet(3, 1, 2)
        val ph   = in(ss)
        val show = sql"x $ph".show
        assertTrue(show == "x = ANY(ARRAY[1, 2, 3])")

      test("in accepts a Range"):
        val ph = in(1 to 3)
        assertTrue(arrayLength(ph) == 3, ph.sql == "= ANY($1)")

      // === dedupe ===

      test("in deduplicates"):
        val ph   = in(List(1, 1, 2, 2, 3))
        val show = sql"x $ph".show
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 3, show == "x = ANY(ARRAY[1, 2, 3])")

      test("Placeholder.list dedupes single-element collapse"):
        val ph = Placeholder.list(List("a", "a"))
        assertTrue(ph.sql == "$1", arrayLength(ph) == 1)

      test("in with all-duplicates collapses to one and is NOT an error"):
        val ph = in(List(1, 1, 1))
        assertTrue(ph.sql == "= ANY($1)", arrayLength(ph) == 1, ph.issues.isEmpty)

      // === negative: empty (issue accumulation) ===

      test("in(empty) returns placeholder with EmptyCollection issue tagged 'in'"):
        val ph = in(List.empty[String])
        assertTrue(
          ph.issues.size == 1,
          ph.issues.exists {
            case FragmentIssue.EmptyCollection("in", origin) => origin.isDefined
            case _                                           => false
          },
        )

      test("Placeholder.list(empty) issue tagged 'Placeholder.list'"):
        val ph = Placeholder.list(List.empty[String])
        assertTrue(
          ph.issues.size == 1,
          ph.issues.exists {
            case FragmentIssue.EmptyCollection("Placeholder.list", _) => true
            case _                                                    => false
          },
        )

      test("in degenerate-empty-after-filter yields one issue"):
        val ph = in(List(1, 2, 3).filter(_ > 100))
        assertTrue(ph.issues.size == 1)

      test("fragment with one in(empty) carries one issue and validate fails"):
        val frag = sql"select * from t where x ${in(List.empty[Int])}"
        for result <- frag.validate.either
        yield assertTrue(
          frag.issues.size == 1,
          result match
            case Left(SaferisError.InvalidStatement(List(FragmentIssue.EmptyCollection("in", _)))) => true
            case _                                                                                 => false,
        )

      test("multi-issue accumulation: two empty in() splices yield two issues"):
        val frag =
          sql"select * from t where a ${in(List.empty[Int])} or b ${in(List.empty[Int])}"
        for result <- frag.validate.either
        yield assertTrue(
          frag.issues.size == 2,
          result match
            case Left(SaferisError.InvalidStatement(issues)) if issues.size == 2 =>
              issues.forall:
                case FragmentIssue.EmptyCollection("in", _) => true
                case _                                      => false
            case _ => false,
        )

      test("mix valid + invalid in() splices: writes from valid, issues from invalid"):
        val frag = sql"select * from t where a ${in(List(1, 2))} or b ${in(List.empty[Int])}"
        assertTrue(frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1, frag.issues.size == 1)

      test("origin frame is the caller, outside the saferis package"):
        val ph = in(List.empty[Int])
        assertTrue(ph.issues.exists {
          case FragmentIssue.EmptyCollection(_, Some(frame)) =>
            // Native stack traces omit the file name. The class is still the caller.
            val file = Option(frame.getFileName).forall(_.nonEmpty)
            file && !frame.getClassName.startsWith("saferis.")
          case _ => false
        })

      // === macro interaction: parameter ordering ===

      test("mixed splice keeps writes in argument order"):
        val name = "Bob"
        val ids  = List(10, 20, 30)
        val age  = 42
        val frag = sql"select * from t where a = $name and b ${in(ids)} and c = $age"
        assertTrue(
          frag.sql == "select * from t where a = $1 and b = ANY($2) and c = $3",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 3,
          frag.show == "select * from t where a = 'Bob' and b = ANY(ARRAY[10, 20, 30]) and c = 42",
        )

      test("two in() splices keep ordering across argument positions"):
        val xs   = List(1, 2)
        val ys   = List(3, 4, 5)
        val frag = sql"... a ${in(xs)} and b ${in(ys)}"
        assertTrue(
          frag.sql == "... a = ANY($1) and b = ANY($2)",
          frag.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2,
          frag.show == "... a = ANY(ARRAY[1, 2]) and b = ANY(ARRAY[3, 4, 5])",
        )

      test("SqlFragment.validate succeeds for valid fragments"):
        val frag = sql"select 1"
        for result <- frag.validate
        yield assertTrue(result == frag)

end InterpolatorSpecs
