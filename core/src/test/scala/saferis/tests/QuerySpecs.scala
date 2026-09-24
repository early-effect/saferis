package saferis.tests

import saferis.*
import zio.*
import zio.test.*

object QuerySpecs extends ZIOSpecDefault:

  private def arrayLength(fragment: SqlFragment): Int =
    fragment.pieces
      .collect:
        case SqlPiece.Param(SqlValue.Array(_, values)) => values.length
      .sum

  // Test tables
  @tableName("users")
  final case class User(@generated @key id: Int, name: String, email: String, age: Int) derives Table

  @tableName("orders")
  final case class Order(@generated @key id: Int, userId: Int, amount: BigDecimal, status: String) derives Table

  @tableName("products")
  final case class Product(@generated @key id: Int, name: String, price: BigDecimal) derives Table

  @tableName("order_items")
  final case class OrderItem(@key orderId: Int, @key productId: Int, quantity: Int) derives Table

  @tableName("categories")
  final case class Category(@generated @key id: Int, name: String) derives Table

  val spec = suite("Unified Query API")(
    suite("Query1 - Single table")(
      test("Query[A] creates aliased table query") {
        val q   = Query[User].all
        val sql = q.build.sql
        assertTrue(
          sql == "select * from users as users_ref_1"
        )
      },
      test("where with SqlFragment") {
        val q   = Query[User].where(sql"name = 'Alice'")
        val sql = q.build.sql
        assertTrue(
          sql.contains("where name = 'Alice'")
        )
      },
      test("type-safe where with eq") {
        val q   = Query[User].where(_.name).eq("Alice")
        val sql = q.build.sql
        assertTrue(
          sql.contains("where"),
          sql.contains("name ="),
        )
      },
      test("orderBy adds sort clause") {
        val users = Table[User]
        val q     = Query[User].orderBy(users.name.asc).all
        val sql   = q.build.sql
        assertTrue(
          sql.contains("order by name asc")
        )
      },
      test("limit and offset") {
        val q   = Query[User].limit(10).offset(20)
        val sql = q.build.sql
        assertTrue(
          sql.contains("limit 10"),
          sql.contains("offset 20"),
        )
      },
      test("seekAfter generates where and order by") {
        val users = Table[User]
        val q     = Query[User].seekAfter(users.id, 100)
        val sql   = q.build.sql
        assertTrue(
          sql.contains("where id > $1"),
          sql.contains("order by id asc"),
        )
      },
    ),
    suite("Query2 - Two tables joined")(
      test("innerJoin generates correct SQL") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("select * from users as users_ref_1"),
          sql.contains("inner join orders as orders_ref_1"),
          sql.contains("on users_ref_1.id = orders_ref_1.userId"),
        )
      },
      test("leftJoin generates correct SQL") {
        val q = Query[User]
          .leftJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("left join orders as orders_ref_1")
        )
      },
      test("rightJoin generates correct SQL") {
        val q = Query[User]
          .rightJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("right join orders as orders_ref_1")
        )
      },
      test("fullJoin generates correct SQL") {
        val q = Query[User]
          .fullJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("full join orders as orders_ref_1")
        )
      },
      test("join with where clause") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .where(_.name)
          .eq("Alice")
        val sql = q.build.sql
        assertTrue(
          sql.contains("inner join orders as orders_ref_1"),
          sql.contains("where"),
        )
      },
      test("join with limit and offset") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .limit(20)
          .offset(40)
        val sql = q.build.sql
        assertTrue(
          sql.contains("limit 20"),
          sql.contains("offset 40"),
        )
      },
      test("join with seekAfter") {
        val users = Table[User]
        val q     = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .endJoin
          .seekAfter(users.id, 100)
        val sql = q.build.sql
        assertTrue(
          sql.contains("where id > $1"),
          sql.contains("order by id asc"),
        )
      },
    ),
    suite("Query3 - Three tables joined")(
      test("chained joins generate correct SQL") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .eq(_.userId)
          .innerJoin[OrderItem]
          .onPrev(_.id)
          .eq(_.orderId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("inner join orders as orders_ref_1"),
          sql.contains("inner join order_items as order_items_ref_1"),
          sql.contains("on users_ref_1.id = orders_ref_1.userId"),
          sql.contains("on orders_ref_1.id = order_items_ref_1.orderId"),
        )
      }
    ),
    suite("ON clause operators")(
      test("neq generates <>") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .neq(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("on users_ref_1.id <> orders_ref_1.userId")
        )
      },
      test("lt generates <") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .lt(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("on users_ref_1.id < orders_ref_1.userId")
        )
      },
      test("lte generates <=") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .lte(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("on users_ref_1.id <= orders_ref_1.userId")
        )
      },
      test("gt generates >") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .gt(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("on users_ref_1.id > orders_ref_1.userId")
        )
      },
      test("gte generates >=") {
        val q = Query[User]
          .innerJoin[Order]
          .on(_.id)
          .gte(_.userId)
          .endJoin
          .all
        val sql = q.build.sql
        assertTrue(
          sql.contains("on users_ref_1.id >= orders_ref_1.userId")
        )
      },
    ),
    suite("WHERE clause")(
      test("type-safe where with neq") {
        val q   = Query[User].where(_.name).neq("Bob")
        val sql = q.build.sql
        assertTrue(sql.contains("where"), sql.contains("<>"))
      },
      test("type-safe where with lt") {
        val q   = Query[User].where(_.age).lt(30)
        val sql = q.build.sql
        assertTrue(sql.contains("where"), sql.contains("<"))
      },
      test("type-safe where with gt") {
        val q   = Query[User].where(_.age).gt(18)
        val sql = q.build.sql
        assertTrue(sql.contains("where"), sql.contains(">"))
      },
      test("type-safe where with isNull") {
        val q   = Query[User].where(_.email).isNull()
        val sql = q.build.sql
        assertTrue(sql.contains("is null"))
      },
      test("type-safe where with isNotNull") {
        val q   = Query[User].where(_.email).isNotNull()
        val sql = q.build.sql
        assertTrue(sql.contains("is not null"))
      },
    ),
    suite("IN Subqueries")(
      test("simple IN subquery") {
        val subquery = Query[Order].select(_.userId)
        val q        = Query[User].where(_.id).inSubquery(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("in (select userId from orders as orders_ref_1)")
        )
      },
      test("IN subquery with where clause") {
        val subquery = Query[Order].where(_.status).eq("paid").select(_.userId)
        val q        = Query[User].where(_.id).inSubquery(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("in (select userId from orders as orders_ref_1 where"),
          sql.contains("status ="),
        )
      },
      test("NOT IN subquery") {
        val subquery = Query[Order].select(_.userId)
        val q        = Query[User].where(_.id).notInSubquery(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("not in (select userId from orders as orders_ref_1)")
        )
      },
      test("inList is one array parameter") {
        val q = Query[User].where(_.id).inList(List(1, 2, 3)).build
        assertTrue(
          q.sql.contains("= ANY($1)"),
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 3,
          q.issues.isEmpty,
        )
      },
      test("notInList is one array parameter") {
        val q = Query[User].where(_.id).notInList(List(1, 2, 3)).build
        assertTrue(q.sql.contains("<> ALL($1)"), q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1)
      },
      test("in varargs is one array parameter") {
        val q = Query[User].where(_.name).in("active", "pending").build
        assertTrue(
          q.sql.contains("= ANY($1)"),
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 2,
        )
      },
      test("notIn varargs is one array parameter") {
        val q = Query[User].where(_.name).notIn("archived").build
        assertTrue(q.sql.contains("<> ALL($1)"), q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1)
      },
      test("inList with single element") {
        val q = Query[User].where(_.id).inList(List(42)).build
        assertTrue(q.sql.contains("= ANY($1)"), q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1)
      },
      test("inList accepts a Set") {
        val q = Query[User].where(_.id).inList(Set(1, 2, 3)).build
        assertTrue(
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 3,
          q.sql.contains("= ANY($1)"),
        )
      },
      test("inList accepts a Vector") {
        val q = Query[User].where(_.id).inList(Vector(1, 2, 3)).build
        assertTrue(
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 3,
          q.sql.contains("= ANY($1)"),
        )
      },
      test("inList deduplicates input") {
        val q = Query[User].where(_.id).inList(List(1, 1, 2)).build
        assertTrue(
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 2,
          q.sql.contains("= ANY($1)"),
        )
      },
      test("varargs in dedupes too") {
        val q = Query[User].where(_.id).in(1, 1, 2, 2, 3).build
        assertTrue(
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1,
          arrayLength(q) == 3,
          q.sql.contains("= ANY($1)"),
        )
      },
      test("inSubquery still works alongside the literal in/inList overloads") {
        val subquery = Query[Order].select(_.userId)
        val q        = Query[User].where(_.id).inSubquery(subquery).build
        assertTrue(q.sql.contains("in (select"), q.issues.isEmpty)
      },
      test("inList mixed with other where predicates preserves parameter order") {
        val q = Query[User].where(_.name).eq("Bob").where(_.id).inList(List(1, 2, 3)).build
        assertTrue(
          q.sql.contains("name = $1"),
          q.sql.contains("= ANY($2)"),
          q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2,
        )
      },
      test("two inList calls on different columns accumulate writes") {
        val q = Query[User].where(_.id).inList(List(1, 2)).where(_.name).inList(List("a", "b", "c")).build
        assertTrue(q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 2, arrayLength(q) == 5)
      },
      test("inList(empty) does not throw — fragment carries one issue tagged WhereBuilder.inList") {
        val q = Query[User].where(_.id).inList(List.empty[Int]).build
        assertTrue(
          q.issues.size == 1,
          q.issues.exists {
            case FragmentIssue.EmptyCollection("WhereBuilder.inList", _) => true
            case _                                                       => false
          },
        )
      },
      test("notInList(empty) carries one issue tagged WhereBuilder.notInList") {
        val q = Query[User].where(_.id).notInList(List.empty[Int]).build
        assertTrue(
          q.issues.size == 1,
          q.issues.exists {
            case FragmentIssue.EmptyCollection("WhereBuilder.notInList", _) => true
            case _                                                          => false
          },
        )
      },
      test("all-duplicates collapsing to one is NOT an error") {
        val q = Query[User].where(_.id).inList(List(7, 7, 7)).build
        assertTrue(q.issues.isEmpty, q.pieces.count(_.isInstanceOf[SqlPiece.Param]) == 1, q.sql.contains("= ANY($1)"))
      },
      test("two empty inList calls produce two accumulated issues") {
        val q = Query[User].where(_.id).inList(List.empty[Int]).where(_.name).inList(List.empty[String]).build
        assertTrue(q.issues.size == 2)
      },
      test("SqlFragment.validate succeeds for a valid query") {
        val q = Query[User].where(_.id).inList(List(1, 2)).build
        for r <- q.validate
        yield assertTrue(r == q)
      },
      test("SqlFragment.validate fails with InvalidStatement for an empty inList") {
        val q = Query[User].where(_.id).inList(List.empty[Int]).build
        for r <- q.validate.either
        yield assertTrue(r match
          case Left(SaferisError.InvalidStatement(issues)) if issues.size == 1 => true
          case _                                                               => false)
      },
      test("select modifies query to select specific column") {
        val q   = Query[User].select(_.id)
        val sql = q.build.sql
        assertTrue(
          sql.startsWith("select id from users")
        )
      },
    ),
    suite("EXISTS Subqueries")(
      test("simple EXISTS subquery") {
        val subquery = Query[Order].all
        val q        = Query[User].whereExists(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("exists (select * from orders as orders_ref_1)")
        )
      },
      test("EXISTS subquery with where clause") {
        val subquery = Query[Order].where(_.status).eq("paid")
        val q        = Query[User].whereExists(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("exists (select * from orders as orders_ref_1 where")
        )
      },
      test("NOT EXISTS subquery") {
        val subquery = Query[Order].all
        val q        = Query[User].whereNotExists(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("not exists (select * from orders as orders_ref_1)")
        )
      },
      test("correlated EXISTS using sql interpolation") {
        val users    = Table[User]
        val subquery = Query[Order].where(sql"userId = ${users.id}")
        val q        = Query[User].whereExists(subquery)
        val sql      = q.build.sql
        assertTrue(
          sql.contains("exists"),
          sql.contains("userId = id"),
        )
      },
    ),
    suite("Derived Tables")(
      test("selectAll creates typed query for derived table") {
        // Virtual type matching subquery result shape
        @tableName("order_summary")
        final case class OrderSummary(userId: Int, status: String) derives Table

        val subquery = Query[Order]
          .where(_.amount)
          .gt(BigDecimal(100))
          .selectAll[OrderSummary]

        // SelectQuery wraps the original query
        val sql = subquery.build.sql
        assertTrue(
          sql.contains("select * from orders as orders_ref_1"),
          sql.contains("where"),
        )
      },
      test("Query.from creates derived table query") {
        @tableName("paid_orders")
        final case class PaidOrder(userId: Int, amount: BigDecimal, status: String) derives Table

        val subquery = Query[Order]
          .where(_.status)
          .eq("paid")
          .selectAll[PaidOrder]

        val q   = Query.from(subquery, "paid_summary").all
        val sql = q.build.sql
        assertTrue(
          sql.contains("select * from (select * from orders as orders_ref_1 where"),
          sql.contains(") as paid_summary"),
        )
      },
      test("derived table with where clause") {
        @tableName("high_value")
        final case class HighValueOrder(userId: Int, amount: BigDecimal) derives Table

        val subquery = Query[Order]
          .where(_.amount)
          .gt(BigDecimal(1000))
          .selectAll[HighValueOrder]

        val q = Query
          .from(subquery, "high_value")
          .where(_.userId)
          .gt(0)

        val sql = q.build.sql
        assertTrue(
          sql.contains("(select * from orders as orders_ref_1 where"),
          sql.contains(") as high_value"),
          sql.contains("high_value.userId >"),
        )
      },
      test("derived table with join") {
        @tableName("order_totals")
        final case class OrderTotal(userId: Int, amount: BigDecimal) derives Table

        val subquery = Query[Order]
          .where(_.status)
          .eq("completed")
          .selectAll[OrderTotal]

        val q = Query
          .from(subquery, "totals")
          .innerJoin[User]
          .on(_.userId)
          .eq(_.id)
          .endJoin
          .all

        val sql = q.build.sql
        assertTrue(
          sql.contains("(select * from orders as orders_ref_1 where"),
          sql.contains(") as totals"),
          sql.contains("inner join users as users_ref_1"),
          sql.contains("totals.userId = users_ref_1.id"),
        )
      },
    ),
    suite("Schema-qualified table names")(
      test("schema-qualified tableName produces valid alias (no dots)") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        val sql = Query[PrdFoo].all.build.sql
        assertTrue(
          sql == "select * from prd.foo as foo_ref_1",
          !sql.contains("prd.foo_ref_1"),
        )
      },
      test("schema-qualified tableName produces valid column reference in where") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        val sql = Query[PrdFoo].where(_.name).eq("a").build.sql
        assertTrue(
          sql.contains("foo_ref_1.name"),
          !sql.contains("prd.foo_ref_1"),
        )
      },
      test("same bare name in different schemas get distinct, valid aliases") {
        @tableName("prd.user_acct")
        final case class PrdUser(@key id: Long, name: String) derives Table

        @tableName("staging.user_acct")
        final case class StagingUser(@key id: Long, name: String) derives Table

        val sql = Query[PrdUser]
          .innerJoin[StagingUser]
          .on(_.id)
          .eq(_.id)
          .endJoin
          .all
          .build
          .sql
        assertTrue(
          sql.contains("from prd.user_acct as user_acct_ref_1"),
          sql.contains("inner join staging.user_acct as user_acct_ref_2"),
          sql.contains("user_acct_ref_1.id = user_acct_ref_2.id"),
          !sql.contains("prd.user_acct_ref"),
          !sql.contains("staging.user_acct_ref"),
        )
      },
      test("isNull / isNotNull produce valid column refs (UnaryCondition path)") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: Option[String]) derives Table

        val isNullSql    = Query[PrdFoo].where(_.name).isNull().build.sql
        val isNotNullSql = Query[PrdFoo].where(_.name).isNotNull().build.sql
        assertTrue(
          isNullSql.contains("foo_ref_1.name is null"),
          isNotNullSql.contains("foo_ref_1.name is not null"),
          !isNullSql.contains("prd.foo_ref_1"),
          !isNotNullSql.contains("prd.foo_ref_1"),
        )
      },
      test("orderBy uses valid alias-qualified column ref") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        val foo = Table[PrdFoo]
        val sql = Query[PrdFoo].orderBy(foo.name.asc).all.build.sql
        assertTrue(
          // orderBy uses the unaliased column from Table[A], so the assertion is just
          // that nothing in the emitted SQL includes the dotted-alias bug.
          !sql.contains("prd.foo_ref_1"),
          sql.contains("order by"),
        )
      },
      test("select(_.col) for subqueries emits clean SQL (no dotted alias in FROM)") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        // Note: select(_.col) emits the bare column label in the SELECT list
        // (matches existing behavior in IN subquery tests).
        val sub = Query[PrdFoo].select(_.id)
        val sql = sub.build.sql
        assertTrue(
          sql == "select id from prd.foo as foo_ref_1",
          !sql.contains("prd.foo_ref_1"),
        )
      },
      test("seekAfter on schema-qualified table produces valid SQL") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        val foo = Table[PrdFoo]
        val sql = Query[PrdFoo].seekAfter(foo.id, 100L).build.sql
        assertTrue(
          !sql.contains("prd.foo_ref_1"),
          sql.contains("where"),
          sql.contains("order by"),
        )
      },
      test("3-table join with schema-qualified names emits valid aliases everywhere") {
        @tableName("prd.aaa")
        final case class A(@key id: Long, name: String) derives Table

        @tableName("prd.bbb")
        final case class B(@key id: Long, aId: Long) derives Table

        @tableName("staging.aaa")
        final case class C(@key id: Long, bId: Long) derives Table

        val sql = Query[A]
          .innerJoin[B]
          .on(_.id)
          .eq(_.aId)
          .innerJoin[C]
          .onPrev(_.id)
          .eq(_.bId)
          .endJoin
          .all
          .build
          .sql
        assertTrue(
          sql.contains("from prd.aaa as aaa_ref_1"),
          sql.contains("inner join prd.bbb as bbb_ref_1"),
          // staging.aaa shares the bare name "aaa" with prd.aaa, so its counter is 2
          sql.contains("inner join staging.aaa as aaa_ref_2"),
          sql.contains("aaa_ref_1.id = bbb_ref_1.aId"),
          sql.contains("bbb_ref_1.id = aaa_ref_2.bId"),
          !sql.contains("prd.aaa_ref"),
          !sql.contains("prd.bbb_ref"),
          !sql.contains("staging.aaa_ref"),
        )
      },
      test("raw sql interpolation with schema-qualified column references") {
        @tableName("prd.foo")
        final case class PrdFoo(@key id: Long, name: String) derives Table

        // Column.sql is also used when a Column is interpolated into a raw sql"..." fragment.
        // For Table[A] the alias is None so columns emit just their label — verify that path
        // doesn't accidentally pick up the schema prefix.
        val foo = Table[PrdFoo]
        val sql = sql"select ${foo.name} from $foo".sql
        assertTrue(
          !sql.contains("prd.foo_ref"),
          sql.contains("name"),
          sql.contains("prd.foo"),
        )
      },
    ),
  ) @@ TestAspect.sequential
end QuerySpecs
