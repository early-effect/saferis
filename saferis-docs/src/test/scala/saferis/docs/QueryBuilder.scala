package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import saferis.postgres.given
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object QueryBuilder extends SaferisDocSpecSuite:

  @tableName("qb_safety_users")
  case class SafetyUser(@generated @key id: Int, name: String) derives Table

  @tableName("qb_query_users")
  case class QueryUser(@generated @key id: Int, name: String, email: String, age: Int) derives Table

  val users = Table[QueryUser]

  @tableName("qb_join_users")
  case class JoinUser(@generated @key id: Int, name: String) derives Table

  @tableName("qb_join_orders")
  case class JoinOrder(@generated @key id: Int, userId: Int, amount: BigDecimal) derives Table

  @tableName("qb_join_items")
  case class JoinItem(@key orderId: Int, @key productId: Int, quantity: Int) derives Table

  @tableName("qb_page_articles")
  case class Article(@generated @key id: Long, title: String, views: Int, published: Boolean) derives Table

  val articles = Table[Article]

  @tableName("qb_exec_users")
  case class ExecUser(@generated @key id: Int, name: String) derives Table

  @tableName("qb_exec_orders")
  case class ExecOrder(@generated @key id: Int, userId: Int, amount: BigDecimal) derives Table

  val execUsers = Table[ExecUser]

  def doc = page("Query Builder")(
    md"""Saferis provides a unified, type-safe `Query` builder for constructing SQL queries. It supports single-table queries, multi-table joins (up to 5 tables), WHERE clauses, pagination, and subqueries - all with compile-time type safety.""",
    section("Query Safety (Builder/Ready Pattern)")(
      md"""To prevent accidental unbounded queries that could fetch millions of rows, Saferis uses a **Builder/Ready pattern**. A query must have at least one safety constraint before it can be executed:

| Safety Constraint | Description |
|------------------|-------------|
| `.where(...)` | Filter results with a WHERE clause |
| `.limit(n)` | Limit the number of rows returned |
| `.seekAfter(...)` / `.seekBefore(...)` | Cursor-based pagination |
| `.all` | Explicit opt-in to fetch all rows |""",
      exampleValue {
        // These compile - they have safety constraints:
        val withWhere = Query[SafetyUser].where(_.name).eq("Alice").build.sql // Has WHERE
        val withLimit = Query[SafetyUser].limit(100).build.sql // Has LIMIT
        val withAll   = Query[SafetyUser].all.build.sql // Explicit opt-in
        (withWhere, withLimit, withAll)
      }.assert { case (w, l, a) =>
        assertTrue(w.toLowerCase.contains("where"), l.toLowerCase.contains("limit"), a.toLowerCase.contains("select"))
      },
      md"""A query with no safety constraint cannot be built — `.build` simply doesn't exist
on a bare `Builder`, so the snippet below does not compile (the compiler error is
shown beneath it):""",
      expectFail("""
import saferis.*
import saferis.postgres.given

@tableName("qb_safety_users_fail")
case class SafetyUser(@generated @key id: Int, name: String) derives Table

// No WHERE / LIMIT / .all — .build is not available on a Builder.
Query[SafetyUser].build
""").assert(errs => assertTrue(errs.nonEmpty)),
      md"""The pattern ensures you consciously choose to query all rows with `.all` rather than accidentally doing so.""",
    ),
    section("Basic Queries")(
      md"""Start with `Query[A]` for single-table queries:""",
      exampleValue {
        // Simple query with type-safe WHERE
        Query[QueryUser]
          .where(_.name)
          .eq("Alice")
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
      exampleValue {
        // Query with ordering and pagination
        Query[QueryUser]
          .where(_.age)
          .gt(18)
          .orderBy(users.name.asc)
          .limit(10)
          .offset(20)
          .build
          .sql
      }.assert(sql =>
        assertTrue(sql.toLowerCase.contains("order") && sql.toLowerCase.contains("limit") && sql.toLowerCase.contains("offset")),
      ),
    ),
    section("Type-Safe WHERE Clauses")(
      md"""Use selector syntax for type-safe column references:""",
      exampleValue {
        // Equality
        Query[QueryUser].where(_.name).eq("Alice").build.sql
      }.assert(sql => assertTrue(sql.contains("?"))),
      exampleValue {
        // Comparison operators
        Query[QueryUser].where(_.age).gt(21).build.sql
      }.assert(sql => assertTrue(sql.contains("?"))),
      exampleValue {
        // IS NULL / IS NOT NULL
        Query[QueryUser].where(_.email).isNotNull().build.sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("null"))),
      md"""You can also use raw `SqlFragment` for complex conditions:""",
      exampleValue {
        // Raw SQL fragment
        Query[QueryUser]
          .where(sql"${users.age} BETWEEN 18 AND 65")
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("between"))),
    ),
    section("Joins")(
      md"""Chain joins with the fluent API. The `on()` method uses type-safe selectors:""",
      exampleValue {
        // Inner join (using .all to explicitly fetch all rows)
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("join"))),
      exampleValue {
        // Left join
        Query[JoinUser]
          .leftJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("left"))),
      exampleValue {
        // Right join
        Query[JoinUser]
          .rightJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("right"))),
      exampleValue {
        // Full join
        Query[JoinUser]
          .fullJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("full") || sql.toLowerCase.contains("join"))),
    ),
    section("Finalizing Joins with `.endJoin`")(
      md"""After specifying the ON clause, you can either:
1. Use **convenience methods** like `.where()`, `.limit()`, `.all` directly on the join chain
2. Call **`.endJoin`** explicitly to finalize the join and return to the query builder""",
      exampleValue {
        // Using convenience method (implicitly calls endJoin)
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .where(_.name)
          .eq("Alice") // Convenience method
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
      exampleValue {
        // Using explicit .endJoin for more control
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .endJoin // Explicitly finalize join
          .orderBy(Table[JoinUser].name.asc)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("order"))),
      md"""The `.endJoin` method is useful when you want to add operations like `.orderBy()` that aren't available as convenience methods on the join chain.""",
    ),
    section("Multi-Table Joins")(
      md"""Chain up to 5 tables. Use `onPrev()` to reference the previously joined table:""",
      exampleValue {
        // Three-table join
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .innerJoin[JoinItem]
          .onPrev(_.id)
          .eq(_.orderId)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("join"))),
    ),
    section("WHERE on Joined Queries")(
      md"""After joining, use `where()` for the first table or `whereFrom()` for joined tables:""",
      exampleValue {
        // WHERE on first table
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .where(_.name)
          .eq("Alice")
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
      exampleValue {
        // WHERE on joined table
        Query[JoinUser]
          .innerJoin[JoinOrder]
          .on(_.id)
          .eq(_.userId)
          .whereFrom(_.amount)
          .gt(BigDecimal(100))
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where"))),
    ),
    section("ON Clause Operators")(
      md"""All comparison operators are available in the ON clause:

| Method | SQL | Description |
|--------|-----|-------------|
| `eq()` | `=` | Equality |
| `neq()` | `<>` | Not equal |
| `lt()` | `<` | Less than |
| `lte()` | `<=` | Less than or equal |
| `gt()` | `>` | Greater than |
| `gte()` | `>=` | Greater than or equal |
| `isNull()` | `is null` | Null check |
| `isNotNull()` | `is not null` | Non-null check |
| `op(Operator.X)` | Custom | Any operator |""",
    ),
    section("Pagination")(
      section("Offset-Based Pagination")(
        md"""Traditional LIMIT/OFFSET pagination:""",
        exampleValue {
          // Page 3 with 10 items per page
          Query[Article]
            .where(_.published)
            .eq(true)
            .orderBy(articles.views.desc)
            .limit(10)
            .offset(20)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("limit") && sql.toLowerCase.contains("offset"))),
      ),
      section("Cursor/Seek Pagination")(
        md"""More efficient for large datasets - uses indexed lookups:""",
        exampleValue {
          // Get next page after ID 100
          Query[Article]
            .seekAfter(articles.id, 100L)
            .limit(10)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("limit"))),
        exampleValue {
          // Get previous page before ID 50
          Query[Article]
            .seekBefore(articles.id, 50L)
            .limit(10)
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("limit"))),
      ),
    ),
    section("Sorting")(
      md"""Use column extensions for concise sorting:""",
      exampleValue {
        Query[Article]
          .orderBy(articles.views.desc)
          .orderBy(articles.title.asc)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("order"))),
      md"""Control NULL ordering:""",
      exampleValue {
        Query[Article]
          .orderBy(articles.views.descNullsLast)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("null") || sql.toLowerCase.contains("order"))),
      md"""Available sorting extensions:
- `.asc` / `.desc` - basic ordering
- `.ascNullsFirst` / `.ascNullsLast`
- `.descNullsFirst` / `.descNullsLast`""",
    ),
    section("Executing Queries")(
      md"""Use `.query[R]` to execute and decode results:""",
      exampleZIO {
        xa.run(for
          _ <- ddl.createTable[ExecUser](ifNotExists = true)
          _ <- ddl.createTable[ExecOrder](ifNotExists = true)
          _ <- dml.insert(ExecUser(-1, "Alice"))
          _ <- dml.insert(ExecUser(-1, "Bob"))
          _ <- dml.insert(ExecOrder(-1, 1, BigDecimal(100)))
          _ <- dml.insert(ExecOrder(-1, 1, BigDecimal(200)))
          result <- Query[ExecUser]
            .innerJoin[ExecOrder]
            .on(_.id)
            .eq(_.userId)
            .where(_.name)
            .eq("Alice")
            .limit(10)
            .query[ExecUser]
        yield result).either
      }.assert {
        case Right(result) => assertTrue(result.exists(_.name == "Alice"))
        case Left(err)     => assertTrue(false).label(err.message)
      },
    ),
  )
end QueryBuilder
