package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import saferis.postgres.given
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Subqueries extends SaferisDocSpecSuite:

  @tableName("sub_users")
  case class SubUser(@generated @key id: Int, name: String) derives Table

  @tableName("sub_orders")
  case class SubOrder(@generated @key id: Int, userId: Int, status: String) derives Table

  @tableName("sub_derived_orders")
  case class DerivedOrder(@generated @key id: Int, userId: Int, amount: BigDecimal, status: String) derives Table

  @tableName("sub_derived_users")
  case class DerivedUser(@generated @key id: Int, name: String) derives Table

  // Virtual type for the subquery result
  @tableName("sub_order_summary")
  case class OrderSummary(userId: Int, amount: BigDecimal) derives Table

  @tableName("sub_complex_users")
  case class ComplexUser(@generated @key id: Int, name: String) derives Table

  @tableName("sub_complex_orders")
  case class ComplexOrder(@generated @key id: Int, userId: Int, productId: Int) derives Table

  @tableName("sub_complex_products")
  case class ComplexProduct(@generated @key id: Int, category: String) derives Table

  @tableName("sub_timeout_rows")
  case class TimeoutRow(
      @generated @key id: Int,
      deadline: java.time.Instant,
      claimedBy: Option[String],
      claimedUntil: Option[java.time.Instant],
  ) derives Table

  def doc = page("Subqueries")(
    md"""The Query builder supports type-safe subqueries for IN, NOT IN, EXISTS, and derived tables.""",
    section("IN Subqueries")(
      md"""Use `.select(_.column)` to create a typed subquery, then pass it to `.in()`:""",
      exampleValue {
        // Type-safe IN subquery - column types must match
        val activeUserIds = Query[SubOrder]
          .where(_.status)
          .eq("active")
          .select(_.userId) // Returns SelectQuery[Int]

        Query[SubUser]
          .where(_.id)
          .inSubquery(activeUserIds) // Compiles: both are Int
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("in") && sql.contains("("))),
      exampleValue {
        val activeUserIds = Query[SubOrder]
          .where(_.status)
          .eq("active")
          .select(_.userId)

        // NOT IN subquery
        Query[SubUser]
          .where(_.id)
          .notInSubquery(activeUserIds)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("not in") || sql.toLowerCase.contains("not"))),
      md"""The type safety is enforced at compile time — if the column types don't match, it
won't compile. The snippet below tries to match an `Int` column against a
`SelectQuery[String]`; the real compiler error is shown beneath it:""",
      expectFail("""
import saferis.*
import saferis.postgres.given

@tableName("sub_users_fail")
case class SubUser(@generated @key id: Int, name: String) derives Table

@tableName("sub_orders_fail")
case class SubOrder(@generated @key id: Int, userId: Int, status: String) derives Table

// .select(_.status) is SelectQuery[String]; _.id is an Int column — mismatch.
val statuses = Query[SubOrder].where(_.userId).gt(0).select(_.status)
Query[SubUser].where(_.id).inSubquery(statuses).build.sql
""").assert(errs => assertTrue(errs.nonEmpty)),
    ),
    section("IN Literal Collections")(
      md"""For IN-clauses over runtime values (not subqueries), use `in` (varargs) for inline literals or `inList` for any
`Iterable[T]`. Both work in the typed Query DSL and via the top-level `in(...)` helper inside `sql"..."` interpolation.""",
      exampleValue {
        // Varargs form — natural for inline literals
        Query[SubUser]
          .where(_.name)
          .in("Alice", "Bob")
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("in"))),
      exampleValue {
        // Iterable form — for runtime collections (List, Set, Vector, LinkedHashSet, ranges, ...)
        val ids = List(1, 2, 3, 3) // duplicates are removed automatically
        Query[SubUser]
          .where(_.id)
          .inList(ids)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("in"))),
      exampleValue {
        // Same helpers in raw sql"..." interpolation:
        val ids = List(1, 2, 3)
        val a   = sql"select * from sub_users where id in ${in(ids)}".sql
        val b   = sql"select * from sub_users where name in ${in("Alice", "Bob")}".sql
        (a, b)
      }.assert { case (a, b) => assertTrue(a.toLowerCase.contains("in") && b.toLowerCase.contains("in")) },
      md"""`notIn` / `notInList` are symmetric. `inSubquery` / `notInSubquery` (renamed from `in`/`notIn` on `SelectQuery`) cover
the subquery case shown above.""",
      section("Empty collections fail at construction, not in the database")(
        md"""An empty (or all-duplicates-collapse-to-empty) input would produce invalid SQL (`IN ()`) on every supported dialect.
Saferis does **not** throw at the call site — instead the resulting fragment carries one
`FragmentIssue.EmptyCollection` per offending helper. When the fragment is run, execution fails with
`SaferisError.InvalidStatement(issues)` *before* any JDBC call.

The recommended recovery pattern catches `InvalidStatement` and substitutes an empty result — no DB round-trip:""",
        exampleZIO {
          // Recovery pattern for possibly-empty collections — no DB round-trip on failure:
          val ids2      = List.empty[Int]
          val recovered =
            xa.run(Query[SubUser].where(_.id).inList(ids2).query[SubUser])
              .catchSome { case _: SaferisError.InvalidStatement => ZIO.succeed(Chunk.empty[SubUser]) }
          recovered.either
        }.assert {
          case Right(rows) => assertTrue(rows.isEmpty)
          case Left(err)   => assertTrue(false).label(err.toString)
        },
        md"""Without that recovery, running the empty-collection query fails with
`SaferisError.InvalidStatement` — and crucially it fails *before* any JDBC call.
The example below inspects the failure to confirm both facts (the output is shown
beneath it):""",
        exampleZIO {
          // Empty collection → IN () would be invalid SQL → InvalidStatement at run time.
          xa.run(Query[SubUser].where(_.id).inList(List.empty[Int]).query[SubUser]).either.map {
            case Left(_: SaferisError.InvalidStatement) => "Failed with InvalidStatement (no JDBC call made)"
            case Left(other)                            => s"Failed with ${other.getClass.getSimpleName}"
            case Right(rows)                            => s"Unexpectedly succeeded with ${rows.size} rows"
          }
        }.assert(msg => assertTrue(msg.contains("InvalidStatement"))),
        md"""If you want to surface issues independently of execution, call `fragment.validate` on any `SqlFragment` — it succeeds
with the fragment if there are no issues, fails with `InvalidStatement(issues)` otherwise. Multiple offending splices
in a single statement accumulate into the same `InvalidStatement`, so you fix them all at once.""",
      ),
    ),
    section("EXISTS Subqueries")(
      md"""Use `whereExists()` or `whereNotExists()`:""",
      exampleValue {
        // EXISTS - find users who have orders
        Query[SubUser]
          .whereExists(Query[SubOrder].all)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("exists"))),
      exampleValue {
        // NOT EXISTS - find users without orders
        Query[SubUser]
          .whereNotExists(Query[SubOrder].where(_.status).eq("cancelled"))
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("exists"))),
    ),
    section("Correlated Subqueries")(
      md"""For correlated subqueries, use `sql"..."` to reference outer table columns:""",
      exampleValue {
        val users = Table[SubUser]

        // Correlated EXISTS - find users who have at least one order
        Query[SubUser]
          .whereExists(
            Query[SubOrder].where(sql"userId = ${users.id}")
          )
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("exists"))),
    ),
    section("Derived Tables")(
      md"""Use subqueries in the FROM clause with `Query.from()`:""",
      exampleValue {
        // Create a typed subquery
        val highValueOrders = Query[DerivedOrder]
          .where(_.amount)
          .gt(BigDecimal(100))
          .selectAll[OrderSummary] // Returns SelectQuery[OrderSummary]

        // Use as derived table with explicit alias
        Query
          .from(highValueOrders, "high_value")
          .where(_.userId)
          .gt(0)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("select"))),
      exampleValue {
        val highValueOrders = Query[DerivedOrder]
          .where(_.amount)
          .gt(BigDecimal(100))
          .selectAll[OrderSummary]

        // Derived table with join
        Query
          .from(highValueOrders, "summary")
          .innerJoin[DerivedUser]
          .on(_.userId)
          .eq(_.id)
          .all
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("join"))),
    ),
    section("Complex Nested Subqueries")(
      md"""Subqueries can be arbitrarily complex - they support joins, nested subqueries, and all Query features:""",
      exampleValue {
        // Nested subquery: find users who ordered electronics
        val electronicProductIds = Query[ComplexProduct]
          .where(_.category)
          .eq("electronics")
          .select(_.id)

        val usersWithElectronics = Query[ComplexOrder]
          .where(_.productId)
          .inSubquery(electronicProductIds)
          .select(_.userId)

        Query[ComplexUser]
          .where(_.id)
          .inSubquery(usersWithElectronics)
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("in"))),
    ),
    section("Operator Reference")(
      md"""All available operators in `Operator`:

| Operator | SQL | Notes |
|----------|-----|-------|
| `Eq` | `=` | Standard equality |
| `Neq` | `<>` | Standard inequality |
| `Lt` | `<` | Less than |
| `Lte` | `<=` | Less than or equal |
| `Gt` | `>` | Greater than |
| `Gte` | `>=` | Greater than or equal |
| `Like` | `like` | Pattern matching |
| `ILike` | `ilike` | Case-insensitive LIKE (PostgreSQL) |
| `SimilarTo` | `similar to` | Regex pattern (PostgreSQL) |
| `RegexMatch` | `~` | Regex match (PostgreSQL) |
| `RegexMatchCI` | `~*` | Case-insensitive regex (PostgreSQL) |
| `IsNull` | `is null` | Null check |
| `IsNotNull` | `is not null` | Non-null check |"""
    ),
    section("Complex WHERE with OR and Grouping")(
      md"""For queries with complex OR logic, use `andWhere` with a lambda:""",
      exampleValue {
        // Find rows that are due AND either unclaimed or with expired claims
        val now = java.time.Instant.now()
        Query[TimeoutRow]
          .where(_.deadline)
          .lte(now)
          .andWhere(w => w(_.claimedBy).isNull.or(_.claimedUntil).lt(Some(now)))
          .build
          .sql
      }.assert(sql => assertTrue(sql.toLowerCase.contains("where") && sql.toLowerCase.contains("or"))),
      md"""This generates: `select ... where deadline <= ? and (claimed_by is null or claimed_until < ?)`

The parentheses are automatically added around the grouped conditions.""",
      section("Building Complex Conditions")(
        md"""The `andWhere` lambda provides a fluent builder:""",
        exampleValue {
          // Multiple OR conditions
          val now = java.time.Instant.now()
          Query[TimeoutRow]
            .where(_.id)
            .gt(0)
            .andWhere(w =>
              w(_.claimedBy).isNull
                .or(_.claimedUntil)
                .lt(Some(now))
                .or(_.deadline)
                .gt(now)
            )
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("or"))),
        exampleValue {
          // AND within the group
          val now = java.time.Instant.now()
          Query[TimeoutRow]
            .where(_.id)
            .gt(0)
            .andWhere(w =>
              w(_.claimedBy).isNotNull
                .and(_.claimedUntil)
                .gt(Some(now))
            )
            .build
            .sql
        }.assert(sql => assertTrue(sql.toLowerCase.contains("and"))),
        md"""Available operations in the `andWhere` builder:

| Method | Description |
|--------|-------------|
| `w(_.column)` | Start condition on a column |
| `.eq(value)` | Equality check |
| `.neq(value)` | Not equal |
| `.lt(value)` / `.lte(value)` | Less than (or equal) |
| `.gt(value)` / `.gte(value)` | Greater than (or equal) |
| `.isNull` / `.isNotNull` | Null checks |
| `.or(_.column)` | Chain with OR |
| `.and(_.column)` | Chain with AND |""",
      ),
    ),
  )
end Subqueries
