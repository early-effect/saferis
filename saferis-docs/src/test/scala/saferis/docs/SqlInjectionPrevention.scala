package saferis.docs

import saferis.*
import saferis.Schema.*
import saferis.postgres.PostgresDialect
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object SqlInjectionPrevention extends SaferisDocSpecSuite:

  @tableName("sql_injection_users")
  case class User(@generated @key id: Int, name: String, email: String) derives Table

  val users = Table[User]

  @tableName("sql_injection_profiles")
  case class Profile(@key id: Int, name: String, data: Json[Map[String, String]]) derives Table

  def doc = page("SQL Injection Prevention")(
    md"""Saferis is designed from the ground up to prevent SQL injection at multiple levels. This section explains the complete security model.

## The Three Layers of Protection

1. **Parameterized Values**: User data is always bound via prepared statements, never concatenated into SQL.
2. **Compile-Time Literal Enforcement**: Table aliases must be string literals known at compile time.
3. **Runtime Escaping**: When runtime identifiers are unavoidable, proper escaping is applied.

## How the `sql"..."` Interpolator Works

The interpolator analyzes each interpolated expression at compile time and routes it appropriately:""",
    exampleValue {
      val userName = "Alice"
      sql"SELECT * FROM $users WHERE ${users.name} = $userName".sql
    }.assert(sql => assertTrue(sql == "SELECT * FROM sql_injection_users WHERE name = ?")),
    md"""The generated SQL uses a `?` placeholder, and the actual value is bound separately. It never touches the SQL string. Even malicious input is harmless:""",
    exampleValue {
      val malicious = "'; DROP TABLE sql_injection_users; --"
      sql"SELECT * FROM $users WHERE ${users.name} = $malicious".sql
    }.assert(sql => assertTrue(sql == "SELECT * FROM sql_injection_users WHERE name = ?")),
    md"""The malicious string becomes a parameter value, not part of the SQL syntax. (`.show`, used elsewhere in these docs, inlines the bound values for debugging, but it is **not** what gets sent to the database.)

## Table Aliases: Compile-Time Literal Enforcement

Table aliases appear directly in SQL, not as parameters, so they could be injection vectors. Saferis prevents this with a **macro that enforces string literals at compile time**:""",
    exampleValue {
      val u1 = Table[User]("u")
      val u2 = Table[User] as "users"
      val a  = Alias("my_alias")
      s"${u1.tableName}, ${u2.tableName}, ${a.value}"
    }.assert(result => assertTrue(result.nonEmpty)),
    md"""Try to build an alias from a *variable* and the code does not compile. The **actual compiler error** is shown below the snippet, proving the guard is real and not just documentation:""",
    expectFail("""
import saferis.*
@tableName("sql_injection_bad_alias_users")
case class User(@key id: Int, name: String) derives Table
val alias = "u"
val bad = Alias(alias)
""").assert(errs => assertTrue(errs.nonEmpty)),
    md"""The compile error guides you to the safe alternative: for runtime identifiers, use `Placeholder.identifier()` or `dialect.escapeIdentifier()` instead.

This compile-time enforcement means SQL injection via aliases is **impossible**. There is no runtime path for user input to become an alias.

## Runtime Identifiers with `Placeholder.identifier()`

Sometimes you genuinely need runtime-determined identifiers, for example dynamic column names from configuration. For these cases, use `Placeholder.identifier()` which applies proper escaping:""",
    exampleValue {
      val columnName = "name"
      sql"SELECT ${Placeholder.identifier(columnName)} FROM $users".show
    }.assert(sql => assertTrue(sql == """SELECT "name" FROM sql_injection_users""")),
    md"""The identifier is escaped using the dialect's quoting rules. For PostgreSQL, this means double-quote escaping:""",
    exampleValue(PostgresDialect.escapeIdentifier("table"))
      .assert(value => assertTrue(value == "\"table\"")),
    exampleValue(PostgresDialect.escapeIdentifier("user\"input"))
      .assert(value => assertTrue(value == "\"user\"\"input\"")),
    md"""**Important**: While `Placeholder.identifier()` escapes properly, you should still validate runtime identifiers against an allowlist when possible. Escaping is a defense-in-depth measure, not a replacement for input validation.

## The `Placeholder.raw()` Escape Hatch

For rare cases where you need to embed literal SQL, for example database-specific syntax not supported by Saferis, use `Placeholder.raw()`:""",
    exampleValue {
      val trustedSql = "CURRENT_TIMESTAMP"
      sql"SELECT ${Placeholder.raw(trustedSql)} as now".show
    }.assert(sql => assertTrue(sql == "SELECT CURRENT_TIMESTAMP as now")),
    md"""⚠️ **Warning**: `Placeholder.raw()` bypasses all safety mechanisms. Only use it with:

- Hardcoded strings in your source code
- Values from trusted configuration (never user input)
- SQL syntax that Saferis doesn't support natively

Never pass user input to `Placeholder.raw()`.

## JSON Operations: Automatic Escaping

When using JSON operations in the Schema DSL or dialect methods, Saferis automatically escapes single quotes to prevent injection:""",
    exampleValue(PostgresDialect.jsonHasKeySql("data", "user's_key"))
      .assert(sql => assertTrue(sql == "jsonb_exists(data, 'user''s_key')")),
    exampleValue(PostgresDialect.jsonHasKeySql("data", "'); DROP TABLE sql_injection_profiles; --"))
      .assert(sql => assertTrue(sql == "jsonb_exists(data, '''); DROP TABLE sql_injection_profiles; --')")),
    md"""The single quote in the injection attempt is escaped to `''`, rendering it harmless.

## Security Summary

| Mechanism | What It Protects | How It Works |
|-----------|------------------|--------------|
| Parameterized queries | User data values | Bound via `?` placeholders, never in SQL string |
| Alias macro | Table aliases | Compile-time string literal enforcement |
| `Placeholder.identifier()` | Runtime column/table names | Dialect-specific identifier escaping |
| JSON escaping | JSON keys and paths | Automatic single-quote escaping |
| `Placeholder.raw()` | Escape hatch | Developer takes responsibility |

## Best Practices

1. **Use the `sql"..."` interpolator** for all queries: it handles parameterization automatically.
2. **Use literal strings** for table aliases: the compiler enforces this.
3. **Validate runtime identifiers** against an allowlist before using `Placeholder.identifier()`.
4. **Never use `Placeholder.raw()`** with user input.
5. **Prefer the Query builder** for dynamic queries: it's type-safe end-to-end.""",
  )
end SqlInjectionPrevention
