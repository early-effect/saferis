package saferis.docs

import saferis.*
import saferis.docs.DocsTransactor.xa
import saferis.postgres.given
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.test.*

object Capabilities extends SaferisDocSpecSuite:

  @tableName("capabilities_specialized_items")
  case class SpecializedItem(@generated @key id: Int, name: String, category: String) derives Table

  @tableName("capabilities_pg_items")
  case class PgItem(@generated @key id: Int, name: String) derives Table

  @tableName("capabilities_returning_items")
  case class ReturningItem(@generated @key id: Int, name: String) derives Table

  def updateAndReturn(name: String)(using Dialect & ReturningSupport): String =
    Update[ReturningItem].set(_.name, name).where(_.id).eq(1).returningAs.build.sql

  def doc = page("Type-Safe Capabilities")(
    md"""Saferis uses Scala 3's type system to ensure operations are only available when the database supports them.

## Capability Traits

Each dialect mixes in capability traits that enable specific operations:

| Trait | Operations Enabled |
|-------|-------------------|
| `ReturningSupport` | `insertReturning`, `updateReturning`, `deleteReturning` |
| `JsonSupport` | JSON query ops (`.where(_.col).jsonContains/jsonHasKey/jsonPath`), JSON type mappings |
| `ArraySupport` | array containment queries, array type mappings |
| `UpsertSupport` | `Upsert` DSL (`Upsert[A].values(...).onConflict(_.col)`) |
| `IndexIfNotExistsSupport` | Conditional index creation |

## Using SpecializedDML

The `SpecializedDML` object provides type-safe operations that only compile when the dialect supports them:""",
    exampleZIO {
      xa.run(for
        _        <- ddl.createTable[SpecializedItem](ifNotExists = true)
        inserted <- dml.insertReturning(SpecializedItem(-1, "Widget", "hardware"))
        _        <- dml.insert(SpecializedItem(-1, "Gadget", "electronics"))
        all      <- sql"SELECT * FROM ${Table[SpecializedItem]}".query[SpecializedItem]
      yield (inserted, all))
        .either
    }.assert {
      case Right((inserted, all)) => assertTrue(inserted.name == "Widget" && all.size >= 2)
      case Left(err)              => assertTrue(false).label(err.message)
    },
    md"""## Compile-Time Safety

Capabilities are encoded in the dialect's type. You can ask the compiler to prove a dialect supports a capability with a type ascription on `summon[Dialect]`. Each line below only compiles because PostgreSQL actually mixes in that capability:""",
    exampleValue {
      val _: Dialect & ReturningSupport        = summon[Dialect]
      val _: Dialect & JsonSupport             = summon[Dialect]
      val _: Dialect & ArraySupport            = summon[Dialect]
      val _: Dialect & UpsertSupport           = summon[Dialect]
      val _: Dialect & IndexIfNotExistsSupport = summon[Dialect]
      "PostgreSQL provides every documented capability"
    }.assert(message => assertTrue(message.contains("every"))),
    md"""Operations that require a capability take a `using Dialect & SomeSupport` parameter. Because the default dialect (PostgreSQL) provides every capability, `returningAs` compiles out of the box:""",
    exampleValue(Update[PgItem].set(_.name, "x").where(_.id).eq(1).returningAs.build.sql)
      .assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
    md"""Switch to a dialect that lacks a capability, for example a SQLite-only program that tries an `Upsert` (SQLite has no `UpsertSupport`), and the operation no longer typechecks. The constraint is part of the method signature, so the mismatch is caught at compile time rather than failing against the database at runtime.

Available capability-constrained operations in `SpecializedDML`:

| Operation | Required Capability |
|-----------|-------------------|
| `insertReturning` | `ReturningSupport` |
| `updateReturning` | `ReturningSupport` |
| `deleteReturning` | `ReturningSupport` |
| `Upsert` DSL (`onConflict(_.col)`) | `UpsertSupport` |
| JSON query ops (`.where(_.col).jsonContains`/`jsonHasKey`/`jsonPath`) | `JsonSupport` |
| array containment queries | `ArraySupport` |

## Generic Functions with Capability Constraints

Write functions that require specific capabilities via `using` constraints. The constraint propagates to every caller, so a function that needs RETURNING can only be called where the dialect provides it:""",
    exampleValue(updateAndReturn("x"))
      .assert(sql => assertTrue(sql.toLowerCase.contains("returning"))),
    md"""We've already seen this in action: `insertReturning` works because PostgreSQL provides `ReturningSupport`:""",
    exampleZIO {
      xa.run(for
        _        <- ddl.createTable[SpecializedItem](ifNotExists = true)
        returned <- dml.insertReturning(SpecializedItem(-1, "Capability Demo", "demo"))
      yield returned)
        .either
    }.assert {
      case Right(returned) => assertTrue(returned.name == "Capability Demo")
      case Left(err)       => assertTrue(false).label(err.message)
    },
  )
end Capabilities
