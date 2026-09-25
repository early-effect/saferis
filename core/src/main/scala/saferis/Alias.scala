package saferis

import scala.quoted.*

/** Type-safe representation of SQL table aliases.
  *
  * Aliases are used to qualify column references in SQL queries. The `Alias.apply` constructor is a macro that only
  * accepts string literals - this guarantees at compile time that aliases cannot contain SQL injection vectors.
  *
  * ==Compile-time Safety==
  * {{{
  *   Alias("users")           // OK - string literal
  *   Alias("u")               // OK - string literal
  *   val s = "users"
  *   Alias(s)                 // COMPILE ERROR - not a literal
  *   Alias(getUserInput())    // COMPILE ERROR - not a literal
  * }}}
  *
  * ==For Runtime Identifiers==
  * If you need to use a runtime-determined identifier (e.g., from user input or configuration), use the dialect's
  * `escapeIdentifier` method or `Placeholder.identifier`:
  * {{{
  *   import saferis.Placeholder.identifier
  *   val userInput = request.getParameter("column")
  *   sql"SELECT * FROM users ORDER BY \${identifier(userInput)}"  // Safely escaped
  * }}}
  */
final case class Alias private[saferis] (value: String):
  /** Convert the alias to its SQL representation */
  def toSql: SqlText = SqlText(value)

object Alias:
  /** Create an alias from a string literal.
    *
    * This is a macro that enforces the argument is a compile-time string literal. This prevents SQL injection by
    * ensuring aliases can only come from source code, never from runtime input.
    */
  inline def apply(inline value: String): Alias =
    ${ applyImpl('value) }

  private def applyImpl(value: Expr[String])(using Quotes): Expr[Alias] =
    import quotes.reflect.*

    value.asTerm match
      case Inlined(_, _, Literal(StringConstant(s))) =>
        '{ Alias.unsafe(${ Expr(s) }) }
      case Literal(StringConstant(s)) =>
        '{ Alias.unsafe(${ Expr(s) }) }
      case _ =>
        report.errorAndAbort(
          "Alias requires a string literal. " +
            "For runtime identifiers, use Placeholder.identifier() or dialect.escapeIdentifier() instead."
        )
    end match
  end applyImpl

  /** Internal constructor that bypasses the literal-only check. Used for values the library knows are safe: generated
    * aliases (e.g. `users_ref_1`), the compile-time-verified string literal spliced by the `apply` macro, and table
    * names. `private[saferis]` so user code cannot supply a runtime string (which would bypass the injection guard in
    * `apply`); Scala 3 macro hygiene lets the `apply` splice resolve this even though it is package-private. Use
    * `Alias("literal")` in user code.
    */
  private[saferis] def unsafe(value: String): Alias = new Alias(value)
end Alias
