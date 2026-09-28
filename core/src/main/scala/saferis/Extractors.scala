package saferis

/** Get the SQL representation of a table instance.
  *
  * Usage:
  * {{{
  *   val users = Table[User]
  *   toSql(users)  // "users" or "users as u" if aliased
  * }}}
  */
def toSql[A](instance: Instance[A])(using Dialect): SqlText =
  SqlFragment.tableRef(instance.tableName, instance.alias).sql

/** Create an aliased version of a table instance (function style).
  *
  * Usage:
  * {{{
  *   val u = aliased(Table[User], "u")
  *   sql"SELECT \${u.name} FROM \$u"
  * }}}
  *
  * Note: For preserving refined types, use the `as` extension method instead:
  * {{{
  *   val u = Table[User] as "u"
  * }}}
  */
inline def aliased[A: Table](instance: Instance[A], inline alias: String): Instance[A] =
  val userAlias  = Alias(alias)
  val newColumns = instance.columns.map(_.withTableAlias(Some(userAlias)))
  instance.copy(alias = Some(userAlias), columns = newColumns)

/** Remove alias from a table instance. */
def unaliased[A: Table](instance: Instance[A]): Instance[A] =
  val newColumns = instance.columns.map(_.withTableAlias(None))
  instance.copy(alias = None, columns = newColumns)

/** Get foreign key constraint SQL for a table. */
def foreignKeyConstraints[A](instance: Instance[A])(using Dialect): Seq[String] =
  instance.foreignKeyConstraints

/** Get unique constraint SQL for a table. */
def uniqueConstraints[A](instance: Instance[A])(using Dialect): Seq[String] =
  instance.uniqueConstraintsSql

/** Extension methods for Instance - fluent API */
extension [A](instance: Instance[A])
  /** Create an aliased version of this table instance (fluent style).
    *
    * Usage:
    * {{{
    *   val u = Table[User] as "u"
    *   sql"SELECT \${u.name} FROM \$u"
    * }}}
    */
  transparent inline infix def as(inline alias: String) =
    instance.withAlias(alias)
end extension
