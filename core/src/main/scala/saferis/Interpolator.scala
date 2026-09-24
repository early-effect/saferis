package saferis

import scala.annotation.Annotation
import scala.collection.mutable as m
import scala.quoted.*
import scala.reflect.Selectable.reflectiveSelectable

class silent extends Annotation

export Interpolator.sql
export Interpolator.sqlEcho
export Interpolator.array

object Interpolator:

  /** One array value. Write the operator in the SQL string.
    *
    * {{{
    *   sql"select * from $table where ${table.id} = any(${array(ids)})"
    *   sql"select * from $table where ${table.id} <> all(${array(names)})"
    * }}}
    *
    * Accepts any `Iterable[A]`. Duplicates are removed. An empty collection is one empty array parameter.
    */
  def array[A](values: Iterable[A])(using Encoder[A]): Placeholder =
    Placeholder.array(values)

  /** Varargs form. At least one element by construction. */
  def array[A](first: A, rest: A*)(using Encoder[A]): Placeholder =
    Placeholder.array(first +: rest)

  extension (inline sc: StringContext)
    /** Interpolates a string context creating an [[SqlFragment]].
      *
      * @param args
      * @return
      */
    inline def sql(inline args: Any*): SqlFragment =
      ${ sqlImpl('sc, 'args, '{ false }) }

    /** Interpolates a string context creating an [[SqlFragment]] and reports the statement SQL as compilation info.
      *
      * @param args
      * @return
      */
    inline def sqlEcho(inline args: Any*): SqlFragment =
      ${ sqlImpl('sc, 'args, '{ true }) }
  end extension

  private def sqlImpl(sc: Expr[StringContext], values: Expr[Seq[Any]], verbose: Expr[Boolean])(using
      Quotes
  ): Expr[SqlFragment] =
    import quotes.reflect.*
    val isSilent = !verbose.valueOrAbort

    val allArgsExprs: Seq[Expr[Any]] = values match
      case Varargs(ae) => ae

    if !isSilent then
      val parts: Seq[String] = sc match
        case '{ StringContext(${ Varargs(partsExpr) }*) } =>
          partsExpr.map { case '{ $part: String } =>
            part.asTerm match
              case Literal(StringConstant(str)) => str
              case _                            =>
                report.errorAndAbort("StringContext parts must be string literals")
          }
        case _ =>
          report.errorAndAbort("Expected a literal StringContext")

      val argumentDisplayStrings = allArgsExprs.map(argDisplayString)
      val sqlStatement           =
        if parts.size == 1 && argumentDisplayStrings.isEmpty then parts.head
        else
          val sb = new StringBuilder(parts.head)
          for (part, repr) <- parts.tail.zip(argumentDisplayStrings) do sb.append(repr).append(part)
          sb.toString.stripMargin

      report.info(s"Generated SQL: $sqlStatement", Position.ofMacroExpansion)
    end if

    val holder     = '{ (Vector.newBuilder[Placeholder]) }
    val placeExprs = getPlaceHoldersExpr(allArgsExprs, holder)
    '{ SqlFragment.interpolate($sc.parts, $placeExprs) }

  end sqlImpl

  type HoldersBuilder = m.Builder[Placeholder, Vector[Placeholder]]

  // Structural type for table instances - avoids direct Instance reference
  private type TableInstance = { def tableName: String; def alias: Option[Alias] }

  private def getPlaceHoldersExpr(all: Seq[Expr[Any]], builder: Expr[HoldersBuilder])(using
      Quotes
  ): Expr[Vector[Placeholder]] =
    import quotes.reflect.*

    // Helper to check if type is an Instance (using type symbol comparison for robustness)
    val instanceTypeSymbol                     = TypeRepr.of[Instance[?]].typeSymbol
    def isInstanceType(tpe: TypeRepr): Boolean =
      tpe.baseClasses.contains(instanceTypeSymbol)

    all match
      case '{ $arg: Placeholder } +: rest =>
        val acc = '{ $builder.addOne($arg) }
        getPlaceHoldersExpr(rest, acc)
      case expr +: rest =>
        val argTpe = expr.asTerm.tpe
        if isInstanceType(argTpe) then
          // Instance: compute SQL from internal fields
          // Use structural type to avoid cyclic macro dependency
          val ph = '{
            val inst = ${ expr }.asInstanceOf[TableInstance]
            Placeholder.raw(inst.alias.fold(inst.tableName)(a => s"${inst.tableName} as ${a.value}"))
          }
          val acc = '{ $builder.addOne($ph) }
          getPlaceHoldersExpr(rest, acc)
        else
          // Other types: summon Encoder
          expr match
            case '{ $arg: tp } =>
              val ph  = summonPlaceholder[tp](arg)
              val acc = '{ $builder.addOne($ph) }
              getPlaceHoldersExpr(rest, acc)
        end if
      case _ =>
        '{ $builder.result() }
    end match
  end getPlaceHoldersExpr

  private def summonPlaceholder[T: Type](arg: Expr[T])(using Quotes): Expr[Placeholder] =
    '{
      val e = ${ Macros.summonEncoder[T] }
      Placeholder(${ arg })(using e)
    }

  private def argDisplayString(expr: Expr[Any])(using Quotes): String =
    import quotes.reflect.*
    expr.asTerm match
      case Literal(const) =>
        const.value match
          case str: String => s"'$str'"
          case other       => other.toString
      case Select(qualifier, memberName) if qualifier.tpe <:< TypeRepr.of[saferis.Instance[?]] =>
        memberName
      case t if t.tpe <:< TypeRepr.of[saferis.Instance[?]] =>
        t.symbol.name
      case t if t.tpe <:< TypeRepr.of[saferis.SqlFragment] =>
        s"{${t.symbol.name}}"
      case _ =>
        "?"
    end match
  end argDisplayString

end Interpolator
