package saferis

import scala.annotation.StaticAnnotation
import scala.quoted.*

object Macros:

  private[saferis] inline def nameOf[A]: String = ${ nameOfImpl[A] }

  private def nameOfImpl[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val tpe                 = TypeRepr.of[A]
    val tableNameTypeSymbol = TypeRepr.of[tableName].typeSymbol
    tpe.typeSymbol.annotations
      .collectFirst {
        case Apply(Select(New(tpt), _), List(Literal(StringConstant(name))))
            if tpt.tpe.typeSymbol == tableNameTypeSymbol =>
          Expr(name)
      }
      .getOrElse(Expr(tpe.typeSymbol.name))
  end nameOfImpl

  private[saferis] inline def columnsOf[A]: Seq[Column[?]] = ${ columnsOfImpl[A] }

  // Scala field names that cannot be used because they would shadow Selectable trait methods
  private val reservedFieldNames = Set("selectDynamic", "applyDynamic")

  private def columnsOfImpl[A: Type](using Quotes): Expr[Seq[Column[?]]] =
    import quotes.reflect.*
    val tpe    = TypeRepr.of[A]
    val fields = tpe.typeSymbol.caseFields

    // Build a mapping from type parameter names to their actual type arguments
    // This enables resolving path-dependent types like GenericRow.this.E to the actual E from the calling context
    val typeParamSubstitution: Map[String, TypeRepr] = tpe match
      case AppliedType(_, typeArgs) =>
        val typeParams = tpe.typeSymbol.typeMembers.filter(_.isTypeParam)
        typeParams.map(_.name).zip(typeArgs).toMap
      case _ => Map.empty

    // Validate no reserved Scala field names are used
    fields.foreach { field =>
      if reservedFieldNames.contains(field.name) then
        report.errorAndAbort(
          s"Scala field name '${field.name}' is reserved and cannot be used in a Table. " +
            s"These names conflict with Scala's Selectable trait methods. " +
            s"If your database column is named '${field.name}', use a different Scala field name with @label: " +
            s"""@label("${field.name}") myField: String"""
        )
    }

    // Helper to resolve path-dependent types using the substitution map
    def resolveInnerType(innerType: TypeRepr): TypeRepr =
      innerType match
        case tr if tr.typeSymbol.isTypeParam =>
          typeParamSubstitution.getOrElse(tr.typeSymbol.name, tr)
        case tr =>
          // Try to extract type param name from the string representation for path-dependent types
          val typeStr   = tr.show
          val paramName = typeStr.split("\\.").lastOption.getOrElse("")
          typeParamSubstitution.getOrElse(paramName, tr)

    // Get the Json type symbol for comparison (more robust than string matching)
    val jsonTypeSymbol = TypeRepr.of[Json[Any]].typeSymbol

    // Decoder summon with Json[X] special handling using type parameter substitution
    def summonDecoderForField[T: Type]: Expr[Decoder[T]] =
      Expr
        .summon[Decoder[T]]
        .orElse {
          val tpeRepr = TypeRepr.of[T]
          // Check if it's Json[X] by comparing type symbols directly
          if tpeRepr.typeSymbol == jsonTypeSymbol && tpeRepr.typeArgs.nonEmpty then
            val innerType     = tpeRepr.typeArgs.head
            val resolvedInner = resolveInnerType(innerType)
            val codecType     = TypeRepr.of[zio.json.JsonCodec].appliedTo(resolvedInner)
            Implicits.search(codecType) match
              case iss: ImplicitSearchSuccess =>
                resolvedInner.asType match
                  case '[inner] =>
                    Some('{
                      Json
                        .decoder[inner](using ${ iss.tree.asExprOf[zio.json.JsonCodec[inner]] })
                        .asInstanceOf[Decoder[T]]
                    })
              case _: ImplicitSearchFailure => None
            end match
          else None
          end if
        }
        .orElse(
          TypeRepr.of[T].widen.asType match
            case '[tpe] => Expr.summon[Decoder[tpe]].map(d => '{ $d.asInstanceOf[Decoder[T]] })
        )
        .orElse(
          TypeRepr.of[T].widen.asType match
            case '[tpe] => Expr.summon[Codec[tpe]].map(codec => '{ $codec.asInstanceOf[Decoder[T]] })
        )
        .orElse(
          Expr.summon[zio.json.JsonCodec[T]].map(codec => '{ Decoder.fromJsonCodec[T](using $codec) })
        )
        .getOrElse(report.errorAndAbort(s"Could not find a Decoder for ${Type.show[T]}"))

    // Encoder summon with Json[X] special handling using type parameter substitution
    def summonEncoderForField[T: Type]: Expr[Encoder[T]] =
      Expr
        .summon[Encoder[T]]
        .orElse {
          val tpeRepr = TypeRepr.of[T]
          // Check if it's Json[X] by comparing type symbols directly
          if tpeRepr.typeSymbol == jsonTypeSymbol && tpeRepr.typeArgs.nonEmpty then
            val innerType     = tpeRepr.typeArgs.head
            val resolvedInner = resolveInnerType(innerType)
            val codecType     = TypeRepr.of[zio.json.JsonCodec].appliedTo(resolvedInner)
            Implicits.search(codecType) match
              case iss: ImplicitSearchSuccess =>
                resolvedInner.asType match
                  case '[inner] =>
                    Some('{
                      Json
                        .encoder[inner](using ${ iss.tree.asExprOf[zio.json.JsonCodec[inner]] })
                        .asInstanceOf[Encoder[T]]
                    })
              case _: ImplicitSearchFailure => None
            end match
          else None
          end if
        }
        .orElse(
          TypeRepr.of[T].widen.asType match
            case '[tpe] => Expr.summon[Encoder[tpe]].map(e => '{ $e.asInstanceOf[Encoder[T]] })
        )
        .orElse(
          TypeRepr.of[T].widen.asType match
            case '[tpe] => Expr.summon[Codec[tpe]].map(codec => '{ $codec.asInstanceOf[Encoder[T]] })
        )
        .orElse(
          Expr.summon[zio.json.JsonCodec[T]].map(codec => '{ Encoder.fromJsonCodec[T](using $codec) })
        )
        .getOrElse(report.errorAndAbort(s"Could not find Encoder for ${Type.show[T]}"))

    val columns = fields.map { field =>
      val fieldName = field.name

      field.tree match
        case valDef: ValDef =>
          valDef.tpt.tpe.asType match
            case '[a] =>
              val reader     = summonDecoderForField[a]
              val writer     = summonEncoderForField[a]
              val isNullable = isOptionType[a]
              val defaultVal = getDefaultValue[A, a](fieldName)

              '{
                val label       = ${ getLabel[A](fieldName) }
                val isKey       = ${ elemHasAnnotation[A, saferis.key](fieldName) }
                val isGenerated = ${ elemHasAnnotation[A, saferis.generated](fieldName) }

                Column[a](
                  ${ Expr(fieldName) },
                  label,
                  isKey,
                  isGenerated,
                  $isNullable,
                  $defaultVal,
                  None,
                )(using
                  $reader,
                  $writer,
                )
              }
      end match
    }
    Expr.ofSeq(columns)
  end columnsOfImpl

  private[saferis] transparent inline def instanceOf[A](alias: Option[String]) =
    ${ instanceImpl[A]('alias) }

  private def instanceImpl[A: Type](alias: Expr[Option[String]])(using Quotes) =
    import quotes.reflect.*
    val columns         = columnsOfImpl[A]
    val name            = nameOfImpl[A]
    val tpe             = TypeRepr.of[A]
    val caseClassFields = tpe.typeSymbol.caseFields
    // Collect both field names and their types for type-safe refinements
    val caseClassFieldNamesAndTypes = caseClassFields.map(f => (f.name, tpe.memberType(f)))
    val refined                     = refinementForLabels(TypeRepr.of[Instance[A]], caseClassFieldNamesAndTypes)
    val keys                        = elemsWithAnnotation[A, key]
    val x                           = MethodType(keys.map(_._1))(
      _ => keys.map(_._2),
      _ => TypeRepr.of[Instance[A]#TypedFragment],
    )

    val ref3 = Refinement(refined, Instance.getByKey, x)
    val res  = ref3.asType match
      case '[t] =>
        '{
          val x = ${ summonTable[A] }
          // Convert Option[String] to Option[Alias]
          val aliasOpt: Option[Alias] = $alias.map(Alias.unsafe(_))
          new Instance[A](
            $name,
            $columns,
            aliasOpt,
            Vector.empty,
            Vector.empty,
            Vector.empty,
          )(using x).asInstanceOf[t]
        }
    res
  end instanceImpl

  private def elemHasAnnotation[T: Type, A <: StaticAnnotation: Type](elemName: String)(using
      Quotes
  ): Expr[Boolean] =
    import quotes.reflect.*
    val annotType = TypeRepr.of[A]
    // Check annotations from the class definition tree directly
    // Use <:< subtype check because @generated extends @key
    val hasAnnot = getConstructorParamAnnotations[T](elemName).exists { annot =>
      annot.tpe <:< annotType
    }
    Expr(hasAnnot)
  end elemHasAnnotation

  private def isOptionType[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    Expr(tpe.typeSymbol == TypeRepr.of[Option[?]].typeSymbol)

  // Get annotations for a field by looking at both field symbols and constructor parameters
  // This works for both generic and concrete types
  private def getFieldAnnotations[T: Type](fieldName: String)(using Quotes): List[quotes.reflect.Term] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    // Use classSymbol for applied types to get the base class symbol
    val classSym = tpe.classSymbol.getOrElse(tpe.typeSymbol)

    // Try 1: Field symbol annotations (works for most cases)
    val fieldAnnots = classSym.caseFields.find(_.name == fieldName).map(_.annotations).getOrElse(Nil)
    if fieldAnnots.nonEmpty then return fieldAnnots

    // Try 2: Constructor parameter symbol annotations
    // Filter to only term parameter clauses (skip type parameter clauses for generics)
    val termParamss = classSym.primaryConstructor.paramSymss.filter(_.forall(!_.isTypeParam))
    val paramAnnots = termParamss.headOption
      .flatMap(_.find(_.name == fieldName))
      .map(_.annotations)
      .getOrElse(Nil)
    if paramAnnots.nonEmpty then return paramAnnots

    // Try 3: Look at the class definition tree for ValDef body members
    classSym.tree match
      case cd: ClassDef =>
        cd.body
          .collectFirst {
            case vd: ValDef if vd.name == fieldName => vd.symbol.annotations
          }
          .getOrElse(Nil)
      case _ => Nil
  end getFieldAnnotations

  private def elemsWithAnnotation[T: Type, A <: StaticAnnotation: Type](using
      Quotes
  ): List[(String, x$1.reflect.TypeRepr)] =
    import quotes.reflect.*
    val annotType = TypeRepr.of[A]
    val tpe       = TypeRepr.of[T]
    // Use classSymbol for applied types to get the base class symbol
    val classSym = tpe.classSymbol.getOrElse(tpe.typeSymbol)

    // Helper to check if annotations contain the target annotation type or a subtype
    // This is important because @generated extends @key
    def hasAnnotation(annots: List[Term]): Boolean =
      annots.exists(_.tpe <:< annotType)

    // Try 1: caseFields annotations (most reliable for derives Table case)
    val fromCaseFields = classSym.caseFields.filter(f => hasAnnotation(f.annotations))
    if fromCaseFields.nonEmpty then return fromCaseFields.map(f => (f.name, tpe.memberType(f)))

    // Try 2: primaryConstructor parameter annotations
    val termParamss = classSym.primaryConstructor.paramSymss.filter(_.forall(!_.isTypeParam))
    val fromParams  = termParamss.headOption
      .map(_.filter(p => hasAnnotation(p.annotations)))
      .getOrElse(Nil)
    if fromParams.nonEmpty then
      return fromParams.map(p => (p.name, tpe.memberType(classSym.caseFields.find(_.name == p.name).get)))

    // Try 3: class definition tree (needed for generic types)
    classSym.tree match
      case cd: ClassDef =>
        cd.body.collect {
          case vd: ValDef if hasAnnotation(vd.symbol.annotations) =>
            (vd.name, tpe.memberType(classSym.caseFields.find(_.name == vd.name).get))
        }
      case _ => Nil
  end elemsWithAnnotation

  // Get annotations for a constructor parameter by looking at the class definition tree
  // This works for both generic and concrete types because it accesses the AST directly
  private def getConstructorParamAnnotations[T: Type](paramName: String)(using Quotes): List[quotes.reflect.Term] =
    getFieldAnnotations[T](paramName)

  private def getLabel[T: Type](elemName: String)(using
      Quotes
  ): Expr[String] =
    import quotes.reflect.*
    val labelTypeSymbol = TypeRepr.of[label].typeSymbol
    // Get annotations directly from the class definition tree
    getConstructorParamAnnotations[T](elemName)
      .collectFirst {
        case Apply(Select(New(tpt), _), List(Literal(StringConstant(name)))) if tpt.tpe.typeSymbol == labelTypeSymbol =>
          Expr(name)
      }
      .getOrElse(Expr(elemName))
  end getLabel

  private def getDefaultValue[T: Type, A: Type](elemName: String)(using
      Quotes
  ): Expr[Option[A]] =
    import quotes.reflect.*
    val tpe        = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    val companion  = typeSymbol.companionModule
    val params     = typeSymbol.primaryConstructor.paramSymss.head
    val paramIndex = params.indexWhere(_.name == elemName)

    if paramIndex < 0 then Expr(None)
    else
      val param      = params(paramIndex)
      val hasDefault = param.flags.is(Flags.HasDefault)

      if hasDefault then
        // In Scala 3, the default method is named $lessinit$greater$default$N (1-indexed)
        // This corresponds to <init>$default$N but with angle brackets encoded
        val defaultMethodName = s"$$lessinit$$greater$$default$$${paramIndex + 1}"

        // Try to find the default method on the companion object using declarations
        if companion != Symbol.noSymbol then
          // First try methodMember
          val methodOpt = companion
            .methodMember(defaultMethodName)
            .headOption
            .orElse {
              // Fall back to searching declarations directly
              companion.declarations.find(_.name == defaultMethodName)
            }

          methodOpt match
            case Some(defaultMethod) =>
              try
                val defaultExpr = Select(Ref(companion), defaultMethod).asExprOf[A]
                '{ Some($defaultExpr) }
              catch
                case _: Exception =>
                  Expr(None)
            case None =>
              Expr(None)
          end match
        else Expr(None)
        end if
      else Expr(None)
      end if
    end if
  end getDefaultValue

  private def summonTable[T: Type](using Quotes): Expr[Table[T]] =
    import quotes.reflect.*
    Expr
      .summon[Table[T]]
      .getOrElse:
        report.errorAndAbort(s"Could not find a Table instance for ${Type.show[T]}")
  end summonTable

  private[saferis] inline def make[A](args: Seq[(String, Any)]): Either[DecodeError, A] = ${ makeImpl[A]('args) }

  private def makeImpl[A: Type](args: Expr[Seq[(String, Any)]])(using Quotes): Expr[Either[DecodeError, A]] =
    import quotes.reflect.*

    val tpe          = TypeRepr.of[A]
    val companion    = tpe.typeSymbol.companionModule
    val applyMethod  = companion.methodMember("apply").head
    val fields       = tpe.typeSymbol.caseFields
    val typeName     = Type.show[A]
    val expected     = Expr(fields.length)
    val typeNameExpr = Expr(typeName)

    val typeArgs     = tpe.typeArgs
    val baseSelect   = Select(Ref(companion), applyMethod)
    val withTypeArgs =
      if typeArgs.nonEmpty then TypeApply(baseSelect, typeArgs.map(Inferred(_)))
      else baseSelect

    val paramLists      = applyMethod.paramSymss
    val termParamLists  = paramLists.filter(_.forall(!_.isTypeParam))
    val usingParamLists =
      termParamLists.drop(1).filter(_.exists(p => p.flags.is(Flags.Given) || p.flags.is(Flags.Implicit)))

    val usingArgs: List[Term] =
      if usingParamLists.isEmpty then Nil
      else
        val typeParamNames = paramLists.headOption
          .filter(_.forall(_.isTypeParam))
          .map(_.map(_.name))
          .getOrElse(Nil)
        val typeSubstitution                            = typeParamNames.zip(typeArgs).toMap
        def substituteTypeParams(t: TypeRepr): TypeRepr =
          t match
            case tr if tr.typeSymbol.isTypeParam =>
              typeSubstitution.getOrElse(tr.typeSymbol.name, tr)
            case AppliedType(tycon, args) =>
              tycon.appliedTo(args.map(substituteTypeParams))
            case _ => t
        usingParamLists.flatten.map { param =>
          val rawType   = param.tree.asInstanceOf[ValDef].tpt.tpe
          val paramType = substituteTypeParams(rawType)
          Implicits.search(paramType) match
            case iss: ImplicitSearchSuccess => iss.tree
            case _: ImplicitSearchFailure   =>
              report.errorAndAbort(s"Could not find implicit for ${paramType.show} in makeImpl")
        }

    def read[t: Type](paramName: String): Expr[Either[DecodeError, t]] =
      val name = Expr(paramName)
      '{
        $args.toMap
          .get($name)
          .map(_.asInstanceOf[t])
          .toRight(
            DecodeError(
              s"Error constructing instance of ${$typeNameExpr}. Could not find value for parameter ${$name}"
            )
          )
      }
    end read

    def chain(rest: List[Symbol], acc: List[Term]): Expr[Either[DecodeError, A]] =
      rest match
        case Nil =>
          val applied =
            val base = Apply(withTypeArgs, acc)
            if usingArgs.nonEmpty then Apply(base, usingArgs) else base
          '{ Right(${ applied.asExprOf[A] }) }
        case param :: tail =>
          val paramType = tpe.memberType(param)
          paramType.asType match
            case '[t] =>
              val reading = read[t](param.name)
              '{
                $reading.flatMap { (v: t) =>
                  ${ chain(tail, acc :+ '{ v }.asTerm) }
                }
              }

    val built = chain(fields.toList, Nil)
    '{
      val values = $args
      if values.length != $expected then
        Left(
          DecodeError(
            s"wrong arity constructing ${$typeNameExpr}: expected ${$expected} values, got ${values.length}"
          )
        )
      else $built
    }
  end makeImpl

  // This method is used to refine the Instance type with the field names/labels
  // Instance is a structural type - we preserve the actual field types for type-safe column access
  // Important: We take baseType as Instance[A] to preserve the type parameter
  private def refinementForLabels(using
      q: Quotes
  )(
      baseType: q.reflect.TypeRepr,
      fieldNamesAndTypes: Seq[(String, q.reflect.TypeRepr)],
  ) =
    import q.reflect.*
    fieldNamesAndTypes.foldLeft(baseType): (t, nt) =>
      val (name, fieldType) = nt
      // Create Column[FieldType] refinement to preserve type information
      val columnType = TypeRepr.of[Column].appliedTo(fieldType)
      Refinement(t, name, columnType)
  end refinementForLabels

  // Simple encoder summon for columnPlaceholdersImpl (used with concrete types)
  private[saferis] def summonEncoder[T: Type](using Quotes): Expr[Encoder[T]] =
    import quotes.reflect.*
    Expr
      .summon[Encoder[T]]
      .orElse(
        TypeRepr.of[T].widen.asType match
          case '[tpe] => Expr.summon[Encoder[tpe]].map(e => '{ $e.asInstanceOf[Encoder[T]] })
      )
      .orElse(
        TypeRepr.of[T].widen.asType match
          case '[tpe] => Expr.summon[Codec[tpe]].map(codec => '{ $codec.asInstanceOf[Encoder[T]] })
      )
      .orElse(
        Expr.summon[zio.json.JsonCodec[T]].map(codec => '{ Encoder.fromJsonCodec[T](using $codec) })
      )
      .getOrElse(report.errorAndAbort(s"Could not find Encoder for ${Type.show[T]}"))
  end summonEncoder

  private[saferis] inline def columnPlaceholders[A](instance: A): List[(String, Placeholder)] = ${
    columnPlaceholdersImpl('instance)
  }

  private def columnPlaceholdersImpl[A: Type](instance: Expr[A])(using
      Quotes
  ): Expr[List[(String, Placeholder)]] =
    import quotes.reflect.*
    val tpe         = TypeRepr.of[A]
    val fields      = tpe.typeSymbol.caseFields
    val fieldValues = fields.map { field =>
      val fieldName  = field.name
      val fieldType  = tpe.memberType(field)
      val fieldValue =
        fieldType.asType match
          case '[ft] =>
            val f = Select(instance.asTerm, field).asExprOf[ft]
            val e = summonEncoder[ft]
            '{ Placeholder($f)(using $e) }

      '{ (${ Expr(fieldName) }, $fieldValue) }
    }

    Expr.ofList(fieldValues)
  end columnPlaceholdersImpl

  // === Field extraction macros for type-safe selectors ===

  /** Extract field name from a selector function like `_.fieldName`.
    *
    * This is used by Query for type-safe ON and WHERE conditions, and by ForeignKey for type-safe references.
    *
    * @param selector
    *   A lambda like `(a: A) => a.fieldName` or `_.fieldName`
    * @return
    *   The field name as a String
    */
  private[saferis] inline def extractFieldName[A, T](inline selector: A => T): String =
    ${ extractFieldNameImpl[A, T]('selector) }

  private[saferis] def extractFieldNameImpl[A: Type, T: Type](selector: Expr[A => T])(using Quotes): Expr[String] =
    val fieldName = extractFieldNameFromSelector(selector)
    Expr(fieldName)

  /** Extract column from instance using a selector, preserving type information.
    *
    * Unlike extractFieldName which returns a String, this returns the actual Column[T] with proper typing. The cast is
    * internal and safe - the macro verifies at compile time that the selector returns type T.
    *
    * Use via extension method: `instance.column(_.fieldName)` for better type inference.
    */
  private[saferis] inline def extractColumn[A, T](
      inline instance: Instance[A],
      inline selector: A => T,
  ): Column[T] = ${ extractColumnImpl[A, T]('instance, 'selector) }

  private def extractColumnImpl[A: Type, T: Type](
      instance: Expr[Instance[A]],
      selector: Expr[A => T],
  )(using Quotes): Expr[Column[T]] =
    val fieldName = extractFieldNameFromSelector(selector)
    '{
      $instance.selectDynamic(${ Expr(fieldName) }).asInstanceOf[Column[T]]
    }

  /** Internal helper to extract field name from selector - returns String directly for use in other macros */
  private[saferis] def extractFieldNameFromSelector[A: Type, T: Type](selector: Expr[A => T])(using Quotes): String =
    import quotes.reflect.*
    findFieldName(selector.asTerm)

  /** Recursively find the field name from a selector expression */
  private def findFieldName(using Quotes)(term: quotes.reflect.Term): String =
    import quotes.reflect.*
    term match
      // Handle: Block(List(DefDef(..., Some(Select(_, name)))), _)
      case Block(List(DefDef(_, _, _, Some(body))), _) =>
        extractFromBody(body)
      // Handle: Inlined(_, _, innerTerm)
      case Inlined(_, _, inner) =>
        findFieldName(inner)
      // Handle: Lambda(_, Select(_, name))
      case Lambda(_, body) =>
        extractFromBody(body)
      // Handle: Typed(inner, _) - wraps typed expressions
      case Typed(inner, _) =>
        findFieldName(inner)
      // Handle: Block containing just the lambda
      case Block(stats, expr) =>
        findFieldName(expr)
      case other =>
        report.errorAndAbort(
          s"Expected a simple field selector like _.fieldName, got: ${other.show} (class: ${other.getClass.getName})"
        )
    end match
  end findFieldName

  private def extractFromBody(using Quotes)(body: quotes.reflect.Term): String =
    import quotes.reflect.*
    body match
      case Select(_, fieldName) => fieldName
      case Inlined(_, _, inner) => extractFromBody(inner)
      case Typed(inner, _)      => extractFromBody(inner)
      case other                =>
        report.errorAndAbort(
          s"Expected a field access like _.fieldName, got: ${other.show} (class: ${other.getClass.getName})"
        )
  end extractFromBody

  /** Extract a field value extraction function from a selector.
    *
    * Given a selector like `_.id`, generates a function `A => T` that extracts the `id` field from an instance of A.
    * Used by pagedStream to extract cursor values from rows.
    *
    * @param selector
    *   A lambda like `_.fieldName`
    * @return
    *   A function that extracts the field value from an instance
    */
  private[saferis] inline def extractFieldValueFunc[A, T](inline selector: A => T): A => T =
    ${ extractFieldValueFuncImpl[A, T]('selector) }

  private def extractFieldValueFuncImpl[A: Type, T: Type](
      selector: Expr[A => T]
  )(using Quotes): Expr[A => T] =
    import quotes.reflect.*
    val fieldName = extractFieldNameFromSelector(selector)

    // Generate a function that reads the field via its case-field accessor
    // rather than Product.productElement, so Table types need not be
    // recognized as scala.Product.
    val field = TypeRepr
      .of[A]
      .typeSymbol
      .caseFields
      .find(_.name == fieldName)
      .getOrElse(report.errorAndAbort(s"Field '$fieldName' not found in ${Type.show[A]}"))

    '{ (a: A) => ${ Select('a.asTerm, field).asExprOf[T] } }
  end extractFieldValueFuncImpl

end Macros
