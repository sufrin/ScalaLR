package org.sufrin.scalalr
package stage2

/**
 * Support for the normalization of productions that contain repetitions, and
 * productions whose right-hand-sides lack reduction expressions.
 */

object Normalization  {
  import org.sufrin.scalalr.stage2.AST._
  import org.sufrin.scalalr.stage2.Messages

  var synthNumber:     Int = 0
  var syntheticRules:  List[DelayedRule] = Nil

  case class DelayedRule(theName: Name, fields: List[NamedField], repeatType: Repeat, START: SourceLocation, END: SourceLocation)

  def OptionType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("Option", List(symbolType), START)

  def ListType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("List", List(symbolType), START)

  /**
   * Generate (and yield) a synthetic rule for a production that recognises the specified repeat
   * of the given `fields`.
   */
  def syntheticRuleName(fields: List[NamedField], repeatType: Repeat, START: SourceLocation, END: SourceLocation): Name = {
    synthNumber += 1
    val theName = Name(s"S_$synthNumber", false, START)
    val delayedFields = fields take 2
    if (delayedFields != fields) Messages.warning(s"Shortening (${fields.mkString(" ")})$repeatType at $START--$END to (${delayedFields.mkString(" ")})$repeatType ")

    syntheticRules ::= DelayedRule(theName, delayedFields, repeatType, START, END)
    theName
  }


  def forceRule(symbolTable: SymbolTables)(delayedRule: DelayedRule): List[Rule] = {
    import delayedRule._

    def RULE(theName: Name, theType: SymbolType)(rhs: Production*): Rule = {
      Rule(TypedNonterminal(theName, theType, START), rhs, START)
    }

    def PROD(symbols: NamedField*)(reduction: String): Production =
       Production(symbols, Some(CodeExpression(reduction)), None, START)

    def PRODUCTION(symbols: Iterable[NamedField], after: Name*)(reduction: String): Production =
       Production(symbols.toList ++ after.map(name => NamedField(None, name, START)), Some(CodeExpression(reduction)), None, START)

    def LIST(theType: SymbolType) = ListType(theType, START)

    def EMPTY: Production = PROD()("Nil")

    implicit class FieldNaming(theName: Name) {
      def asField = NamedField(None, theName, START)
    }

    implicit class FieldTyping(field: NamedField) {
      def hasNoType: Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType
      def hasType:   Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)!=NoType
    }


    val searchOrdered = fields.iterator.filterNot(_.isAnonymous) ++ fields.iterator.filterNot(_.hasNoType) ++ fields.iterator
    repeatType match {
      case MaybeOne =>
        val field = searchOrdered.next()
        val theType = symbolTable.symbolType(field.theField)
        val theFieldName = field.theFieldName match {
          case None => field.theField
          case Some(other) => other
        }
        List(
          RULE(theName,  OptionType(theType, START))(
            PROD()("None"),
            PRODUCTION(fields)(if (theType.isNoType) "Some(())" else s"Some($$$theFieldName)")
          ))

      case OneOrMore | NoneOrMore | RightNoneOrMore | RightOneOrMore | Ellipsis =>
        val field = searchOrdered.next() // the first named or typed field
        val theType = symbolTable.symbolType.getOrElse(field.theField, NoType)
        val theFieldName = field.theFieldName match {
          case None        => field.theField
          case Some(other) => other
        }

        val theField = fields.filterNot(_.hasNoType) // this is the info-carrying field
        val thePunct = fields.filter(_.hasNoType) // this is the punctuation

        repeatType match {
          case RightNoneOrMore =>
            val infoThenPunct = fields match {
              case List(l, r) if r.hasNoType && l.hasType  =>
                fields
              case List(l, r) if l.hasNoType && r.hasType  =>
                Messages.warning(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded  right recursion")
                List(r, l)
              case List(_) =>
                Messages.warning(s"(${fields.mkString(" ")})$repeatType  at $START--$END a right recursion is space-inefficient (use *)")
                fields
              case _ =>
                Messages.warning(s"(${fields.mkString(" ")})$repeatType  at $START--$END  has no punctuation-guarded right recursion")
                fields
            }
            List(
              RULE(theName, LIST(theType))(
                  EMPTY,
                  PRODUCTION(theField)(s"List($$$theFieldName)"),
                  PRODUCTION(infoThenPunct, theName)(s"$$$theFieldName :: $$$theName"),
            ))

          case RightOneOrMore =>
            val infoThenPunct = fields match {
              case List(l, r) if r.hasNoType && l.hasType  =>
                fields
              case List(l, r) if l.hasNoType && r.hasType  =>
                Messages.inform(s"Reordering for punctuation-guarded right recursion ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType")
                List(r, l)
              case List(_) =>
                Messages.warning(s"Right recursive (${fields.mkString(" ")})$repeatType  at $START--$END is s space-inefficient (use +)")
                fields
              case _ =>
                Messages.inform(s"Reordering for  punctuation-guarded right recursion ${fields.mkString(" ")}$repeatType  at $START--$END  IS INFEASIBLE")
                fields
            }
            val theListName = Name(theName.forScala++"_R", false, START)
            List(
              RULE(theListName, LIST(theType)) ( // list = %empty | field {$field) | field ';' list
                  EMPTY,
                  PRODUCTION(theField)(s"List($$$theFieldName)"),
                  PRODUCTION(infoThenPunct, theListName)(s"$$$theFieldName :: $$$theListName")
              ),
              RULE(theName, LIST(theType)) (    // name = list { $list } | field
                PROD(theListName.asField)(s"$$$theListName"),
                PRODUCTION(fields.filterNot(_.hasNoType))(s"List($$$theFieldName)")
              )
            )

          case Ellipsis =>
            val ordered = fields match {
              case List(l, r) if  l.hasNoType && r.hasType =>
                List(l, r)
              case List(l, r) if  l.hasType && r.hasNoType  =>
                Messages.inform(s"Reordering for punctuation-guarded left recursion ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType ")
                List(r, l)
              case List(_) =>
                Messages.warning(s"(${fields.mkString(" ")})$repeatType at $START--$END treated as (${fields.mkString(" ")})+")
                fields
              case _ => fields
            }
            ordered match {
              case Nil =>
                List()
              case List(info) =>
                val theListName = Name(theName.forScala ++ "_E", false, START)
                List(
                  RULE(theListName, LIST(theType))(
                    PROD(theListName.asField, info)(s"$$$theFieldName :: $$$theListName"),
                    PRODUCTION(theField)(s"List($$$theFieldName)"),
                  ),
                  RULE(theName, LIST(theType))(PROD(theListName.asField)(s"$$$theListName.reverse"))
                )
              case List(punct: NamedField, info: NamedField) =>
                val theListName = Name(theName.forScala ++ "_E", false, START)
                List(
                  RULE(theListName, LIST(theType))(
                    PROD(theListName.asField, punct, info)(s"$$$theFieldName :: $$$theListName"),
                    PRODUCTION(theField)(s"List($$$theFieldName)"),
                    PROD(theListName.asField, punct)(s"$$$theListName")
                  ),
                  RULE(theName, LIST(theType))(PROD(theListName.asField)(s"$$$theListName.reverse"))
                )
              case _: List[NamedField] => Nil
            }

          case _ =>
            // force any "punctuation" symbol to the front of the body of the iteration
            val reorderedFields = fields match {
              case List(l, r) if l.hasNoType && r.hasType  =>
                fields
              case List(l, r) if l.hasType && r.hasNoType =>
                Messages.inform(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded left recursion")
                List(r, l)
              case List(l, r) if l.hasType && r.hasType =>
                Messages.inform(s"Reordering ($l $r)$repeatType  at $START--$END INFEASIBLE for left recursion: both have nontrivial types")
                fields
              case List(l, r) if l.hasNoType && r.hasNoType =>
                Messages.inform(s"Reordering ($l $r)$repeatType  at $START--$END INFEASIBLE for left recursion: both have trivial (punctuation) types")
                fields
              case _ =>
                Messages.inform(s"Reordering (${fields.mkString(" ")})$repeatType  at $START--$END  INFEASIBLE for punctuation-guarded left recursion")
                fields
            }
            val theListName = Name(theName.forScala++"_L", false, START)
            List(
              RULE(theListName, LIST(theType))(
                PRODUCTION(theField)(s"List($$$theFieldName)"),
                PRODUCTION(theListName.asField :: reorderedFields)(s"$$$theFieldName :: $$$theListName")
              ),
              if (repeatType == NoneOrMore)
                RULE(theName, LIST(theType))(EMPTY, PROD(theListName.asField)(s"$$$theListName.reverse"))
              else
                RULE(theName, LIST(theType))(PROD(theListName.asField)(s"$$$theListName.reverse"))
            )
        }
    }
  }

  // TODO: this should be a Notation feature
  var autoResults: Boolean = Generator.logGeneration.contains("auto")

  /**
   *
   * Invent/infer a reduction for a production that:
   *
   * (1) lacks one, or
   * (2) has a reduction that is a constructor or constant value
   *
   * 1. is only effective if the production has exactly one value-carrying symbol
   * 2. uses the following heuristics based on the form of the production
   *
   * {{{
   *    lhs: Type = ... l1: S1 ... ln: Sn => C
   * }}}
   * where the `C` can be interpreted as a result-constructing expression (Constructor)  so
   * that the `C` is invoked with named arguments that match its parameter names.
   * {{{
   *    lhs: Type = ... l1: S1 ... ln: Sn => C(l1=\$l1, ... ln=\$ln)
   * }}}
   * and, if `TT` is a solo terminal value-carrying symbol (and `C` is a one-argument constructor)
   * {{{
   *   lhs: Type = ... TT ... => C
   * }}}
   * into
   * {{{
   *   lhs: Type = ... TT ... => C(\$TT)
   * }}}
   *
   * If there are no value-carrying terminal symbols, then the result will be the constant `C`
   *
   */
  def inferReduction(symbolTable: SymbolTables)(rule: Rule): Rule = {
    implicit class FieldTyping(field: NamedField) {
      def hasNoType: Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType
      def hasType:   Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)!=NoType
      def isPunctuation: Boolean =
        symbolTable.symbolType.get(field.theField) match {
           case None     => false
           case Some(ty) => ty.isNoType
      }
    }

    // TODO: needs a little refactoring!
    def toExpression(field: NamedField): Expression =
      symbolTable.symbolType.get(field.theField) match {
        case None =>
          Messages.warning(s"Named symbol $field has no type")
          CodeExpression(" () ")
        case Some(theType) =>
        val scalaType = theType.scalaTypeName
        field.theFieldName match {
          case Some(name) =>
            if (theType == NoType) {
              Messages.warning(s"Named symbol ${name}: ${Type} carries no value")
              CodeExpression(s"${mangle(name)}")
            }
            else
              CodeExpression(s"${mangle(name)}")

          case None =>
            if (theType == NoType) CodeExpression("None") else CodeExpression(s"${mangle(field.theField)}")
        }
    }

    def inferredProduction(production: Production): Production = {
      if (production.reduction.isDefined) production else
      production.symbols.length match {
        case 0 =>
          Messages.inform(s"""\n Using universal default reduction expression value \"()\" for the production at: ${production.location}
                  | this is because the production is empty.
                  | Recommended remedy: specify the reduction expression explicitly.
                  | """.stripMargin)
          production.copy(reduction = Some(CodeExpression(" ()) ")))
        case 1 =>
          val field = production.symbols.head
          val result: Name  =
            field.theFieldName match {
              case Some(name) => name
              case None       => field.theField
            }
          production.copy(reduction = Some(CodeExpression(s"$$$result")))
        case n =>
          val searchOrdered = production.symbols.filterNot(_.hasNoType)
          searchOrdered.length match {
            case 0 =>
              Messages.inform(s"""\n Using universal default reduction expression value \"()\" for the production at: ${production.location}
                      | this is because the production has no value-carrying symbols.
                      | Recommended remedy: specify the reduction expression explicitly.""".stripMargin)
              production.copy(reduction = Some(CodeExpression(" () ")))
            case 1 =>
              val field = searchOrdered.head
              val result: Name  =
                field.theFieldName match {
                  case Some(name) => name
                  case None       => field.theField
                }
              production.copy(reduction = Some(CodeExpression(s"$$$result")))
            case n =>
              Messages.inform(
                s"""\n Using universal default reduction expression value \"()\" for the production $production at: ${production.location}
                   | This is because the production's intended value cannot be determined (there is more than one value-carrying symbol).
                   | Recommended remedy: specify the reduction expression explicitly.
                   |""".stripMargin)
              production.copy(reduction =  Some(CodeExpression(" () ")))
          }
      }
    }

    def fieldNames(fields: Seq[NamedField]): Seq[Name] = {
      (for { field <- fields } yield field.theFieldName.getOrElse(field.theField)).distinct
    }

    implicit class ScalaScope(scala: Scala) {

    }

    /**
     * Yields the production after checking its result expression for sanity.
     *
     * 1. At present the sanity check warns of variables free in the result expression
     * that are not in scope (declared as labels) in the production. It also warns of those
     * in scope that are not decorated with dollar signs; and eventually vetoes code-generation
     * if any appear.
     *
     * 2. Eventually it will (if enabled) transform the result expressions of rules of the form
     * {{{
     *    lhs: Type = ... l1: S1 ... ln: Sn => C
     * }}}
     * where the `C` can be interpreted as a result-constructing expression (Constructor)  so
     * that the `C` is invoked with named arguments that match its parameter names.
     * {{{
     *    lhs: Type = ... l1: S1 ... ln: Sn => C(l1=$l1, ... ln=$ln)
     * }}}
     * and, if `TT` is a solo terminal value-carrying symbol (and `C` is a one-argument constructor)
     * {{{
     *   lhs: Type = ... TT ... => C
     * }}}
     * into
     * {{{
     *   lhs: Type = ... TT ... => C($TT)
     * }}}
     * If there are no value carrying terminal symbols, then the result will be the constant `C`
     *
     * 3. The above rules naturally give rise to the possibility that the collection of rules can /define/
     * a collection of types.
     *
     * @param lhs
     * @param production
     * @return production
     */
    def resultCheckedProduction(lhs: TypedNonterminal, production: Production): Production =
      production.reduction match {
        case None => production
        case Some(CodeExpression(_)) => production
        case Some(ScalaExpression(scala, start: SourceLocation)) =>
          if (symbolTable.inferResults && scala.isConst ) {
            val inScope = fieldNames(production.symbols.filterNot(_.isPunctuation)) // ignore punctuation
            val START = scala.START
             val autoReduction =
             inScope.length match {
               case 0 => production.reduction
               case 1 => Some(ScalaExpression(Apply(scala, List(Dollar(Id(inScope(0), START)))), START)) // IDENTICAL ANON TERMINALS PICKED UP LATER
               case _ => Some(ScalaExpression(ApplyNamed(scala, inScope), START))
             }
             production.copy(reduction = autoReduction)
          } else {
            val inScope = fieldNames(production.symbols)
            val used = scala.free
            val hasDollar = scala.decorated
            val unscoped = for {variable <- used if !inScope.contains(variable)} yield variable
            val noDollar = for {variable <- used if inScope.contains(variable) && !hasDollar.contains(variable)} yield variable
            if (unscoped.nonEmpty) Messages.warning(s"${start} undeclared: (${unscoped.mkString(" ")})  $lhs = $production  ")
            if (noDollar.nonEmpty) Messages.fatal(s"${start} un$$ollared: (${noDollar.mkString(" ")}) $lhs = $production  ")
            production.copy(reduction = production.reduction)
          }
      }


    val newRHS = for { production <- rule.rhs } yield inferredProduction(resultCheckedProduction(rule.lhs, production))
    rule.copy(rhs=newRHS)

  }

  /**
   * Normalize the given `Notation` by expanding derived repetition constructs
   * that appear there.
   *
   * @param notation
   * @return
   */
  def normalize(notation: Notation): Notation = {
    val symbolTables    = new SymbolTables(notation)
    val expandedRules   = notation.theRules ++ syntheticRules.reverse.flatMap(forceRule(symbolTables))
    val inferencedRules = expandedRules.map(inferReduction(symbolTables))
    if (Generator.logGeneration.contains("syn")) {
      println("\nProductions after code-synthesis and normalization")
      var i: Int = 0
      val width = (for { Rule(lhs, rhs, _) <- inferencedRules} yield lhs.toString.size).max

      for {Rule(lhs, rhs, _) <- inferencedRules; prod <- rhs} {
        i += 1
        println(f"$i%04d   ${lhs.toString} ${" " * (width - lhs.toString.size)} = $prod")
      }
    }
    notation.copy(theRules = inferencedRules)
  }

}
