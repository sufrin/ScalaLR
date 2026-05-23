package org.sufrin.scalalr
package stage2

/**
 * Support for the normalization of productions that contain repetitions, and
 * productions whose right-hand-sides lack reduction expressions.
 */

object Normalization {
  import org.sufrin.scalalr.stage2.AST._
  import org.sufrin.scalalr.stage2.Generator.warn

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
    if (delayedFields != fields) warn(s"Shortening (${fields.mkString(" ")})$repeatType at $START...$END to (${delayedFields.mkString(" ")})$repeatType ")

    syntheticRules ::= DelayedRule(theName, delayedFields, repeatType, START, END)
    theName
  }

  def forceRule(symbolTable: SymbolTables)(delayedRule: DelayedRule): List[Rule] = {
    import delayedRule._
    def hasNoType(field: NamedField): Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType
    val searchOrdered = fields.iterator.filterNot(_.isAnonymous) ++ fields.iterator.filterNot(hasNoType) ++ fields.iterator
    repeatType match {
      case MaybeOne =>
        val field = searchOrdered.next()
        val theType = symbolTable.symbolType(field.theField)
        val theFieldName = field.theFieldName match {
          case None => field.theField
          case Some(other) => other
        }
        val theSomething = if (theType.isNoType) "Some(())" else s"Some($$$theFieldName)"
        val lhs = TypedNonterminal(theName, OptionType(theType, START), START)
        val rhs = List(
          Production(Nil, Some(Expression("None")), None, START),
          Production(fields, Some(Expression(theSomething)), None, START)
        )
        List(Rule(lhs, rhs, START))

      case OneOrMore | NoneOrMore =>
        val field = searchOrdered.next() // the first named or typed field
        val theType = symbolTable.symbolType.getOrElse(field.theField, NoType)
        val theFieldName = field.theFieldName match {
          case None        => field.theField
          case Some(other) => other
        }

        // force any "punctuation" symbol to the front of the body of the iteration
        val reorderedFields = fields match {
          case List(l, r) if (hasNoType(l))  =>
            fields
          case List(l, r) if (hasNoType(r))  =>
            warn(s"Reordering ($l $r)$repeatType  at $START...$END to ($r $l)$repeatType (for natural left recursion)")
            List(r, l)
          case _ => fields
        }

        val theListName = Name(theName.forScala++"_L", false, START)
        val lhs = TypedNonterminal(theListName, ListType(theType, START), START)
        val rhs = List(
          Production(fields.iterator.filterNot(hasNoType).toList,    Some(Expression(s"List($$$theFieldName)")), None, START),
          Production(NamedField(None, theListName, START) :: reorderedFields, Some(Expression(s"$$$theFieldName :: $$$theListName")), None, START)
        )
        val revlhs:    TypedNonterminal = TypedNonterminal(theName, ListType(theType, START), START)
        val orNothing: List[Production] = if (repeatType==NoneOrMore) List(Production(Nil, Some(Expression("Nil")), None, START)) else Nil
        val revrhs:    Production       = Production(List(NamedField(None, theListName, START)), Some(Expression(s"$$$theListName.reverse")), None, START)
        List(Rule(lhs, rhs, START), Rule(revlhs, revrhs::orNothing, START))
    }
  }

  /**
   *
   * Invent/infer a reduction for a production that lacks one
   * This is only effective if the production has exactly one value-carrying symbol
   */
  def inferReduction(symbolTable: SymbolTables)(rule: Rule): Rule = {
    def hasNoType(field: NamedField): Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType

    // TODO: needs a little refactoring!
    def toExpression(field: NamedField): Expression =
      symbolTable.symbolType.get(field.theField) match {
        case None =>
          warn(s"Named symbol $field has no type")
          Expression(" () ")
        case Some(theType) =>
        val scalaType = theType.scalaTypeName
        field.theFieldName match {
          case Some(name) =>
            if (theType == NoType) {
              warn(s"Named symbol ${name}: ${Type} carries no value")
              Expression(s"${mangle(name)}")
            }
            else
              Expression(s"${mangle(name)}")

          case None =>
            if (theType == NoType) Expression("None") else Expression(s"${mangle(field.theField)}")
        }
    }

    if (rule.rhs.forall(_.reduction.isDefined)) rule else {
      val newRHS =
        for { production <- rule.rhs } yield
          production.symbols.length match {
            case 0 =>
              warn(s"""\n Using universal default reduction expression value \"()\" for the production at: ${production.location}
                      | this is because the production is empty.
                      | Recommended remedy: specify the reduction expression explicitly.
                      | """.stripMargin)
              production.copy(reduction = Some(Expression(" ()) ")))
            case 1 =>
              val field = production.symbols.head
              val result: Name  =
                field.theFieldName match {
                  case Some(name) => name
                  case None       => field.theField
                }
                production.copy(reduction = Some(Expression(s"$$$result")))
            case n =>
              val searchOrdered = production.symbols.filterNot(hasNoType)
              searchOrdered.length match {
                case 0 =>
                  warn(s"""\n Using universal default reduction expression value \"()\" for the production at: ${production.location}
                          | this is because the production has no value-carrying symbols.
                          | Recommended remedy: specify the reduction expression explicitly.""".stripMargin)
                  production.copy(reduction = Some(Expression(" () ")))
                case 1 =>
                  val field = searchOrdered.head
                  val result: Name  =
                    field.theFieldName match {
                      case Some(name) => name
                      case None       => field.theField
                    }
                  production.copy(reduction = Some(Expression(s"$$$result")))
                case n =>
                  warn(
                    s"""\n Using universal default reduction expression value \"()\" for the production $production at: ${production.location}
                       | This is because the production's intended value cannot be determined (there is more than one value-carrying symbol).
                       | Recommended remedy: specify the reduction expression explicitly.
                       |""".stripMargin)
                  production.copy(reduction =  Some(Expression(" () ")))
              }


          }
      rule.copy(rhs=newRHS)
    }
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
        println(f"$i%03d   ${lhs.toString} ${" " * (width - lhs.toString.size)} = $prod")
      }
    }
    notation.copy(theRules = inferencedRules)
  }

}
