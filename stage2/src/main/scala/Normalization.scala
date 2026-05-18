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

  case class DelayedRule(theName: Name, fields: List[NamedField], repeatType: Repeat, START: SourceLocation)

  def OptionType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("Option", List(symbolType.asInstanceOf[Type]), START)

  def ListType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("List", List(symbolType.asInstanceOf[Type]), START)

  def synthesiseRepeated(fields: List[NamedField], repeatType: Repeat, START: SourceLocation, END: SourceLocation): Name = {
    synthNumber += 1
    val theName = Name(s"S$$$synthNumber", false, START)
    syntheticRules ::= DelayedRule(theName, fields, repeatType, START)
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
        val lhs = TypedNonterminal(theName, OptionType(theType, START), START)
        val rhs = List(
          Production(Nil, Some(Expression("None")), None, START),
          Production(fields, Some(Expression(s"Some($$$theFieldName)")), None, START)
        )
        List(Rule(lhs, rhs, START))

      case OneOrMore | NoneOrMore =>
        val field = searchOrdered.next()
        val theType = symbolTable.symbolType.getOrElse(field.theField, NoType)
        val theFieldName = field.theFieldName match {
          case None        => field.theField
          case Some(other) => other
        }
        val theListName = Name(theName.forScala++"LIST", false, START)
        val lhs = TypedNonterminal(theListName, ListType(theType, START), START)
        val rhs = List(
          Production(fields.iterator.filterNot(hasNoType).toList,    Some(Expression(s"List($$$theFieldName)")), None, START),
          Production(NamedField(None, theListName, START) :: fields, Some(Expression(s"$$$theFieldName :: $$$theListName")), None, START)
        )
        val revlhs    = TypedNonterminal(theName, ListType(theType, START), START)
        val orNothing: List[Production] = if (repeatType==NoneOrMore) List(Production(Nil, Some(Expression("Nil")), None, START)) else Nil
        val revrhs:    Production = Production(List(NamedField(None, theListName, START)), Some(Expression(s"$$$theListName.reverse")), None, START)
        List(Rule(lhs, rhs, START), Rule(revlhs, revrhs::orNothing, START))
    }
  }

  /**
   *
   * Invent/infer a reduction for a production that lacks one
   * This is only effective if the production has exactly one symbol
   */
  def inferReduction(symbolTable: SymbolTables)(rule: Rule): Rule = {
    def hasNoType(field: NamedField): Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType

    // TODO: needs a little refactoring!
    def toExpression(field: NamedField): Expression =
      symbolTable.symbolType.get(field.theField) match {
        case None =>
          warn(s"Named symbol $field has no type")
          Expression(" None ")
        case Some(theType) =>
        val scalaType = theType.scalaTypeName
        field.theFieldName match {
          case Some(name) =>
            if (Type == NoType) {
              warn(s"Named symbol ${name}: ${Type} carries no value")
              Expression(s"${mangle(name)}")
            }
            else
              Expression(s"${mangle(name)}")

          case None =>
            if (Type == NoType) Expression("None") else Expression(s"${mangle(field.theField)}")
        }
    }

    if (rule.rhs.forall(_.reduction.isDefined)) rule else {
      val newRHS =
        for { production <- rule.rhs } yield
          production.symbols.length match {
            case 1 =>
              val field = production.symbols.head
              val result: Name  =
                field.theFieldName match {
                  case Some(name) => name
                  case None       => field.theField
                }
                production.copy(reduction = Some(Expression(s"$$$result")))
            case n if n>0 =>
              val searchOrdered = production.symbols.filterNot(_.isAnonymous) ++ production.symbols.filterNot(hasNoType) ++ production.symbols
              searchOrdered.length match {
                case 0 =>
                  warn(s"""Using default reduction expression value \"None\" for the reduction at: ${production.location}
                           this is because the production has neither named nor value-carrying symbols""")
                  production.copy(reduction = Some(Expression(" None ")))
                case n if n>= 1 =>
                  if (n>1)   warn(s"""Ambiguity for the reduction at: ${production.location}""")
                  production.copy(reduction = Some(toExpression(searchOrdered.head)))
              }

            case 0 =>
              warn(s"""Using default reduction expression value \"None\" for the reduction at: ${production.location}
                       this is because the production has an empty RHS""")
              production.copy(reduction = Some(Expression(" None ")))
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
