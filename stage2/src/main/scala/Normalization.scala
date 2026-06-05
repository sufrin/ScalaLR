package org.sufrin.scalalr
package stage2

/**
 * Support for the normalization of productions that contain repetitions, and
 * productions whose right-hand-sides lack reduction expressions.
 */

object Normalization  {
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
    if (delayedFields != fields) warn(s"Shortening (${fields.mkString(" ")})$repeatType at $START--$END to (${delayedFields.mkString(" ")})$repeatType ")

    syntheticRules ::= DelayedRule(theName, delayedFields, repeatType, START, END)
    theName
  }


  def forceRule(symbolTable: SymbolTables)(delayedRule: DelayedRule): List[Rule] = {
    import delayedRule._

    def RULE(theName: Name, theType: SymbolType)(rhs: Production*): Rule = {
      Rule(TypedNonterminal(theName, theType, START), rhs, START)
    }

    def PROD(symbols: NamedField*)(reduction: String): Production =
       Production(symbols, Some(Expression(reduction)), None, START)

    def PRODUCTION(symbols: Iterable[NamedField], after: Name*)(reduction: String): Production =
       Production(symbols.toList ++ after.map(name => NamedField(None, name, START)), Some(Expression(reduction)), None, START)

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
                warn(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded  right recursion")
                List(r, l)
              case List(_) =>
                warn(s"(${fields.mkString(" ")})$repeatType  at $START--$END a right recursion is space-inefficient (use *)")
                fields
              case _ =>
                warn(s"(${fields.mkString(" ")})$repeatType  at $START--$END  has no punctuation-guarded right recursion")
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
                warn(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded  right recursion")
                List(r, l)
              case List(_) =>
                warn(s"(${fields.mkString(" ")})$repeatType  at $START--$END a right recursion is space-inefficient (use +)")
                fields
              case _ =>
                warn(s"${fields.mkString(" ")}$repeatType  at $START--$END  has no punctuation-guarded right recursion")
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
                Some((l, r))
              case List(l, r) if  l.hasType && r.hasNoType  =>
                warn(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded left recursion")
                Some((r, l))
              case _ =>
                warn(s"(${fields.mkString(" ")})$repeatType means nothing at $START--$END $repeatType\n Use * or + instead")
                None
            }
            ordered match {
              case None =>
                List()
              case Some((punct: NamedField, info: NamedField)) =>
                val theListName = Name(theName.forScala ++ "_E", false, START)
                List(
                  RULE(theListName, LIST(theType))(
                    PROD(theListName.asField, punct, info)(s"$$$theFieldName :: $$$theListName"),
                    PRODUCTION(theField)(s"List($$$theFieldName)"),
                    PROD(theListName.asField, punct)(s"$$$theListName")
                  ),
                  RULE(theName, LIST(theType))(PROD(theListName.asField)(s"$$$theListName.reverse"))
                )
            }

          case _ =>
            // force any "punctuation" symbol to the front of the body of the iteration
            val reorderedFields = fields match {
              case List(l, r) if l.hasNoType && r.hasType  =>
                fields
              case List(l, r) if l.hasType && r.hasNoType =>
                warn(s"Reordering ($l $r)$repeatType  at $START--$END to ($r $l)$repeatType for punctuation-guarded left recursion")
                List(r, l)
              case _ =>
                warn(s"(${fields.mkString(" ")})$repeatType  at $START--$END  cannot be reordered for punctuation-guarded left recursion")
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

  /**
   *
   * Invent/infer a reduction for a production that lacks one
   * This is only effective if the production has exactly one value-carrying symbol
   */
  def inferReduction(symbolTable: SymbolTables)(rule: Rule): Rule = {
    implicit class FieldTyping(field: NamedField) {
      def hasNoType: Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType
      def hasType:   Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)!=NoType
    }

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

    def inferredProduction(production: Production): Production = {
      if (production.reduction.isDefined) production else
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
          val searchOrdered = production.symbols.filterNot(_.hasNoType)
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

    }

    if (rule.rhs.forall(_.reduction.isDefined)) rule else {
      val newRHS = for { production <- rule.rhs } yield inferredProduction(production)
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
