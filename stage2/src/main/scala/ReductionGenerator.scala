package org.sufrin.scalalr
package stage2

import org.sufrin.scalalr.stage2.AST.{Expression, Name, NamedField, NoType, Notation, mangle}
import org.sufrin.scalalr.stage2.Generator.warn
import org.sufrin.utility.SourceCode

/**
 * TODO: reduction table could avoid overflowing the code bounds on functions if
 * writtten in the following form:
 * for the ith production:
 * def red#i(dol$START: SourceLocation, dol$END: SourceLocation): PartialFunction[List[Any], Any] = { case pattern#i => expr#i }
 * and
 * def reduction(START: SourceLocation, END: SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 * ... for each production #1
 * case i => red#i(START, END)
 * }
 */
class ReductionGenerator(notation: Notation, symbolTables: SymbolTables) extends SourceCode {
  import symbolTables.{symbolType}
  import notation.thePackage
  val theUnion = notation.theTokenType.name
  val theRules = notation.theRules
  val theRulesInclude = notation.theRulesInclude



  def toPattern(field: NamedField): String = {
    val Type = symbolType(field.theField)
    val scalaType = Type.scalaTypeName
    field.theFieldName match {
      case Some(name) =>
        if (Type == NoType) {
          warn(s"Named symbol ${name}: ${Type} carries no value")
          "_"
        }
        else
          s"${mangle(name)}: ${scalaType}"

      case None =>
        if (Type == NoType) "_" else s"${mangle(field.theField)}: ${scalaType}"

    }
  }



  /*
   *  Suppress the match for duplicated symbols: they need naming
   */
  val matchAll = Some("_")

  def sameFieldType(thisField: NamedField)(thatField: NamedField): Boolean =
    symbolType(thisField.theField).scalaTypeName==symbolType(thatField.theField).scalaTypeName

  /** Avoid giving gratis names to fields  */
  def toPatterns(fields: Seq[NamedField]): Seq[String] = {
    val anonfields = fields.filter(_.isAnonymous)
    for {field <- fields} yield
      if (anonfields.filter(sameFieldType(field)(_)).length <= 1) toPattern(field) else "_"
  }

  def outReduction(): Unit = {
    out("def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
    var productionNum = 0
    for {rule <- theRules} {
      for {production <- rule.rhs} {
        productionNum += 1
        out(s" /* ${rule.lhs} = ${production} */")
        val pat = toPatterns(production.symbols).mkString("List(", ", ", ") => ")
        out(s" case $productionNum => \n  { case ${pat}")

        production.reduction match {
          // No explicit result expression
          case None =>
            production.symbols.length match {
              case 1 =>
                val field = production.symbols.head
                val result: Name  =
                field.theFieldName match {
                  case Some(name) => name
                  case None       => field.theField
                }
                gen(s" ${mangle(result.forScala)} }")
              case _ =>
                warn(s"No obvious value for reduction at: ${production.location}")
                gen(" None }")
            }

          case Some(expression) =>
            val mangled = expression.mangle
            if (mangled.size + pat.size < 80) out(s" ${mangled} } ", false) else out(s"        ${mangled}\n  }")
        }
      }
    }


    out("\n }\n")
  }

  def outTreeReduction(): Unit = {
    out("case class PARSETREE(prod: String, rule: Int, trees:List[Any])")
    out("def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
    var ruleNum = 0
    for {rule <- theRules} {
      // val lhsName = s"\"${rule.lhs.theName}\""
      for {production <- rule.rhs} {
        ruleNum += 1
        val wholeProduction = s"${rule.lhs} = ${production}"
        //out(s"\n // ${wholeProduction}")
        out(s""" case $ruleNum => \n  { case trees$$trees => PARSETREE(\"\"\"$wholeProduction\"\"\", $ruleNum, trees$$trees ) }""")
      }
    }
    out(" }\n")
  }

  out(s"\npackage $thePackage\nobject Reduction {")

  out("\n")

  out(theRulesInclude)

  outReduction()
  //outTreeReduction()
  out("}\n")
}
