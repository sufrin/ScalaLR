package org.sufrin.scalalr
package stage2

import org.sufrin.scalalr.stage2.AST.{Name, NamedField, NoType, Notation, mangle}
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
  import notation.thePackage
  import symbolTables.symbolType
  val theUnion = notation.theTokenType.name
  val theRules = notation.theRules
  val theRulesInclude = notation.theRulesInclude



  def toPattern(field: NamedField): String = {
    val Type = symbolType(field.theField)
    val scalaType = Type.scalaTypeName
    field.theFieldName match {
      case Some(name) =>
        if (Type == NoType) {
          warn(s"Named ${field} has no substantive value ${field.location}.")
          "_"
        }
        else
          s"${mangle(name)}: ${scalaType}"

      case None =>
        if (Type == NoType) "_" else s"${mangle(field.theField)}: ${scalaType}"

    }
  }



  /*
   *  Suppress the match for duplicated symbols: they need naming to be used
   */
  val matchAll = Some("_")

  def sameFieldType(thisField: NamedField)(thatField: NamedField): Boolean =
    symbolType(thisField.theField).scalaTypeName==symbolType(thatField.theField).scalaTypeName

  /** Avoid giving gratis names to fields  */
  def toPatterns(fields: Seq[NamedField]): Seq[String] = {
    val anonfields = fields.filter(_.isAnonymous)
    for {field <- fields} yield
      if (!field.isAnonymous || anonfields.filter(sameFieldType(field)(_)).length <= 1)
        toPattern(field)
      else
        " _ "
  }

  def outReduction(): Unit = {
    val (logRed) = Generator.logGeneration.contains("red")
    if (logRed) {
      println("\nType environments during reduction generation")
    }

    out("def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
    var productionNum = 0
    for {rule <- theRules} {
      for {production <- rule.rhs} {
        val fields:     Seq[NamedField] = production.symbols

        productionNum += 1
        out(s" /* ${rule.lhs} = ${production} */")
        val patterns = toPatterns(production.symbols)
        val pat = patterns.mkString("List(", ", ", ") => ")
        out(s" case $productionNum => \n  { case ${pat}")

        if (logRed) {

          val anonfields: Seq[NamedField] = fields.filter(_.isAnonymous)

          val fieldTypes =
            for {field <- fields} yield
              s"${if (field.theFieldName.isDefined) field.theFieldName.get + "::" else ""}${field.theField}: ${symbolType(field.theField).scalaTypeName}" // anonfields.filter(sameFieldType(field)(_))"

          print(
            s"""  $productionNum: ${rule.lhs} = ${production}
               |       PROD ${fields.mkString("(", "  ", ")")}
               |       TYPE ${fieldTypes.mkString("; ")}
               |       PATS ${patterns.mkString("(", ", ", ")")}
               |       ANON ${anonfields.mkString(", ")}
               |
               |""".stripMargin)
        }

        production.reduction match {
          // No explicit result expression
          case None =>
            production.symbols.length match {
              case 1 =>
                val field = production.symbols.head
                val fieldType = symbolType(field.theField)
                val result: Name  =
                field.theFieldName match {
                  case Some(name) => name
                  case None       => field.theField
                }
                if (fieldType.isNoType) " () " else gen(s" ${mangle(result.forScala)} }")
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

  out(s"\npackage $thePackage\nobject Reduction {")

  out("\n")

  out(theRulesInclude)

  outReduction()
  //outTreeReduction()
  out("}\n")
}



