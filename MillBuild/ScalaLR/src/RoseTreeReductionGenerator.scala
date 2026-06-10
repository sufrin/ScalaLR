package org.sufrin.scalalr
package stage2

/**
 * Reductions generate rose trees
 */

import org.sufrin.scalalr.stage2.AST.Notation
import org.sufrin.utility.SourceCode


class RoseTreeReductionGenerator(notation: Notation, symbolTables: SymbolTables) extends SourceCode {
  import notation.{thePackage, theRules}
  def outReduction(): Unit = {
    out("case class ROSETREE(nonTerminal: String, rule: Int, trees:List[Any])")
    out("def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, rule: Int): PartialFunction[List[Any], Any] = rule match {")
    var ruleNum = 0
    for {rule <- theRules} {
      // val lhsName = s"\"${rule.lhs.theName}\""
      for {production <- rule.rhs} {
        ruleNum += 1
        val nonterminal = s"${rule.lhs.theName}"
        out(s""" case $ruleNum => \n  { case trees$$trees => ROSETREE(\"\"\"$nonterminal\"\"\", $ruleNum, trees$$trees ) }""")
      }
    }
    out(" }\n")
  }
  out(s"\npackage $thePackage\nobject RoseTreeReduction {")
  out("\n")
  outReduction()
  //outTreeReduction()
  out("}\n")
}
