package org.sufrin.scalalr
package stage2

import org.sufrin.scalalr.Action.Action
import org.sufrin.scalalr.stage2.AST.{Name, Notation, Rule}
import org.sufrin.scalalr.stage2.Generator.{fine, finer, bisonTokenToInt}
import org.sufrin.utility.SourceCode

import scala.collection.mutable


class TableGenerator(targetPath: String, notation: Notation, symbolTables: SymbolTables) extends SourceCode {
  import notation.thePackage
  import symbolTables.{nameToNumber}

  case class StateEntry(number: Int, transitions: Seq[(Int,Action)], reductions: Seq[(Int, Action)], gotos: Seq[(Int, Action)], disabled: Int)

  def readBisonStates(name: String, theRules: Seq[Rule]): Seq[StateEntry] = {
    import Action._

    import scala.xml._
    val root      = XML.loadFile(s"$name.xml")
    val grammar   = root \\ "grammar"
    val automaton = root \\ "automaton"

    val xmlTerminals    = (grammar \\ "terminals" \\ "terminal") . toList
    val xmlNonterminals = (grammar \\ "grammar"   \\ "nonterminals" \\ "nonterminal") . toList


    val states      = automaton \\ "state"
    val stateCount  = states.length
    val symbolCount = xmlTerminals.size + xmlNonterminals.size

    /** for each nonterminal: name -> symbol number */
    val nonterminalsymbol = mutable.LinkedHashMap[String, Int]()
    locally {
      for { node <- xmlNonterminals } nonterminalsymbol(node \@ "name") = (node \@ "symbol-number").toInt
    }
    finer(s"for each nonterminal: name -> symbol number\n  ${nonterminalsymbol.toList.mkString("\n  ")}")

    /** for each production: lhs name -> rhs length  */
    val info: Seq[(Name,Int)]=
      for { rule <- theRules; production <- rule.rhs } yield
        (rule.lhs.theName, production.symbols.length)
    finer(s"for each production: lhs name -> rhs length\n  ${info.mkString("\n  ")}")

    def symbolNumber(name: Name): Int = nameToNumber(name)

    def readState(node: xml.Node): StateEntry = {
      val number      = (node \ "@number").text.toInt
      val actions     = node \\ "actions"
      val transitions = actions \\ "transitions" \\ "transition"
      val reductions  = actions \\ "reductions" \\ "reduction"

      /** The reduction corresponding to Bison's rule numbered `rule` */
      def makeREDUCE(rule: Int): REDUCE = {
        // REDUCE(symbol: Int, production: Int, size: Int) extends Action
        // the info table has origin 0
        val (name, length) = info(rule - 1)
        REDUCE(symbolNumber(name), rule, length)
      }

      lazy val allActions: Seq[(String, Action)]  =
        for {node <- (transitions)} yield {
          val symbol = (node \ "@symbol").text
          (node \ "@type").text match {
            //case "accept" => ACCEPT
            case "error"  => ((symbol), ERROR)
            case "reduce" => ((symbol), makeREDUCE((node \@ "rule").toInt))
            case "shift"  => ((symbol), SHIFT((node \ "@state").text.toInt))
            case "goto"   => ((symbol), GOTO(inState = number, toState = (node \ "@state").text.toInt))
          }
        }

      lazy val theActions: Seq[(String, Action)]  =
        for { (sy, tr) <- allActions if !tr.isInstanceOf[GOTO] } yield (sy, tr)

      lazy val theGotos: Seq[(String, Action)] =
        for { (sy, tr) <- allActions if tr.isInstanceOf[GOTO] } yield (sy, tr)

      lazy val theReductions: Seq[(String, Action)] =
        for {node <- (reductions) if (node \ "@enabled").text=="true" } yield {
          val symbol = (node \ "@symbol").text
          (node \ "@rule").text match {
            case "accept" => ((symbol), ACCEPT )
            case "error"  => ((symbol), ERROR )
            case rule     => ((symbol), makeREDUCE (rule.toInt) )
          }
        }

      lazy val disabled = (for { node <- reductions if (node \ "@enabled").text=="false" } yield 1).sum
      fine(s"State $number $theActions / $theReductions / $theGotos")

      def encodeSymbolic(table: Seq[(String, Action)]): Seq[(Int, Action) ] = table.map{  case (token, tr) => (bisonTokenToInt(token), tr) }

      val result = StateEntry(number, encodeSymbolic(theActions), encodeSymbolic(theReductions), encodeSymbolic(theGotos), disabled)
      fine(result.toString)
      result
    }
    val result: Seq[StateEntry] = states map readState
    result
  }

  import Action._
  fine(s"Making tables for: ${notation.theName}")
  val entries: Seq[StateEntry] = readBisonStates(targetPath, notation.theRules)

  out(s"package $thePackage\nobject Tables {")

  // GOTO TABLES
  gen(s"\nval goto: Int => Int => Int = {")
  for {entry <- entries if entry.gotos.nonEmpty} {
    fine(entry.toString)
    gen(
      s"\n  case ${entry.number} => { ")
    for {(sy, GOTO(from, to)) <- entry.gotos} gen(s"case $sy => $to;  ")
    gen("}")
  }
  gen("\n  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}")
  gen("\n  }\n")

  // Action TABLES
  gen(s"\nimport org.sufrin.scalalr.Action._")
  gen(s"\nval action: Int => Int => Action = {")
  for {entry <- entries} {
    fine(entry.toString)
    gen(
      s"\n  case ${entry.number} => { ")
    // for { (sy, GOTO(from, to)) <- entry.gotos } gen(s"case $sy => $to;  ")
    for {(sy, act) <- entry.transitions} {
      gen(s"case $sy => $act;  ")
    }

    for {(sy, act) <- entry.reductions if (sy >= 0)} {
      gen(s"case $sy => $act;  ")
    }
    var needsDefault = true
    for {(sy, act) <- entry.reductions if (sy < 0)} {
      gen(s"case _ => $act;  ")
      needsDefault = false
    }

    if (needsDefault) gen(s"case _ => ERROR;  ")

    gen("}")
  }
  gen("\n  case _ => { case _ => ERROR }")
  gen("\n  }\n")

  entries.foreach(e => fine(e.toString))
  gen("}\n")
}