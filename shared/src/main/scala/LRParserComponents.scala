package org.sufrin.scalalr

import org.sufrin.scalalr.Action.Action
import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}

/**
 * Aggregation of action tables, goto tables, reductions, and symbol names
 */
trait LRParserComponents {
  val action:     State=>Terminal=>Action
  val goto:       State => NonTerminal => State
  val reduction:  (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any]
  val symbolName: Map[Symbol, String]
}
