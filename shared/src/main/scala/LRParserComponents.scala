package org.sufrin.scalalr

import org.sufrin.scalalr.Action.Action
import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}

/**
 * Aggregation of action tables, goto tables, reductions, and symbol names
 */
trait LRParserComponents { outer =>
  val action:     State=>Terminal=>Action
  val goto:       State => NonTerminal => State
  val reduction:  (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any]
  val symbolName: Map[Symbol, String]
  /** Copy of these components with a different reduction: to support debugging */
  def withReduction(red:  (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any]): LRParserComponents = new LRParserComponents {
    val action: State => Terminal => Action = outer.action
    val goto: State => NonTerminal => State = outer.goto
    val reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any] = red
    val symbolName: Map[Symbol, String] = outer.symbolName
  }
}
