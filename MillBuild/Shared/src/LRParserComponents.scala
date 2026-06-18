package org.sufrin.scalalr

import org.sufrin.scalalr.Action.Action
import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}

/**
 * Aggregation of action tables, goto tables, reductions, symbol->name correspondence, and symbol->Token correspondence
 */
trait LRParserComponents { outer =>
  type Token <: Lexeme
  val action:      State=>Terminal=>Action
  val goto:        State => NonTerminal => State
  val reduction:   (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any]
  val symbolName:  Map[Symbol, String]
  //val symbolToken: collection.immutable.Map[String, Token]
  /** Copy of these components with a different reduction: to support debugging
  def withReduction(red:  (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any]): LRParserComponents[Token] = new LRParserComponents[Token] {
    val action: State => Terminal => Action = outer.action
    val goto: State => NonTerminal => State = outer.goto
    val reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any] = red
    val symbolName: Map[Symbol, String] = outer.symbolName
    val symbolToken: collection.immutable.Map[String, Token] = outer.symbolToken
  } */
}
