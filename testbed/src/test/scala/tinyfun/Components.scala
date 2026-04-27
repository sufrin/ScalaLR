// 
// 
// 2026-04-26T14:49:06.379897
package tinyfun
object Components extends org.sufrin.scalalr.LRParserComponents {
  import org.sufrin.scalalr.Action.Action
  import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}
  import org.sufrin.scalalr.SourceLocation
  val action: State=>Terminal=>Action =tinyfun.Tables.ACTIONTABLE
  val goto: State => NonTerminal => State =tinyfun.Tables.GOTOTABLE
  val reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any] =tinyfun.Reduction.reduction
  val symbolName: Map[Symbol, String] =tinyfun.Scanner.symbolName
}

