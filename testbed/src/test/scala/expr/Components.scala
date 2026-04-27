
package expr.Expr
object Components extends org.sufrin.scalalr.LRParserComponents {
  import org.sufrin.scalalr.Action.Action
  import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}
  import org.sufrin.scalalr.SourceLocation
  val action: State=>Terminal=>Action =expr.Expr.Tables.ACTIONTABLE
  val goto: State => NonTerminal => State =expr.Expr.Tables.GOTOTABLE
  val reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any] =expr.Expr.Reduction.reduction
  val symbolName: Map[Symbol, String] =expr.Expr.Scanner.symbolName
}

