

package expr.Expr
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 // Scala source here is included in the generated object expr.Expr.Reduction
 // that defines the result value for each production.
 // It must import or implement the abstract syntax of the language.
 // Here we do the latter

 trait Expr { val loc: SourceLocation }                                 // §4
 case class Id(s: String, loc: SourceLocation) extends Expr
 case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr
 case class Bra(expr: Expr, loc: SourceLocation)extends Expr


def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* exprs: List[Expr] = expr {  List($expr)  }  */
 case 1 => 
  { case List(dol$expr: Expr) =>  List(dol$expr) } 
 /* exprs: List[Expr] = exprs `;` expr {  $expr::$exprs  }  */
 case 2 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
        dol$expr::dol$exprs
  }
 /* expr: Expr = ID {  Id($ID, $START)  }  */
 case 3 => 
  { case List(dol$ID: String) =>  Id(dol$ID, dol$START) } 
 /* expr: Expr = l: expr `*` r: expr {  Binop("*", $l, $r, $START)  }  */
 case 4 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("*", dol$l, dol$r, dol$START) } 
 /* expr: Expr = l: expr `+` r: expr {  Binop("+", $l, $r, $START)  }  */
 case 5 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("+", dol$l, dol$r, dol$START) } 
 /* expr: Expr = `(` expr `)` {  Bra($expr, $START)  }  */
 case 6 => 
  { case List( _ , dol$expr: Expr,  _ ) =>  Bra(dol$expr, dol$START) } 
 /* expr: Expr = `[` expr `]` {  $expr  }  */
 case 7 => 
  { case List( _ , dol$expr: Expr,  _ ) =>  dol$expr } 

 }

}
