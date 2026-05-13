

package expr.Expr
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 trait Expr
 case class Id(s: String, loc: SourceLocation) extends Expr
 case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr
 case class Bra(expr: Expr, loc: SourceLocation)extends Expr

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* exprs: List[Expr@62.17]@62.12 = expr{ List($expr) } */
 case 1 => 
  { case List(dol$expr: Expr) =>  List(dol$expr) } 
 /* exprs: List[Expr@62.17]@62.12 = exprs `;` expr{ $expr::$exprs } */
 case 2 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
        dol$expr::dol$exprs
  }
 /* exprs: List[Expr@62.17]@62.12 = error{ List(Id("RECOVER", $START)) } */
 case 3 => 
  { case List(_) =>  List(Id("RECOVER", dol$START)) } 
 /* expr: Expr@67.10 = ID{ Id($ID, $START) } */
 case 4 => 
  { case List(dol$ID: String) =>  Id(dol$ID, dol$START) } 
 /* expr: Expr@67.10 = l: expr `*` r: expr{ Binop("*", $l, $r, $START) } */
 case 5 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("*", dol$l, dol$r, dol$START) } 
 /* expr: Expr@67.10 = l: expr `+` r: expr{ Binop("+", $l, $r, $START) } */
 case 6 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("+", dol$l, dol$r, dol$START) } 
 /* expr: Expr@67.10 = `(` expr `)`{ Bra($expr, $START) } */
 case 7 => 
  { case List(_, dol$expr: Expr, _) =>  Bra(dol$expr, dol$START) } 
 /* expr: Expr@67.10 = `[` expr `]`{ $expr } */
 case 8 => 
  { case List(_, dol$expr: Expr, _) =>  dol$expr } 

 }

}
