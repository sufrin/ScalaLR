
package testbed.scala.expr.generated.Expr
object Reduction {

 import org.sufrin.scalalr.SourceLocation
 trait Expr
 case class Id(s: String, loc: SourceLocation) extends Expr
 case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr
 case class Bra(expr: Expr, loc: SourceLocation)extends Expr

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // exprs: List[Expr] = expr { List($expr) }
 case 1 => 
  { case List(dol$expr: Expr) =>   List(dol$expr)  } 
 // exprs: List[Expr] = exprs ";" expr { $expr::$exprs }
 case 2 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
         dol$expr::dol$exprs 
  }
 // exprs: List[Expr] = error { List(Id("RECOVER", $START)) }
 case 3 => 
  { case List(_) => 
         List(Id("RECOVER", dol$START)) 
  }
 // expr: Expr = ID { Id($ID, $START) }
 case 4 => 
  { case List(dol$ID: String) => 
         Id(dol$ID, dol$START) 
  }
 // expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }
 case 5 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("*", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }
 case 6 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("+", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = "(" expr ")" { Bra($expr, $START) }
 case 7 => 
  { case List(_, dol$expr: Expr, _) => 
         Bra(dol$expr, dol$START) 
  }
 // expr: Expr = "[" expr "]" { $expr }
 case 8 => 
  { case List(_, dol$expr: Expr, _) =>   dol$expr  } 
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""exprs: List[Expr] = expr { List($expr) }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""exprs: List[Expr] = exprs ";" expr { $expr::$exprs }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""exprs: List[Expr] = error { List(Id("RECOVER", $START)) }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID { Id($ID, $START) }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""expr: Expr = "(" expr ")" { Bra($expr, $START) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""expr: Expr = "[" expr "]" { $expr }""", 8, trees$trees ) }
 }

}
