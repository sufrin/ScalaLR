
package tinyfun
object Reduction {

 import org.sufrin.scalalr.SourceLocation
 import tinyfun.TinyFun._

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // loop: Unit =  { () }
 case 1 => 
  { case List() =>   ()  } 
 // loop: Unit = loop command NL { () }
 case 2 => 
  { case List(dol$loop: Unit, dol$command: Unit, _) =>   ()  } 
 // loop: Unit = error NL { () }
 case 3 => 
  { case List(_, _) =>   ()  } 
 // command: Unit = expr { Fun.Syntax.run(List($expr)) }
 case 4 => 
  { case List(dol$expr: Expr) => 
         Fun.Syntax.run(List(dol$expr)) 
  }
 // expr: Expr = ID { Id($ID, $START) }
 case 5 => 
  { case List(dol$ID: String) => 
         Id(dol$ID, dol$START) 
  }
 // expr: Expr = NUM { Num($NUM.toDouble, $START) }
 case 6 => 
  { case List(dol$NUM: String) => 
         Num(dol$NUM.toDouble, dol$START) 
  }
 // expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }
 case 7 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("*", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }
 case 8 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("+", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "/" r: expr { Binop("/", $l, $r, $START) }
 case 9 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("/", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "-" r: expr { Binop("-", $l, $r, $START) }
 case 10 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("-", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = "(" expr ")" { $expr }
 case 11 => 
  { case List(_, dol$expr: Expr, _) =>   dol$expr  } 
 // expr: Expr = ID "(" exprs ")" { Apply($ID, $exprs, $START) }
 case 12 => 
  { case List(dol$ID: String, _, dol$exprs: List[Expr @unchecked], _) => 
         Apply(dol$ID, dol$exprs, dol$START) 
  }
 // exprs: List[Expr] = expr { List($expr) }
 case 13 => 
  { case List(dol$expr: Expr) =>   List(dol$expr)  } 
 // exprs: List[Expr] = exprs "," expr { $expr::$exprs }
 case 14 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
         dol$expr::dol$exprs 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""loop: Unit =  { () }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""loop: Unit = loop command NL { () }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""loop: Unit = error NL { () }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""command: Unit = expr { Fun.Syntax.run(List($expr)) }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID { Id($ID, $START) }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""expr: Expr = NUM { Num($NUM.toDouble, $START) }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }""", 8, trees$trees ) }
 case 9 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "/" r: expr { Binop("/", $l, $r, $START) }""", 9, trees$trees ) }
 case 10 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "-" r: expr { Binop("-", $l, $r, $START) }""", 10, trees$trees ) }
 case 11 => 
  { case trees$trees => PARSETREE("""expr: Expr = "(" expr ")" { $expr }""", 11, trees$trees ) }
 case 12 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID "(" exprs ")" { Apply($ID, $exprs, $START) }""", 12, trees$trees ) }
 case 13 => 
  { case trees$trees => PARSETREE("""exprs: List[Expr] = expr { List($expr) }""", 13, trees$trees ) }
 case 14 => 
  { case trees$trees => PARSETREE("""exprs: List[Expr] = exprs "," expr { $expr::$exprs }""", 14, trees$trees ) }
 }

}
