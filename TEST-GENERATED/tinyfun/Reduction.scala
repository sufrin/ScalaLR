

package tinyfun
object Reduction {



         import org.sufrin.scalalr.SourceLocation
         import tinyfun.TinyFun._
        
def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit@226.18 = { () } */
 case 1 => 
  { case List() =>  () } 
 /* loop: Unit@226.18 = loop command NL{ () } */
 case 2 => 
  { case List(_, _, _) =>  () } 
 /* command: Unit@231.21 = expr{ run(List($expr)) } */
 case 3 => 
  { case List(dol$expr: Expr) =>  run(List(dol$expr)) } 
 /* command: Unit@231.21 = QUIT{ System.exit(0) } */
 case 4 => 
  { case List(_) =>  System.exit(0) } 
 /* expr: Expr@234.18 = ID{ Id($ID, $START) } */
 case 5 => 
  { case List(dol$ID: String) =>  Id(dol$ID, dol$START) } 
 /* expr: Expr@234.18 = number{ $number } */
 case 6 => 
  { case List(dol$number: Expr) =>  dol$number } 
 /* expr: Expr@234.18 = ID `=` expr{ Assign($ID, $expr, $START) } */
 case 7 => 
  { case List(dol$ID: String, _, dol$expr: Expr) =>  Assign(dol$ID, dol$expr, dol$START) } 
 /* expr: Expr@234.18 = l: expr `*` r: expr{ Binop("*", $l, $r, $START) } */
 case 8 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("*", dol$l, dol$r, dol$START) } 
 /* expr: Expr@234.18 = l: expr `+` r: expr{ Binop("+", $l, $r, $START) } */
 case 9 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("+", dol$l, dol$r, dol$START) } 
 /* expr: Expr@234.18 = l: expr `/` r: expr{ Binop("/", $l, $r, $START) } */
 case 10 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("/", dol$l, dol$r, dol$START) } 
 /* expr: Expr@234.18 = l: expr `-` r: expr{ Binop("-", $l, $r, $START) } */
 case 11 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("-", dol$l, dol$r, dol$START) } 
 /* expr: Expr@234.18 = `(` expr `)`{ $expr } */
 case 12 => 
  { case List(_, dol$expr: Expr, _) =>  dol$expr } 
 /* expr: Expr@234.18 = ID `(` exprs `)`{ Apply($ID, $exprs, $START) } */
 case 13 => 
  { case List(dol$ID: String, _, dol$exprs: List[Expr @unchecked], _) => 
        Apply(dol$ID, dol$exprs, dol$START)
  }
 /* exprs: List[Expr@247.25]@247.20 = expr{ List($expr) } */
 case 14 => 
  { case List(dol$expr: Expr) =>  List(dol$expr) } 
 /* exprs: List[Expr@247.25]@247.20 = exprs `,` expr{ $expr::$exprs } */
 case 15 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
        dol$expr::dol$exprs
  }
 /* number: Expr@252.20 = NUM{ Num($NUM.toDouble, $START) } */
 case 16 => 
  { case List(dol$NUM: String) =>  Num(dol$NUM.toDouble, dol$START) } 
 /* number: Expr@252.20 = `#` NUM{ Num($NUM.toInt, $START) } */
 case 17 => 
  { case List(_, dol$NUM: String) =>  Num(dol$NUM.toInt, dol$START) } 

 }

}
