

package tinyfun
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import tinyfun.TinyFun._
 import org.sufrin.utility.RevSeq

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit@93.10 = { () } */
 case 1 => 
  { case List() =>  () } 
 /* loop: Unit@93.10 = loop command NL{ () } */
 case 2 => 
  { case List(_, _, _) =>  () } 
 /* loop: Unit@93.10 = loop error NL{ () } */
 case 3 => 
  { case List(dol$loop: Unit, _, _) =>  () } 
 /* command: Unit@99.13 = exprs{ run($exprs.toList) } */
 case 4 => 
  { case List(dol$exprs: RevSeq[Expr @unchecked]) =>  run(dol$exprs.toList) } 
 /* command: Unit@99.13 = QUIT{ System.exit(0) } */
 case 5 => 
  { case List(_) =>  System.exit(0) } 
 /* command: Unit@99.13 = { System.exit(0) } */
 case 6 => 
  { case List() =>  System.exit(0) } 
 /* expr: Expr@102.10 = ID{ Id($ID, $START) } */
 case 7 => 
  { case List(dol$ID: String) =>  Id(dol$ID, dol$START) } 
 /* expr: Expr@102.10 = NUM{ Num($NUM.toDouble, $START) } */
 case 8 => 
  { case List(dol$NUM: String) =>  Num(dol$NUM.toDouble, dol$START) } 
 /* expr: Expr@102.10 = ID `=` expr{ Assign($ID, $expr, $START) } */
 case 9 => 
  { case List(dol$ID: String, _, dol$expr: Expr) =>  Assign(dol$ID, dol$expr, dol$START) } 
 /* expr: Expr@102.10 = l: expr `^` r: expr{ Binop("^", $l, $r, $START) } */
 case 10 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("^", dol$l, dol$r, dol$START) } 
 /* expr: Expr@102.10 = l: expr `*` r: expr{ Binop("*", $l, $r, $START) } */
 case 11 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("*", dol$l, dol$r, dol$START) } 
 /* expr: Expr@102.10 = l: expr `+` r: expr{ Binop("+", $l, $r, $START) } */
 case 12 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("+", dol$l, dol$r, dol$START) } 
 /* expr: Expr@102.10 = l: expr `/` r: expr{ Binop("/", $l, $r, $START) } */
 case 13 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("/", dol$l, dol$r, dol$START) } 
 /* expr: Expr@102.10 = l: expr `-` r: expr{ Binop("-", $l, $r, $START) } */
 case 14 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("-", dol$l, dol$r, dol$START) } 
 /* expr: Expr@102.10 = `(` expr `)`{ $expr } */
 case 15 => 
  { case List(_, dol$expr: Expr, _) =>  dol$expr } 
 /* expr: Expr@102.10 = ID `(` exprs `)`{ Apply($ID, $exprs.toList, $START) } */
 case 16 => 
  { case List(dol$ID: String, _, dol$exprs: RevSeq[Expr @unchecked], _) => 
        Apply(dol$ID, dol$exprs.toList, dol$START)
  }
 /* exprs: RevSeq[Expr@115.19]@115.14 = expr{ RevSeq($expr) } */
 case 17 => 
  { case List(dol$expr: Expr) =>  RevSeq(dol$expr) } 
 /* exprs: RevSeq[Expr@115.19]@115.14 = exprs `,` expr{ $exprs :+ $expr } */
 case 18 => 
  { case List(dol$exprs: RevSeq[Expr @unchecked], _, dol$expr: Expr) => 
        dol$exprs :+ dol$expr
  }

 }

}
