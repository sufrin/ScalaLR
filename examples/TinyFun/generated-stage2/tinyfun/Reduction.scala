

package tinyfun
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import tinyfun.TinyFun._

  def hex(chars: Seq[Char]): Long  = chars.foldLeft(0L) { (acc, c) => acc * 16 + Character.digit(c, 16) }
  def dec(chars: Seq[Char]): Long  = chars.foldLeft(0L) { (acc, c) => acc * 10 + Character.digit(c, 10) }


def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit =  { () }  */
 case 1 => 
  { case List() =>  () } 
 /* loop: Unit = loop command NL { () }  */
 case 2 => 
  { case List(_, _, _) =>  () } 
 /* command: Unit = expressions { run($expressions, "> ") }  */
 case 3 => 
  { case List(dol$expressions: List[Expr @unchecked]) =>  run(dol$expressions, "> ") } 
 /* command: Unit = `QUIT` { System.exit(0) }  */
 case 4 => 
  { case List(_) =>  System.exit(0) } 
 /* expr: Expr = simple { $simple }  */
 case 5 => 
  { case List(dol$simple: Expr) =>  dol$simple } 
 /* expr: Expr = `-` simple { Neg($simple, $START) }  */
 case 6 => 
  { case List(_, dol$simple: Expr) =>  Neg(dol$simple, dol$START) } 
 /* expr: Expr = NAME `=` expr { Assign($NAME, $expr, $START) }  */
 case 7 => 
  { case List(dol$NAME: String, _, dol$expr: Expr) => 
        Assign(dol$NAME, dol$expr, dol$START)
  }
 /* expr: Expr = l: expr `^` r: expr { Binop("^", $l, $r, $START) }  */
 case 8 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("^", dol$l, dol$r, dol$START) } 
 /* expr: Expr = l: expr `*` r: expr { Binop("*", $l, $r, $START) }  */
 case 9 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("*", dol$l, dol$r, dol$START) } 
 /* expr: Expr = l: expr `+` r: expr { Binop("+", $l, $r, $START) }  */
 case 10 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("+", dol$l, dol$r, dol$START) } 
 /* expr: Expr = l: expr `/` r: expr { Binop("/", $l, $r, $START) }  */
 case 11 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("/", dol$l, dol$r, dol$START) } 
 /* expr: Expr = l: expr `-` r: expr { Binop("-", $l, $r, $START) }  */
 case 12 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Binop("-", dol$l, dol$r, dol$START) } 
 /* expr: Expr = NAME `(` expressions `)` { Apply($NAME, $expressions, $START) }  */
 case 13 => 
  { case List(dol$NAME: String, _, dol$expressions: List[Expr @unchecked], _) => 
        Apply(dol$NAME, dol$expressions, dol$START)
  }
 /* expr: Expr = `*` `(` expressions `)` { Apply("*", $expressions, $START) }  */
 case 14 => 
  { case List(_, _, dol$expressions: List[Expr @unchecked], _) => 
        Apply("*", dol$expressions, dol$START)
  }
 /* expr: Expr = `+` `(` expressions `)` { Apply("+", $expressions, $START) }  */
 case 15 => 
  { case List(_, _, dol$expressions: List[Expr @unchecked], _) => 
        Apply("+", dol$expressions, dol$START)
  }
 /* expr: Expr = `/` `(` expressions `)` { Apply("/", $expressions, $START) }  */
 case 16 => 
  { case List(_, _, dol$expressions: List[Expr @unchecked], _) => 
        Apply("/", dol$expressions, dol$START)
  }
 /* expr: Expr = `-` `(` expressions `)` { Apply("-", $expressions, $START) }  */
 case 17 => 
  { case List(_, _, dol$expressions: List[Expr @unchecked], _) => 
        Apply("-", dol$expressions, dol$START)
  }
 /* simple: Expr = NAME { Id($NAME, $START) }  */
 case 18 => 
  { case List(dol$NAME: String) =>  Id(dol$NAME, dol$START) } 
 /* simple: Expr = NUM { Num($NUM, $START) }  */
 case 19 => 
  { case List(dol$NUM: Double) =>  Num(dol$NUM, dol$START) } 
 /* simple: Expr = `(` expr `)` { $expr }  */
 case 20 => 
  { case List(_, dol$expr: Expr, _) =>  dol$expr } 
 /* simple: Expr = `(` error `)` { Num(1.0/0.0, $START) }  */
 case 21 => 
  { case List(_, _, _) =>  Num(1.0/0.0, dol$START) } 
 /* expressions: List[Expr] = exprs { $exprs.reverse }  */
 case 22 => 
  { case List(dol$exprs: List[Expr @unchecked]) =>  dol$exprs.reverse } 
 /* exprs: List[Expr] = expr { $expr :: Nil }  */
 case 23 => 
  { case List(dol$expr: Expr) =>  dol$expr :: Nil } 
 /* exprs: List[Expr] = exprs `,` expr { $expr :: $exprs }  */
 case 24 => 
  { case List(dol$exprs: List[Expr @unchecked], _, dol$expr: Expr) => 
        dol$expr :: dol$exprs
  }
 /* NUM: Double = HEX { hex($HEX.toString).toDouble }  */
 case 25 => 
  { case List(dol$HEX: Seq[Char @unchecked]) =>  hex(dol$HEX.toString).toDouble } 
 /* NUM: Double = DEC { $DEC.toString.toDouble }  */
 case 26 => 
  { case List(dol$DEC: Seq[Char @unchecked]) =>  dol$DEC.toString.toDouble } 
 /* NUM: Double = REAL { $REAL.toString.toDouble }  */
 case 27 => 
  { case List(dol$REAL: Seq[Char @unchecked]) =>  dol$REAL.toString.toDouble } 
 /* NAME: String = ID { $ID.toString }  */
 case 28 => 
  { case List(dol$ID: Seq[Char @unchecked]) =>  dol$ID.toString } 

 }

}
