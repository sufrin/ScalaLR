
package tinyfun
object Reduction {

 import org.sufrin.scalalr.SourceLocation
 import tinyfun.TinyFun._
 import org.sufrin.utility.RevSeq

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // loop: Unit =  { () }
 case 1 => 
  { case List() =>   ()  } 
 // loop: Unit = loop command NL { () }
 case 2 => 
  { case List(dol$loop: Unit, dol$command: Unit, _) =>   ()  } 
 // loop: Unit = loop error NL { () }
 case 3 => 
  { case List(dol$loop: Unit, _, _) =>   ()  } 
 // command: Unit = exprs { run($exprs.toList) }
 case 4 => 
  { case List(dol$exprs: RevSeq[Expr @unchecked]) => 
         run(dol$exprs.toList) 
  }
 // command: Unit = QUIT { System.exit(0) }
 case 5 => 
  { case List(_) =>   System.exit(0)  } 
 // command: Unit =  { System.exit(0) }
 case 6 => 
  { case List() =>   System.exit(0)  } 
 // expr: Expr = ID { Id($ID, $START) }
 case 7 => 
  { case List(dol$ID: String) => 
         Id(dol$ID, dol$START) 
  }
 // expr: Expr = NUM { Num($NUM.toDouble, $START) }
 case 8 => 
  { case List(dol$NUM: String) => 
         Num(dol$NUM.toDouble, dol$START) 
  }
 // expr: Expr = ID "=" expr { Assign($ID, $expr, $START) }
 case 9 => 
  { case List(dol$ID: String, _, dol$expr: Expr) => 
         Assign(dol$ID, dol$expr, dol$START) 
  }
 // expr: Expr = l: expr "^" r: expr { Binop("^", $l, $r, $START) }
 case 10 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("^", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }
 case 11 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("*", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }
 case 12 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("+", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "/" r: expr { Binop("/", $l, $r, $START) }
 case 13 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("/", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = l: expr "-" r: expr { Binop("-", $l, $r, $START) }
 case 14 => 
  { case List(dol$l: Expr, _, dol$r: Expr) => 
         Binop("-", dol$l, dol$r, dol$START) 
  }
 // expr: Expr = "(" expr ")" { $expr }
 case 15 => 
  { case List(_, dol$expr: Expr, _) =>   dol$expr  } 
 // expr: Expr = ID "(" exprs ")" { Apply($ID, $exprs.toList, $START) }
 case 16 => 
  { case List(dol$ID: String, _, dol$exprs: RevSeq[Expr @unchecked], _) => 
         Apply(dol$ID, dol$exprs.toList, dol$START) 
  }
 // exprs: RevSeq[Expr] = expr { RevSeq($expr) }
 case 17 => 
  { case List(dol$expr: Expr) =>   RevSeq(dol$expr)  } 
 // exprs: RevSeq[Expr] = exprs "," expr { $exprs :+ $expr }
 case 18 => 
  { case List(dol$exprs: RevSeq[Expr @unchecked], _, dol$expr: Expr) => 
         dol$exprs :+ dol$expr 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""loop: Unit =  { () }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""loop: Unit = loop command NL { () }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""loop: Unit = loop error NL { () }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""command: Unit = exprs { run($exprs.toList) }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""command: Unit = QUIT { System.exit(0) }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""command: Unit =  { System.exit(0) }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID { Id($ID, $START) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""expr: Expr = NUM { Num($NUM.toDouble, $START) }""", 8, trees$trees ) }
 case 9 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID "=" expr { Assign($ID, $expr, $START) }""", 9, trees$trees ) }
 case 10 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "^" r: expr { Binop("^", $l, $r, $START) }""", 10, trees$trees ) }
 case 11 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "*" r: expr { Binop("*", $l, $r, $START) }""", 11, trees$trees ) }
 case 12 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "+" r: expr { Binop("+", $l, $r, $START) }""", 12, trees$trees ) }
 case 13 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "/" r: expr { Binop("/", $l, $r, $START) }""", 13, trees$trees ) }
 case 14 => 
  { case trees$trees => PARSETREE("""expr: Expr = l: expr "-" r: expr { Binop("-", $l, $r, $START) }""", 14, trees$trees ) }
 case 15 => 
  { case trees$trees => PARSETREE("""expr: Expr = "(" expr ")" { $expr }""", 15, trees$trees ) }
 case 16 => 
  { case trees$trees => PARSETREE("""expr: Expr = ID "(" exprs ")" { Apply($ID, $exprs.toList, $START) }""", 16, trees$trees ) }
 case 17 => 
  { case trees$trees => PARSETREE("""exprs: RevSeq[Expr] = expr { RevSeq($expr) }""", 17, trees$trees ) }
 case 18 => 
  { case trees$trees => PARSETREE("""exprs: RevSeq[Expr] = exprs "," expr { $exprs :+ $expr }""", 18, trees$trees ) }
 }

}
