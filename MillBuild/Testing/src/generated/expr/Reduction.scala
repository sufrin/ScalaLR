

package expr.Expression
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import expr.Expression._
 import org.sufrin.utility.PrettyPrint._

 trait Expr
 case class Bin(op: String, l: Expr, r: Expr) extends Expr
 case class Atom(string: String)              extends Expr
 case class Bra(e: Expr)                      extends Expr
 case class Quoted(string: String)            extends Expr { override val toString = s"$string" }

 def reHexify(string: String): String = s"0x$string"

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit = S_1 S_2 => ()  */
 case 1 => 
  { case List(dol$S_1: Option[Unit @unchecked], dol$S_2: List[String @unchecked]) =>  () } 
 /* oneLine: String = expr {  $expr.prettyPrint(); ""  }  */
 case 2 => 
  { case List(dol$expr: Expr) =>  dol$expr.prettyPrint(); "" } 
 /* expr: Expr = atom { $atom }  */
 case 3 => 
  { case List(dol$atom: Expr) =>  dol$atom } 
 /* expr: Expr = l: expr `+` r: expr => Bin("+", $l, $r)  */
 case 4 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Bin("+", dol$l, dol$r) } 
 /* expr: Expr = l: expr `*` r: expr => Bin("*", $l, $r)  */
 case 5 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Bin("*", dol$l, dol$r) } 
 /* expr: Expr = l: expr `-` r: expr => Bin("-", $l, $r)  */
 case 6 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Bin("-", dol$l, dol$r) } 
 /* expr: Expr = l: expr `/` r: expr => Bin("/", $l, $r)  */
 case 7 => 
  { case List(dol$l: Expr, _, dol$r: Expr) =>  Bin("/", dol$l, dol$r) } 
 /* expr: Expr = `(` expr `)` => Bra($expr)  */
 case 8 => 
  { case List(_, dol$expr: Expr, _) =>  Bra(dol$expr) } 
 /* atom: Expr = prim => Atom($prim)  */
 case 9 => 
  { case List(dol$prim: String) =>  Atom(dol$prim) } 
 /* atom: Expr = STRING => Quoted($STRING)  */
 case 10 => 
  { case List(dol$STRING: String) =>  Quoted(dol$STRING) } 
 /* prim: String = HEX => reHexify($HEX)  */
 case 11 => 
  { case List(dol$HEX: String) =>  reHexify(dol$HEX) } 
 /* prim: String = DEC { $DEC }  */
 case 12 => 
  { case List(dol$DEC: String) =>  dol$DEC } 
 /* prim: String = ID { $ID }  */
 case 13 => 
  { case List(dol$ID: String) =>  dol$ID } 
 /* prim: String = REAL { $REAL }  */
 case 14 => 
  { case List(dol$REAL: String) =>  dol$REAL } 
 /* prim: String = ID { $ID }  */
 case 15 => 
  { case List(dol$ID: String) =>  dol$ID } 
 /* S_1: Option[Unit] =  { None }  */
 case 16 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = NL { Some(()) }  */
 case 17 => 
  { case List(_) =>  Some(()) } 
 /* S_2_L: List[String] = oneLine { List($oneLine) }  */
 case 18 => 
  { case List(dol$oneLine: String) =>  List(dol$oneLine) } 
 /* S_2_L: List[String] = S_2_L NL oneLine { $oneLine :: $S_2_L }  */
 case 19 => 
  { case List(dol$S_2_L: List[String @unchecked], _, dol$oneLine: String) => 
        dol$oneLine :: dol$S_2_L
  }
 /* S_2: List[String] =  { Nil }  */
 case 20 => 
  { case List() =>  Nil } 
 /* S_2: List[String] = S_2_L { $S_2_L.reverse }  */
 case 21 => 
  { case List(dol$S_2_L: List[String @unchecked]) =>  dol$S_2_L.reverse } 

 }

}
