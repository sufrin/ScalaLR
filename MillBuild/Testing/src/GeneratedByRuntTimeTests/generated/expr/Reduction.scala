

package expr.Expression
object Reduction {





       import org.sufrin.utility.PrettyPrint._
       import expr.AST._
       trait Void
       case object Void extends Void
      
def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit = S_1 S_2 => ()  */
 case 1 => 
  { case List(dol$S_1: Option[Unit @unchecked], dol$S_2: List[Void @unchecked]) =>  () } 
 /* oneLine: Void = expr {  $expr.prettyPrint(); print(">> "); Void  }  */
 case 2 => 
  { case List(dol$expr: Expr) =>  dol$expr.prettyPrint(); print(">> "); Void } 
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
  { case List( _ , dol$expr: Expr,  _ ) =>  Bra(dol$expr) } 
 /* atom: Expr = ID => Id($ID)  */
 case 9 => 
  { case List(dol$ID: String) =>  Id(dol$ID) } 
 /* atom: Expr = LONG => Num($LONG.toDouble)  */
 case 10 => 
  { case List(dol$LONG: Long) =>  Num(dol$LONG.toDouble) } 
 /* atom: Expr = DOUBLE => Num($DOUBLE)  */
 case 11 => 
  { case List(dol$DOUBLE: Double) =>  Num(dol$DOUBLE) } 
 /* atom: Expr = QUOTE => Quoted($QUOTE)  */
 case 12 => 
  { case List(dol$QUOTE: String) =>  Quoted(dol$QUOTE) } 
 /* S_1: Option[Unit] =  { None }  */
 case 13 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = NL { Some(()) }  */
 case 14 => 
  { case List(_) =>  Some(()) } 
 /* S_2_E: List[Void] = S_2_E NL oneLine { $oneLine :: $S_2_E }  */
 case 15 => 
  { case List(dol$S_2_E: List[Void @unchecked], _, dol$oneLine: Void) => 
        dol$oneLine :: dol$S_2_E
  }
 /* S_2_E: List[Void] = oneLine { List($oneLine) }  */
 case 16 => 
  { case List(dol$oneLine: Void) =>  List(dol$oneLine) } 
 /* S_2_E: List[Void] = S_2_E NL { $S_2_E }  */
 case 17 => 
  { case List(dol$S_2_E: List[Void @unchecked], _) =>  dol$S_2_E } 
 /* S_2: List[Void] = S_2_E { $S_2_E.reverse }  */
 case 18 => 
  { case List(dol$S_2_E: List[Void @unchecked]) =>  dol$S_2_E.reverse } 

 }

}
