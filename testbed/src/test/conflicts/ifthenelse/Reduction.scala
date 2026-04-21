
package IfThenElse
object Reduction {

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // expr = ID
 case 1 => 
  { case List(_) => None }
 // expr = expr "+" ID
 case 2 => 
  { case List(_, _, _) => None }
 // expr = IF expr THEN expr
 case 3 => 
  { case List(_, _, _, _) => None }
 // expr = IF expr THEN expr ELSE expr
 case 4 => 
  { case List(_, _, _, _, _, _) => None }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""expr = ID""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""expr = expr "+" ID""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""expr = IF expr THEN expr""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""expr = IF expr THEN expr ELSE expr""", 4, trees$trees ) }
 }

}
