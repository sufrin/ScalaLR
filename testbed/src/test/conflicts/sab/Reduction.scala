
package SAB
object Reduction {

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // S = A
 case 1 => 
  { case List(_) => None }
 // S = B
 case 2 => 
  { case List(_) => None }
 // A = a
 case 3 => 
  { case List(_) => None }
 // B = a
 case 4 => 
  { case List(_) => None }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""S = A""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""S = B""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""A = a""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""B = a""", 4, trees$trees ) }
 }

}
