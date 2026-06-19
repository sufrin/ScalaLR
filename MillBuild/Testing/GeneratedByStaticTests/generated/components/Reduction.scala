

package 
object Reduction {


 this is the first include 

 this is the second include 
def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* S: Unit = A { $A }  */
 case 1 => 
  { case List(dol$A: Unit) =>  dol$A } 
 /* S: Unit = B { $B }  */
 case 2 => 
  { case List(dol$B: Unit) =>  dol$B } 
 /* A: Unit = a { $a }  */
 case 3 => 
  { case List(_) =>  dol$a } 
 /* B: Unit = a { $a }  */
 case 4 => 
  { case List(_) =>  dol$a } 

 }

}
