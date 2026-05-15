

package SAB
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* S = A */
 case 1 => 
  { case List(_) => 
None }
 /* S = B */
 case 2 => 
  { case List(_) => 
None }
 /* A = a */
 case 3 => 
  { case List(_) => 
None }
 /* B = a */
 case 4 => 
  { case List(_) => 
None }

 }

}
