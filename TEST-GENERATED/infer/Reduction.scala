

package infer.Infer
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* expr = ID { $ID }  */
 case 1 => 
  { case List(dol$ID: String) =>  dol$ID } 
 /* expr = this: ID ID { () }  */
 case 2 => 
  { case List(dol$this: String, dol$ID: String) =>  () } 

 }

}
