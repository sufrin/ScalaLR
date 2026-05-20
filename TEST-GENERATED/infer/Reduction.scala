

package infer.Infer
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* expr: NoType = ID { $ID }  */
 case 1 => 
  { case List(dol$ID: String) =>  dol$ID } 
 /* expr: NoType = this: ID { $this }  */
 case 2 => 
  { case List(dol$this: String) =>  dol$this } 
 /* expr: NoType = `(` ID `)` { $ID }  */
 case 3 => 
  { case List(_, dol$ID: String, _) =>  dol$ID } 
 /* expr: NoType = `(` ID that: pig `)` { () }  */
 case 4 => 
  { case List(_, dol$ID: String, _, _) =>  () } 
 /* pig: NoType = ID { $ID }  */
 case 5 => 
  { case List(dol$ID: String) =>  dol$ID } 

 }

}
