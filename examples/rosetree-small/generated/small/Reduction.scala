

package small.Small
object Reduction {



 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* ids: List[String] = ids: S_1 { $ids }  */
 case 1 => 
  { case List(dol$ids: List[String @unchecked]) =>  dol$ids } 
 /* S_1_L: List[String] = ID { List($ID) }  */
 case 2 => 
  { case List(dol$ID: String) =>  List(dol$ID) } 
 /* S_1_L: List[String] = S_1_L `;` ID { $ID :: $S_1_L }  */
 case 3 => 
  { case List(dol$S_1_L: List[String @unchecked], _, dol$ID: String) => 
        dol$ID :: dol$S_1_L
  }
 /* S_1: List[String] = S_1_L { $S_1_L.reverse }  */
 case 4 => 
  { case List(dol$S_1_L: List[String @unchecked]) =>  dol$S_1_L.reverse } 

 }

}
