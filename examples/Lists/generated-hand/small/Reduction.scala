

package small.Small
object Reduction {



 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* top = ids { $ids }  */
 case 1 => 
  { case List(dol$ids: List[String @unchecked]) =>  dol$ids } 
 /* ids: List[String] = idList { $idList.reverse }  */
 case 2 => 
  { case List(dol$idList: List[String @unchecked]) =>  dol$idList.reverse } 
 /* idList: List[String] = ID { List($ID) }  */
 case 3 => 
  { case List(dol$ID: String) =>  List(dol$ID) } 
 /* idList: List[String] = idList `;` ID { $ID :: $idList }  */
 case 4 => 
  { case List(dol$idList: List[String @unchecked], _, dol$ID: String) => 
        dol$ID :: dol$idList
  }

 }

}
