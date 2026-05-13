

package small.Small
object Reduction {



 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* ids: List[String@58.16]@58.9 = idList{ $idList.reverse } */
 case 1 => 
  { case List(dol$idList: List[String @unchecked]) =>  dol$idList.reverse } 
 /* idList: List[String@59.19]@59.12 = ID{ List($ID) } */
 case 2 => 
  { case List(dol$ID: String) =>  List(dol$ID) } 
 /* idList: List[String@59.19]@59.12 = idList `;` ID{ $ID :: $idList } */
 case 3 => 
  { case List(dol$idList: List[String @unchecked], _, dol$ID: String) => 
        dol$ID :: dol$idList
  }

 }

}
