

package small.Small
object Reduction {



 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* top: Unit@213.9 = ids { println($ids) }  */
 case 1 => 
  { case List(dol$ids: List[String @unchecked]) =>  println(dol$ids) } 
 /* ids: List[String@214.16]@214.9 = REP1 */
 case 2 => 
  { case List(dol$REP1: List[String @unchecked]) => 
None }
 /* REP1LIST: List[String@206.16]@214.22 = ID { List($ID) }  */
 case 3 => 
  { case List(dol$ID: String) =>  List(dol$ID) } 
 /* REP1LIST: List[String@206.16]@214.22 = REP1LIST `;` ID { $ID :: $REP1LIST }  */
 case 4 => 
  { case List(dol$REP1LIST: List[String @unchecked], _, dol$ID: String) => 
        dol$ID :: dol$REP1LIST
  }
 /* REP1: List[String@206.16]@214.22 = REP1LIST { $REP1LIST.reverse }  */
 case 5 => 
  { case List(dol$REP1LIST: List[String @unchecked]) =>  dol$REP1LIST.reverse } 

 }

}
