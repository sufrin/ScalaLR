

package small.Small
object Reduction {



 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* top: Unit = ids { println($ids) }  */
 case 1 => 
  { case List(dol$ids: List[String @unchecked]) =>  println(dol$ids) } 
 /* ids: List[String] = S$1 { $S$1 }  */
 case 2 => 
  { case List(dol$S$1: List[String @unchecked]) =>  dol$Sdol$1 } 
 /* S$1LIST: List[String] = ID { List($ID) }  */
 case 3 => 
  { case List(dol$ID: String) =>  List(dol$ID) } 
 /* S$1LIST: List[String] = S$1LIST `;` ID { $ID :: $S$1LIST }  */
 case 4 => 
  { case List(dol$S$1LIST: List[String @unchecked], _, dol$ID: String) => 
        dol$ID :: dol$Sdol$1LIST
  }
 /* S$1: List[String] = S$1LIST { $S$1LIST.reverse }  */
 case 5 => 
  { case List(dol$S$1LIST: List[String @unchecked]) =>  dol$Sdol$1LIST.reverse } 

 }

}
