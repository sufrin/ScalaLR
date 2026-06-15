

package lists.Lists
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit =  {  ()  }  */
 case 1 => 
  { case List() =>  () } 
 /* loop: Unit = loop command NL {  ()  }  */
 case 2 => 
  { case List(_, _, _) =>  () } 
 /* command: Unit = ListInt => println($ListInt)  */
 case 3 => 
  { case List(dol$ListInt: List[Int @unchecked]) =>  println(dol$ListInt) } 
 /* ListInt: List[Int] = list: S_1 => List($list)  */
 case 4 => 
  { case List(dol$list: List[String @unchecked]) =>  List(dol$list) } 
 /* S_1_E: List[String] = S_1_E `,` DEC { $DEC :: $S_1_E }  */
 case 5 => 
  { case List(dol$S_1_E: List[String @unchecked], _, dol$DEC: String) => 
        dol$DEC :: dol$S_1_E
  }
 /* S_1_E: List[String] = DEC { List($DEC) }  */
 case 6 => 
  { case List(dol$DEC: String) =>  List(dol$DEC) } 
 /* S_1_E: List[String] = S_1_E `,` { $S_1_E }  */
 case 7 => 
  { case List(dol$S_1_E: List[String @unchecked], _) =>  dol$S_1_E } 
 /* S_1: List[String] = S_1_E { $S_1_E.reverse }  */
 case 8 => 
  { case List(dol$S_1_E: List[String @unchecked]) =>  dol$S_1_E.reverse } 

 }

}
