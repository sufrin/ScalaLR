

package scalalr.err4
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* ListInt: List[Int] = list: S_1 => List($list.length/2)  */
 case 1 => 
  { case List(dol$list: List[Int @unchecked]) =>  List(dol$list.length/2) } 
 /* S_1_E: List[Int] = S_1_E INT { $INT :: $S_1_E }  */
 case 2 => 
  { case List(dol$S_1_E: List[Int @unchecked], dol$INT: Int) =>  dol$INT :: dol$S_1_E } 
 /* S_1_E: List[Int] = INT { List($INT) }  */
 case 3 => 
  { case List(dol$INT: Int) =>  List(dol$INT) } 
 /* S_1: List[Int] = S_1_E { $S_1_E.reverse }  */
 case 4 => 
  { case List(dol$S_1_E: List[Int @unchecked]) =>  dol$S_1_E.reverse } 

 }

}
