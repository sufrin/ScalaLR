

package scalalr.err5
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* ListInt: List[Int] = list: S_1 => List($list.length-2)  */
 case 1 => 
  { case List(dol$list: List[Int @unchecked]) =>  List(dol$list.length-2) } 
 /* ListPig: List[Pig] = list: S_2 => List($list.length-2)  */
 case 2 => 
  { case List(dol$list: List[Pig @unchecked]) =>  List(dol$list.length-2) } 
 /* S_1_E: List[Int] = S_1_E INT { $INT :: $S_1_E }  */
 case 3 => 
  { case List(dol$S_1_E: List[Int @unchecked], dol$INT: Int) =>  dol$INT :: dol$S_1_E } 
 /* S_1_E: List[Int] = INT { List($INT) }  */
 case 4 => 
  { case List(dol$INT: Int) =>  List(dol$INT) } 
 /* S_1: List[Int] = S_1_E { $S_1_E.reverse }  */
 case 5 => 
  { case List(dol$S_1_E: List[Int @unchecked]) =>  dol$S_1_E.reverse } 
 /* S_2_E: List[Pig] = S_2_E PIG { $PIG :: $S_2_E }  */
 case 6 => 
  { case List(dol$S_2_E: List[Pig @unchecked], dol$PIG: Pig) =>  dol$PIG :: dol$S_2_E } 
 /* S_2_E: List[Pig] = PIG { List($PIG) }  */
 case 7 => 
  { case List(dol$PIG: Pig) =>  List(dol$PIG) } 
 /* S_2: List[Pig] = S_2_E { $S_2_E.reverse }  */
 case 8 => 
  { case List(dol$S_2_E: List[Pig @unchecked]) =>  dol$S_2_E.reverse } 

 }

}
