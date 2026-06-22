

package interactive.Lists
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import interactive.Lists._
 trait NONE
 case object NONE extends NONE



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: NONE = S_1 S_2 => NONE  */
 case 1 => 
  { case List(dol$S_1: Option[Unit @unchecked], dol$S_2: List[NONE @unchecked]) =>  NONE } 
 /* aLine: NONE = theList: aList {  println($theList); NONE  }  */
 case 2 => 
  { case List(dol$theList: List[Long @unchecked]) =>  println(dol$theList); NONE } 
 /* aLine: NONE = `.` {  println("Finished"); System.exit(0); NONE  }  */
 case 3 => 
  { case List(_) =>  println("Finished"); System.exit(0); NONE } 
 /* aList: List[Long] = theList: S_3 => $theList  */
 case 4 => 
  { case List(dol$theList: List[Long @unchecked]) =>  dol$theList } 
 /* S_1: Option[Unit] =  { None }  */
 case 5 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = NL { Some(()) }  */
 case 6 => 
  { case List(_) =>  Some(()) } 
 /* S_2_E: List[NONE] = S_2_E NL aLine { $aLine :: $S_2_E }  */
 case 7 => 
  { case List(dol$S_2_E: List[NONE @unchecked], _, dol$aLine: NONE) => 
        dol$aLine :: dol$S_2_E
  }
 /* S_2_E: List[NONE] = aLine { List($aLine) }  */
 case 8 => 
  { case List(dol$aLine: NONE) =>  List(dol$aLine) } 
 /* S_2_E: List[NONE] = S_2_E NL { $S_2_E }  */
 case 9 => 
  { case List(dol$S_2_E: List[NONE @unchecked], _) =>  dol$S_2_E } 
 /* S_2: List[NONE] = S_2_E { $S_2_E.reverse }  */
 case 10 => 
  { case List(dol$S_2_E: List[NONE @unchecked]) =>  dol$S_2_E.reverse } 
 /* S_3_L: List[Long] = LONG { List($LONG) }  */
 case 11 => 
  { case List(dol$LONG: Long) =>  List(dol$LONG) } 
 /* S_3_L: List[Long] = S_3_L `,` LONG { $LONG :: $S_3_L }  */
 case 12 => 
  { case List(dol$S_3_L: List[Long @unchecked], _, dol$LONG: Long) => 
        dol$LONG :: dol$S_3_L
  }
 /* S_3: List[Long] = S_3_L { $S_3_L.reverse }  */
 case 13 => 
  { case List(dol$S_3_L: List[Long @unchecked]) =>  dol$S_3_L.reverse } 

 }

}
