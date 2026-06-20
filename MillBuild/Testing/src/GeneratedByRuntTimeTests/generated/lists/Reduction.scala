

package lists.Lists
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* lists: List[List[Long @unchecked]] = S_1 theLists: S_2 => $theLists  */
 case 1 => 
  { case List(dol$S_1: Option[Unit @unchecked], dol$theLists: List[List[Long @unchecked] @unchecked]) => 
        dol$theLists
  }
 /* aList: List[Long] = theList: S_3 => $theList  */
 case 2 => 
  { case List(dol$theList: List[Long @unchecked]) =>  dol$theList } 
 /* S_1: Option[Unit] =  { None }  */
 case 3 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = NL { Some(()) }  */
 case 4 => 
  { case List(_) =>  Some(()) } 
 /* S_2_E: List[List[Long @unchecked]] = S_2_E NL aList { $aList :: $S_2_E }  */
 case 5 => 
  { case List(dol$S_2_E: List[List[Long @unchecked] @unchecked], _, dol$aList: List[Long @unchecked]) => 
        dol$aList :: dol$S_2_E
  }
 /* S_2_E: List[List[Long @unchecked]] = aList { List($aList) }  */
 case 6 => 
  { case List(dol$aList: List[Long @unchecked]) =>  List(dol$aList) } 
 /* S_2_E: List[List[Long @unchecked]] = S_2_E NL { $S_2_E }  */
 case 7 => 
  { case List(dol$S_2_E: List[List[Long @unchecked] @unchecked], _) =>  dol$S_2_E } 
 /* S_2: List[List[Long @unchecked]] = S_2_E { $S_2_E.reverse }  */
 case 8 => 
  { case List(dol$S_2_E: List[List[Long @unchecked] @unchecked]) =>  dol$S_2_E.reverse } 
 /* S_3_E: List[Long] = S_3_E `,` LONG { $LONG :: $S_3_E }  */
 case 9 => 
  { case List(dol$S_3_E: List[Long @unchecked], _, dol$LONG: Long) => 
        dol$LONG :: dol$S_3_E
  }
 /* S_3_E: List[Long] = LONG { List($LONG) }  */
 case 10 => 
  { case List(dol$LONG: Long) =>  List(dol$LONG) } 
 /* S_3_E: List[Long] = S_3_E `,` { $S_3_E }  */
 case 11 => 
  { case List(dol$S_3_E: List[Long @unchecked], _) =>  dol$S_3_E } 
 /* S_3: List[Long] = S_3_E { $S_3_E.reverse }  */
 case 12 => 
  { case List(dol$S_3_E: List[Long @unchecked]) =>  dol$S_3_E.reverse } 

 }

}
