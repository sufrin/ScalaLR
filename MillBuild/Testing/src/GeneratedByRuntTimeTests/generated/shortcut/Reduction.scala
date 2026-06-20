

package shortcut.Lists
object Reduction {



 import shortcut.Lists._
 import org.sufrin.scalalr.Shortcut._



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* loop: Unit = S_1 S_2 => ()  */
 case 1 => 
  { case List(dol$S_1: Option[Unit @unchecked], dol$S_2: List[Shortcut @unchecked]) => 
        ()
  }
 /* aLine: Shortcut = theList: aList {  println($theList); Continue  }  */
 case 2 => 
  { case List(dol$theList: List[Long @unchecked]) =>  println(dol$theList); Continue } 
 /* aLine: Shortcut = `.` {  Accept("Finished")  }  */
 case 3 => 
  { case List(_) =>  Accept("Finished") } 
 /* aList: List[Long] = theList: S_3 => $theList  */
 case 4 => 
  { case List(dol$theList: List[Long @unchecked]) =>  dol$theList } 
 /* S_1: Option[Unit] =  { None }  */
 case 5 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = NL { Some(()) }  */
 case 6 => 
  { case List(_) =>  Some(()) } 
 /* S_2_E: List[Shortcut] = S_2_E NL aLine { $aLine :: $S_2_E }  */
 case 7 => 
  { case List(dol$S_2_E: List[Shortcut @unchecked], _, dol$aLine: Shortcut) => 
        dol$aLine :: dol$S_2_E
  }
 /* S_2_E: List[Shortcut] = aLine { List($aLine) }  */
 case 8 => 
  { case List(dol$aLine: Shortcut) =>  List(dol$aLine) } 
 /* S_2_E: List[Shortcut] = S_2_E NL { $S_2_E }  */
 case 9 => 
  { case List(dol$S_2_E: List[Shortcut @unchecked], _) =>  dol$S_2_E } 
 /* S_2: List[Shortcut] = S_2_E { $S_2_E.reverse }  */
 case 10 => 
  { case List(dol$S_2_E: List[Shortcut @unchecked]) =>  dol$S_2_E.reverse } 
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
