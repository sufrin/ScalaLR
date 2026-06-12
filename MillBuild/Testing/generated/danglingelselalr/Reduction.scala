

package 
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* expr: Unit = ID { $ID }  */
 case 1 => 
  { case List(_) =>  dol$ID } 
 /* expr: Unit = expr `+` ID { $expr }  */
 case 2 => 
  { case List(_, _, _) =>  dol$expr } 
 /* expr: Unit = IF expr THEN expr {  ()  }  */
 case 3 => 
  { case List(_, _, _, _) =>  () } 
 /* expr: Unit = IF expr THEN expr ELSE expr {  ()  }  */
 case 4 => 
  { case List(_, _, _, _, _, _) =>  () } 

 }

}
