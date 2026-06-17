

package 
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* expr: String = ID { $ID }  */
 case 1 => 
  { case List(dol$ID: String) =>  dol$ID } 
 /* expr: String = l: expr `+` r: ID => $r  */
 case 2 => 
  { case List(dol$l: String, _, dol$r: String) =>  dol$r } 
 /* expr: String = expr `+` r: ID => $r  */
 case 3 => 
  { case List(dol$expr: String, _, dol$r: String) =>  dol$r } 
 /* expr: String = l: expr `+` ID => $ID  */
 case 4 => 
  { case List(dol$l: String, _, dol$ID: String) =>  dol$ID } 
 /* expr: String = expr `+` ID => $ID  */
 case 5 => 
  { case List( _ , _,  _ ) =>  dol$ID } 
 /* expr: String = IF guard: expr THEN expr => $guard  */
 case 6 => 
  { case List( _ , dol$guard: String,  _ , dol$expr: String) =>  dol$guard } 
 /* expr: String = IF guard: expr x: THEN expr ELSE expr => $guard  */
 case 7 => 
  { case List( _ , dol$guard: String, _,  _ ,  _ ,  _ ) =>  dol$guard } 

 }

}
