

package 
object Reduction {



def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* expr: List[Any] = ID {  List("ID")  }  */
 case 1 => 
  { case List(_) =>  List("ID") } 
 /* expr: List[Any] = expr `+` ID {  List($expr, "+", "ID")  }  */
 case 2 => 
  { case List(dol$expr: List[Any @unchecked], _, _) =>  List(dol$expr, "+", "ID") } 
 /* expr: List[Any] = IF g: expr THEN l: expr {  List("IF", $g, $l, Nil)  }  */
 case 3 => 
  { case List(_, dol$g: List[Any @unchecked], _, dol$l: List[Any @unchecked]) => 
        List("IF", dol$g, dol$l, Nil)
  }
 /* expr: List[Any] = IF g: expr THEN l: expr ELSE r: expr {  List("IF", $g, $l, $r)  }  */
 case 4 => 
  { case List(_, dol$g: List[Any @unchecked], _, dol$l: List[Any @unchecked], _, dol$r: List[Any @unchecked]) => 
        List("IF", dol$g, dol$l, dol$r)
  }

 }

}
