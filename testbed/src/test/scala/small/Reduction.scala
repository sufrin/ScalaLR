
package small.Small
object Reduction {

 // after rules

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // ids: List[String] = idList { $idList.reverse }
 case 1 => 
  { case List(dol$idList: List[String @unchecked]) => 
         dol$idList.reverse 
  }
 // idList: List[String] = ID { List($ID) }
 case 2 => 
  { case List(dol$ID: String) =>   List(dol$ID)  } 
 // idList: List[String] = idList ";" idList { $ID :: $idList }
 case 3 => 
  { case List(dol$idList: List[String @unchecked], _, dol$idList: List[String @unchecked]) => 
         dol$ID :: dol$idList 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""ids: List[String] = idList { $idList.reverse }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""idList: List[String] = ID { List($ID) }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""idList: List[String] = idList ";" idList { $ID :: $idList }""", 3, trees$trees ) }
 }

}
