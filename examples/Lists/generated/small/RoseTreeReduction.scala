

package small.Small
object RoseTreeReduction {


case class ROSETREE(nonTerminal: String, rule: Int, trees:List[Any])
def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, rule: Int): PartialFunction[List[Any], Any] = rule match {
 case 1 => 
  { case trees$trees => ROSETREE("""top""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => ROSETREE("""ids""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => ROSETREE("""S_1_L""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => ROSETREE("""S_1_L""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => ROSETREE("""S_1""", 5, trees$trees ) }
 }

}
