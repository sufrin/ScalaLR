
//> using scala 2.13
//> using jar ./Runtime/scalalrruntime.jar
import  lists.Lists._
import org.sufrin.scalalr.stage2.TestRunner
import org.sufrin.scalalr._
import org.sufrin.utility.SourceTextCursor
object listsLists extends TestRunner [Scanner.Token] {
  val    components:  LRParserComponents = Components
  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor("""1,2,3,4,5
6,7,8,9,0

"""))
}
