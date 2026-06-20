
//> using scala 2.13
//> using jar ROOT/ScalaLR/bin/scalalr.jar
//> using jar ROOT/Runtime/scalalrlibrary.jar
package lists.Lists
import org.sufrin.scalalr.stage2.Test.Runner
import org.sufrin.scalalr._
import org.sufrin.utility.SourceTextCursor
object run extends Runner [Scanner.Token] {
  val    components:  LRParserComponents = Components
  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor("""0,1,2,3,4,
5,6,7,8,9



10,11,12,13,14
20,21,22,23,24
"""))
}
