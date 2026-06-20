
//> using scala 2.13
//> using jar ROOT/ScalaLR/bin/scalalr.jar
//> using jar ROOT/Runtime/scalalrlibrary.jar
package shortcut.Lists
import org.sufrin.scalalr.stage2.Test.Runner
import org.sufrin.scalalr._
import org.sufrin.utility.SourceTextCursor
object run extends Runner [Scanner.Token] {
  val    components:  LRParserComponents = Components
  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor.console)
}
