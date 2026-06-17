
//> using scala 2.13
//> using jar ROOT/ScalaLR/bin/scalalr.jar
//> using jar ROOT/Runtime/scalalrlibrary.jar
package interactive.Lists
package runner
import org.sufrin.scalalr.stage2.TestRunner
import org.sufrin.scalalr._
import org.sufrin.utility.SourceTextCursor
object runner extends TestRunner [Scanner.Token] {
  val    components:  LRParserComponents = Components
  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor(java.nio.file.Path.of("/dev/tty")))
}
