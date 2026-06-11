



import  expr.Expression._
import org.sufrin.scalalr.stage2.TestRunner
import org.sufrin.scalalr._
import org.sufrin.utility.SourceTextCursor
object exprExpression extends TestRunner [Scanner.Token] {
  val    components:  LRParserComponents = Components
  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor(java.nio.file.Path.of("/dev/tty")))
}
