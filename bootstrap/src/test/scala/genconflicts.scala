import scala.collection.immutable.ListMap

/**
 * Run this `App` to test reports of notation definitions with conflicts.
 */
object genconflicts extends App {
  import org.sufrin.scalalr.bootstrap.{Syntax, Generator}

  val sources = ListMap(
    "sab" ->
    """%notation  SAB
      |%path      "testbed/src/test/conflicts/sab"
      |%type      lr
      |
      |%token a
      |
      |%rules
      | S = A | B;
      | A = a;
      | B = a;
      |
      |""",
    "ifthenelse" ->
    """%notation  IfThenElse
      |%package   IfThenElse
      |%path      "testbed/src/test/conflicts/ifthenelse"
      |%type      lr
      |
      |%token IF THEN ELSE ID '+'
      |
      |%rules
      |
      |expr = ID | expr '+' ID;
      |expr = IF expr THEN expr;
      |expr = IF expr THEN expr ELSE expr;
      |
      |"""
  )
  import Syntax.Lexical
  import Syntax.Parser
  import org.sufrin.utility._
  for { (report, source) <- sources } {
    {
      val scanner = Lexical.Scanner(SourceTextCursor(source.stripMargin))
      val notation = Parser(scanner).parseNotation()
      val translation = Generator(notation)
      translation.makeFiles()
    }
  }
}

