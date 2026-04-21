import java.io.PrintStream
import java.nio.file.Path
import scala.collection.immutable.ListMap

/**
 * Run this `App` to test reports of notation definitions with conflicts.
 */
object genconflicts extends App {
  import org.sufrin.scalalr.{Notation, Translation}

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
  import Notation.{Lexical, Syntax}
  import Syntax.Parser
  import org.sufrin.utility._
  for { (report, source) <- sources } {
    val file = new PrintStream(Path.of(s"testbed/src/test/conflicts/$report/$report.log").toFile)
    Console.withOut(file) {
      val scanner = Lexical.Scanner(SourceTextCursor(source.stripMargin))
      val notation = Parser(scanner).parseNotation()
      val translation = Translation(notation)
      translation.makeFiles()
    }
  }
}

