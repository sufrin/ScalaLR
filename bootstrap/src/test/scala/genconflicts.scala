import org.sufrin.scalalr.{Notation, Translation}

object shiftreduce extends App {

  val source =
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
      |""".stripMargin
  import Notation.{Lexical, Syntax}
  import Syntax.Parser
  import org.sufrin.utility._
  val scanner = Lexical.Scanner(SourceTextCursor(source))
  val notation = Parser(scanner).parseNotation()
  val translation = Translation(notation)
  translation.makeFiles()
}


object reducereduce extends App {

  val source =
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
      |""".stripMargin
  import Notation.{Lexical, Syntax}
  import Syntax.Parser
  import org.sufrin.utility._
  val scanner = Lexical.Scanner(SourceTextCursor(source))
  val notation = Parser(scanner).parseNotation()
  val translation = Translation(notation)
  translation.makeFiles()
}
