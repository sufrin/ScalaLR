package org.sufrin.scalalr.bootstrap

/**
 * Run this app to generate the "testbed/src/main/scala/small" files
 */

object generatesmall extends App {

  val source =
    """%notation  Small
      |%package   small.Small
      |%path      "testbed/src/test/scala/small"
      |%type      lr
      |%dialect   "dialect: bootstrap syntax; hand parsed"
      |%scalalr   "scalalr: scalalr.bootstrap"
      |
      |
      |%include {
      |   import org.sufrin.utility.{SourceTextCursor}
      |   import org.sufrin.scalalr.SourceLocation
      |
      |
      |    def Scanner(chars: SourceTextCursor): Scanner = new Scanner(chars)
      |
      |
      |    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      |      def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
      |
      |      @inline def hasChar: Boolean = chars.hasCurrent
      |      @inline def theChar: Char = chars.current
      |      @inline def nextChar(): Unit = chars.next()
      |      @inline def afterNextChar(t: Token): Token = {
      |        nextChar()
      |        t
      |      }
      |
      |      def hasNext: Boolean = chars.hasCurrent
      |
      |      def next(): Token = if (hasNext)
      |      {
      |          chars.current match {
      |            case ';' => afterNextChar(`;`)
      |
      |            case c if c.isLetter =>
      |              val prefix = chars.takeWhile(_.isLetterOrDigit)
      |              ID((prefix).mkString(""))
      |            case c if c.isWhitespace =>
      |               while (hasChar && theChar.isWhitespace) nextChar()
      |               if (hasChar) next() else $end
      |            case other =>
      |               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
      |          }
      |      } else $end
      |    }
      |
      |
      |}
      |
      |%token ID(String) ";" LEXICALERROR(String)
      |
      |%rules
      |%include {
      | // after rules
      |}
      |
      |ids:(List[String]) = idList { $idList.reverse };
      |idList:(List[String]) = ID { List($ID) } | idList ";" ID { $ID :: $idList };
      |
      |""".stripMargin
  import Syntax.{Lexical, Parser}
  import Parser.Parser
  import org.sufrin.utility._
  val scanner = Lexical.Scanner(SourceTextCursor(source))
  val notation = Parser(scanner).parseNotation()
  val translation = Generator(notation)
  translation.makeFiles()
}




