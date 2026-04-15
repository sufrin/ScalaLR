package org.sufrin.scalalr

/**
 * Run this app to generate testbed.scala.small.generated.Small files
 * Make sure the artefact: scalalr.jar is linked from testbed/scala/
 * From testbed/scala/: scala-cli runsmall.scala small
 */

object generatesmall extends App {

  val source =
    """%notation  Small
      |%package   testbed.scala.small.generated.Small
      |%scanner   Scanner
      |%type      lr
      |%extending Token
      |
      |%include {
      |   import org.sufrin.utility.{SourceTextCursor}
      |   import org.sufrin.scalalr.SourceLocation
      |
      |    object Scanner {
      |      def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
      |    }
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
      |      def next(): Token = {
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
      |      }
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
  import Notation.{Lexical, Syntax}
  import Syntax.Parser
  import org.sufrin.utility._
  val scanner = Lexical.Scanner(SourceTextCursor(source))
  val notation = Parser(scanner).parseNotation()
  val translation = Translation(notation)
  translation.makeFiles()
}




