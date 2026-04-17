package org.sufrin.scalalr

// A little nonsense notation that demonstrates how to implement a read-eval-print loop
// This grammar is in the bootstrap notation
// BS April '26

/**
 * Run this app to generate the "testbed/src/main/scala/tinyfun" files
 */

object generatetinyfun extends App {

  val source =
    """
      |
      |
      |%notation TinyFun
      |%package  tinyfun
      |%path     "testbed/src/main/scala/tinyfun"
      |
      |%include {
      |   import org.sufrin.scalalr.SourceLocation
      |   import org.sufrin.utility.SourceTextCursor
      |
      |    object Scanner {
      |      def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
      |    }
      |
      |    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      |      def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
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
      |            case '\n'     =>
      |                 chars.current = ' '            // the subsequent next() skips this space without accounting
      |                 NL                             // NL once
      |            case '\u0004' => $end               // invariantly
      |            case '.'      => $end               // invariantly
      |
      |            case '(' => afterNextChar(`(`)
      |            case ')' => afterNextChar(`)`)
      |            case '[' => afterNextChar(`[`)
      |            case ']' => afterNextChar(`]`)
      |            case '/' => afterNextChar(`/`)
      |            case '-' => afterNextChar(`-`)
      |            case '+' => afterNextChar(`+`)
      |            case '*' => afterNextChar(`*`)
      |            case ',' => afterNextChar(`,`)
      |            case '=' => afterNextChar(`=`)
      |            case c if c.isLetter =>
      |              val prefix = chars.takeWhile(_.isLetterOrDigit)
      |              prefix.mkString("") match {
      |                case "quit" => QUIT
      |                case other  => ID(other)
      |              }
      |            case c if c.isDigit =>
      |              val prefix = chars.takeWhile(c=>c.isDigit||c=='.')
      |              NUM((prefix).mkString(""))
      |             case c if c.isWhitespace =>
      |               while (hasChar && theChar.isWhitespace) nextChar()
      |               if (hasChar) next() else $end
      |             case other =>
      |               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
      |
      |          }
      |      }
      |    }
      |}
      |
      |%token NUM: String ID: String  `(` `)` `[` `]` `,` LEXICALERROR: (String) NL QUIT
      |%right `=`
      |%left `+` `-`
      |%left `*` `/`
      |
      |
      |
      |%rules
      |
      |%include {
      | import org.sufrin.scalalr.SourceLocation
      | import tinyfun.TinyFun._
      |}
      |
      |/*
      |        This is typical of a grammar needed to run as a read-expr/run loop as it is parsed.
      |
      |        The `command` production is a "hook" that is parsed by parsing an expr, then
      |        reduced when the NL appears to its right (as the lookahead symbol).
      |        It is at the reduction that the parsed $expr is run.
      |
      |*/
      |
      |loop: Unit =
      |          %empty          { () }
      |        | loop command NL { () }
      |        ;
      |
      |command: Unit = expr { run(List($expr)) } | QUIT { System.exit(0) };
      |
      |
      |expr: Expr =
      |          ID                  { Id($ID, $START) }
      |        | NUM                 { Num($NUM.toDouble, $START) }
      |        | ID `=` expr         { Assign($ID, $expr, $START) }
      |        | l:expr `*` r:expr   { Binop("*", $l, $r, $START) }
      |        | l:expr `+` r:expr   { Binop("+", $l, $r, $START) }
      |        | l:expr `/` r:expr   { Binop("/", $l, $r, $START) }
      |        | l:expr `-` r:expr   { Binop("-", $l, $r, $START) }
      |        | "(" expr ")"        { $expr }
      |        | ID `(` exprs `)`    { Apply($ID, $exprs, $START) }
      |        ;
      |
      |exprs: (List[Expr]) =
      |            expr            { List($expr) }
      |        |   exprs `,` expr  { $expr::$exprs }
      |;
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




