package org.sufrin.scalalr.bootstrap

/**
 * Run this app to generate the appropriate %package files in testbed/src
 * Make sure the artefact: scalalr.jar is linked from testbed/scala/
 * From testbed/scala/: scala-cli runexpr.scala expr
 */
object generatexpr extends App {
    import Syntax.{Lexical, Parser}

    val source =
      """%notation  Expr
        |%package   expr.Expr
        |%path      "testbed/src/test/scala/expr"
        |
        |%include {
        |   import org.sufrin.utility.SourceTextCursor
        |   import org.sufrin.scalalr.SourceLocation
        |
        |    def Scanner (chars: SourceTextCursor): Scanner = new Scanner(chars)
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
        |      def hasNext: Boolean = hasChar
        |      def next(): Token = if (hasChar) {
        |          chars.current match {
        |            case '(' => afterNextChar(`(`)
        |            case ')' => afterNextChar(`)`)
        |            case '[' => afterNextChar(`[`)
        |            case ']' => afterNextChar(`]`)
        |            case '+' => afterNextChar(`+`)
        |            case '*' => afterNextChar(`*`)
        |            case ';' => afterNextChar(`;`)
        |            case c if c.isLetter =>
        |              val prefix = chars.takeWhile(_.isLetterOrDigit)
        |              ID((prefix).mkString(""))
        |             case c if c.isWhitespace =>
        |               while (hasChar && theChar.isWhitespace) nextChar()
        |               if (hasChar) next() else $end
        |             case other =>
        |               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
        |
        |          }
        |      } else $end
        |    }
        |
        |
        |
        |}
        |
        |%token ID(String) `(` `)` `[` `]` `;` LEXICALERROR(String)
        |%left `+`
        |%left `*`
        |
        |%rules
        |
        |%include {
        | import org.sufrin.scalalr.SourceLocation
        | trait Expr
        | case class Id(s: String, loc: SourceLocation) extends Expr
        | case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr
        | case class Bra(expr: Expr, loc: SourceLocation)extends Expr
        |}
        |
        |exprs: (List[Expr]) = expr            { List($expr) }
        |                  |   exprs `;` expr  { $expr::$exprs }
        |                  |   error           { List(Id("RECOVER", $START)) }
        |                  ;
        |
        |expr: Expr = ID                  { Id($ID, $START) }
        |           | l:expr `*` r:expr   { Binop("*", $l, $r, $START) }
        |           | l:expr `+` r:expr   { Binop("+", $l, $r, $START) }
        |           | "(" expr ")"        { Bra($expr, $START) }
        |           | `[` expr `]`        { $expr }
        |           ;
        |""".stripMargin
    import org.sufrin.utility._
    val scanner = Lexical.Scanner(SourceTextCursor(source))
    val notation = Parser(scanner).parseNotation()
    //notation.prettyPrint()
    val translation = Generator(notation)
    translation.makeFiles()

  }

