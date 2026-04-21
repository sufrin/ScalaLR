
package tinyfun
object Scanner {

   import org.sufrin.scalalr.SourceLocation
   import org.sufrin.utility.SourceTextCursor

    object Scanner {
      def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
    }

    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
      @inline def hasChar: Boolean = chars.hasCurrent
      @inline def theChar: Char = chars.current
      @inline def nextChar(): Unit = chars.next()
      @inline def afterNextChar(t: Token): Token = {
        nextChar()
        t
      }

      def hasNext: Boolean = chars.hasCurrent
      def next(): Token = {
          chars.current match {
            case '\n'     =>
                 chars.current = ' '            // the subsequent next() skips this space without accounting
                 NL                             // NL once
            case '\u0004' => $end               // invariantly
            case '.'      => $end               // invariantly

            case '(' => afterNextChar(`(`)
            case ')' => afterNextChar(`)`)
            case '[' => afterNextChar(`[`)
            case ']' => afterNextChar(`]`)
            case '/' => afterNextChar(`/`)
            case '-' => afterNextChar(`-`)
            case '+' => afterNextChar(`+`)
            case '*' => afterNextChar(`*`)
            case ',' => afterNextChar(`,`)
            case '=' => afterNextChar(`=`)
            case c if c.isLetter =>
              val prefix = chars.takeWhile(_.isLetterOrDigit)
              prefix.mkString("") match {
                case "quit" => QUIT
                case other  => ID(other)
              }
            case c if c.isDigit =>
              val prefix = chars.takeWhile(c=>c.isDigit||c=='.')
              NUM((prefix).mkString(""))
             case c if c.isWhitespace =>
               while (hasChar && theChar.isWhitespace) nextChar()
               if (hasChar) next() else $end
             case other =>
               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")

          }
      }
    }

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case class NUM(value: String) extends Token { val symbol = 3 }
case class ID(value: String) extends Token { val symbol = 4 }
case object `(` extends Token { val value = (); val symbol = 5 }
case object `)` extends Token { val value = (); val symbol = 6 }
case object `[` extends Token { val value = (); val symbol = 7 }
case object `]` extends Token { val value = (); val symbol = 8 }
case object `,` extends Token { val value = (); val symbol = 9 }
case class LEXICALERROR(value: String) extends Token { val symbol = 10 }
case object NL extends Token { val value = (); val symbol = 11 }
case object QUIT extends Token { val value = (); val symbol = 12 }
case object `=` extends Token { val value = (); val symbol = 13 }
case object `+` extends Token { val value = (); val symbol = 14 }
case object `-` extends Token { val value = (); val symbol = 15 }
case object `*` extends Token { val value = (); val symbol = 16 }
case object `/` extends Token { val value = (); val symbol = 17 }
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
// GLOSSARY OF SYMBOL NAMES
val symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](
0->"$end", 1->"error", 2->"UNDEF"
, 0 -> "$end"
, 1 -> "error"
, 3 -> "NUM"
, 4 -> "ID"
, 5 -> "("
, 6 -> ")"
, 7 -> "["
, 8 -> "]"
, 9 -> ","
, 10 -> "LEXICALERROR"
, 11 -> "NL"
, 12 -> "QUIT"
, 13 -> "="
, 14 -> "+"
, 15 -> "-"
, 16 -> "*"
, 17 -> "/"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 18 -> "$accept" 
, 19 -> "loop" 
, 20 -> "command" 
, 21 -> "expr" 
, 22 -> "exprs" 
)

}
