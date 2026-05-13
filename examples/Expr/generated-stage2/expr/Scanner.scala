
package expr.Expr
object Scanner{


   import org.sufrin.utility.SourceTextCursor
   import org.sufrin.scalalr.SourceLocation

    def Scanner (chars: SourceTextCursor): Scanner = new Scanner(chars)

    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
      @inline def hasChar: Boolean = chars.hasCurrent
      @inline def theChar: Char = chars.current
      @inline def nextChar(): Unit = chars.next()
      @inline def afterNextChar(t: Token): Token = {
        nextChar()
        t
      }

      def hasNext: Boolean = hasChar
      def next(): Token = if (hasChar) {
          chars.current match {
            case '(' => afterNextChar(`(`)
            case ')' => afterNextChar(`)`)
            case '[' => afterNextChar(`[`)
            case ']' => afterNextChar(`]`)
            case '+' => afterNextChar(`+`)
            case '*' => afterNextChar(`*`)
            case ';' => afterNextChar(`;`)
            case c if c.isLetter =>
              val prefix = chars.takeWhile(_.isLetterOrDigit)
              ID((prefix).mkString(""))
             case c if c.isWhitespace =>
               while (hasChar && theChar.isWhitespace) nextChar()
               if (hasChar) next() else $end
             case other =>
               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")

          }
      } else $end
    }

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class ID(value: String) extends Token { val symbol = 3 }
case object `(` extends Token { val value = (); val symbol = 4 }
case object `)` extends Token { val value = (); val symbol = 5 }
case object `[` extends Token { val value = (); val symbol = 6 }
case object `]` extends Token { val value = (); val symbol = 7 }
case object `;` extends Token { val value = (); val symbol = 8 }
case class LEXICALERROR(value: String) extends Token { val symbol = 9 }
case object `+` extends Token { val value = (); val symbol = 10 }
case object `*` extends Token { val value = (); val symbol = 11 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](15)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "ID"
          arr(4) = "`(`"
          arr(5) = "`)`"
          arr(6) = "`[`"
          arr(7) = "`]`"
          arr(8) = "`;`"
          arr(9) = "LEXICALERROR"
          arr(10) = "`+`"
          arr(11) = "`*`"
          arr(12) = "$accept"
          arr(13) = "exprs"
          arr(14) = "expr"
         } // locally
         ArrayMap(arr)
     }
}
