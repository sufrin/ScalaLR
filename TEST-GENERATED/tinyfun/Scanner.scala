
package tinyfun
object Scanner{


           import org.sufrin.scalalr.SourceLocation
           import org.sufrin.utility.SourceTextCursor

            def Scanner(chars: SourceTextCursor): Scanner = new Scanner(chars)


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
              def next(): Token = if (hasChar) {
                  chars.current match {
                    case '\n'     =>
                         chars.current = ' '            // the subsequent next() skips this space without accounting
                         NL                             // NL once
                    case 0004     => $end               // invariantly
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
              } else $end
            }
        
trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
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
case object `#` extends Token { val value = (); val symbol = 13 }
case object HIGH extends Token { val value = (); val symbol = 14 }
case object `=` extends Token { val value = (); val symbol = 15 }
case object `+` extends Token { val value = (); val symbol = 16 }
case object `-` extends Token { val value = (); val symbol = 17 }
case object HIGH extends Token { val value = (); val symbol = 14 }
case object `*` extends Token { val value = (); val symbol = 18 }
case object `/` extends Token { val value = (); val symbol = 19 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](26)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "NUM"
          arr(4) = "ID"
          arr(5) = "`(`"
          arr(6) = "`)`"
          arr(7) = "`[`"
          arr(8) = "`]`"
          arr(9) = "`,`"
          arr(10) = "LEXICALERROR"
          arr(11) = "NL"
          arr(12) = "QUIT"
          arr(13) = "`#`"
          arr(14) = "HIGH"
          arr(15) = "`=`"
          arr(16) = "`+`"
          arr(17) = "`-`"
          arr(18) = "`*`"
          arr(19) = "`/`"
          arr(20) = "$accept"
          arr(21) = "loop"
          arr(22) = "command"
          arr(23) = "expr"
          arr(24) = "exprs"
          arr(25) = "number"
         } // locally
         ArrayMap(arr)
     }
}
