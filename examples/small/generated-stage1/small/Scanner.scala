
package small.Small
object Scanner{

   import org.sufrin.utility.{SourceTextCursor}
   import org.sufrin.scalalr.SourceLocation


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

      def next(): Token = if (hasNext)
      {
          chars.current match {
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
case class ID(value: String) extends Token { val symbol = 3 }
case object `;` extends Token { val value = (); val symbol = 4 }
case class LEXICALERROR(value: String) extends Token { val symbol = 5 }
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
// GLOSSARY OF SYMBOL NAMES
val symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](
0->"$end", 1->"error", 2->"UNDEF"
, 0 -> "$end"
, 1 -> "error"
, 3 -> "ID"
, 4 -> ";"
, 5 -> "LEXICALERROR"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 6 -> "$accept" 
, 7 -> "ids" 
, 8 -> "idList" 
)

}
