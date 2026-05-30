
package tinyfun
object Scanner{


   import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
   import org.sufrin.utility.SourceTextCursor

      object Scanner {
        def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
           def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = mkERROR(body)
           def mkHex(source: Seq[Char]):   Token = HEX(source.mkString)
           def mkDec(source: Seq[Char]):   Token = DEC(source.mkString)
           def mkReal(source: Seq[Char]):  Token = REAL(source.mkString)
           def mkID(source: Seq[Char]):    Token = ID(source.mkString)
           def mkERROR(source: Seq[Char]): Token = SCANERROR(source.mkString)
           val ENDSTREAM: Token = $end
           val NEWLINE:   Option[Token] = Some(NL)
           def flush(): Unit = {
               while (chars.hasCurrent && chars.current != '\n') chars.next()
               print(chars.prompt); System.out.flush()
           }
        } withSymbolTokens(symbolToken)
      }

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class HEX(value: Seq[Char @unchecked]) extends Token { val symbol = 3 }
case class DEC(value: Seq[Char @unchecked]) extends Token { val symbol = 4 }
case class REAL(value: Seq[Char @unchecked]) extends Token { val symbol = 5 }
case class ID(value: Seq[Char @unchecked]) extends Token { val symbol = 6 }
case object `(` extends Token { val value = (); val symbol = 7 }
case object `)` extends Token { val value = (); val symbol = 8 }
case object `[` extends Token { val value = (); val symbol = 9 }
case object `]` extends Token { val value = (); val symbol = 10 }
case object `,` extends Token { val value = (); val symbol = 11 }
case class SCANERROR(value: Seq[Char @unchecked]) extends Token { val symbol = 12 }
case object NL extends Token { val value = (); val symbol = 13 }
case object `QUIT` extends Token { val value = (); val symbol = 14 }
case object `=` extends Token { val value = (); val symbol = 15 }
case object `+` extends Token { val value = (); val symbol = 16 }
case object `-` extends Token { val value = (); val symbol = 17 }
case object `*` extends Token { val value = (); val symbol = 18 }
case object `/` extends Token { val value = (); val symbol = 19 }
case object `^` extends Token { val value = (); val symbol = 20 }
case object UNARY extends Token { val value = (); val symbol = 21 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](31)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "HEX"
          arr(4) = "DEC"
          arr(5) = "REAL"
          arr(6) = "ID"
          arr(7) = "`(`"
          arr(8) = "`)`"
          arr(9) = "`[`"
          arr(10) = "`]`"
          arr(11) = "`,`"
          arr(12) = "SCANERROR"
          arr(13) = "NL"
          arr(14) = "`QUIT`"
          arr(15) = "`=`"
          arr(16) = "`+`"
          arr(17) = "`-`"
          arr(18) = "`*`"
          arr(19) = "`/`"
          arr(20) = "`^`"
          arr(21) = "UNARY"
          arr(22) = "$accept"
          arr(23) = "loop"
          arr(24) = "command"
          arr(25) = "expr"
          arr(26) = "simple"
          arr(27) = "expressions"
          arr(28) = "exprs"
          arr(29) = "NUM"
          arr(30) = "NAME"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    "(" -> `(`,
    ")" -> `)`,
    "[" -> `[`,
    "]" -> `]`,
    "," -> `,`,
    "QUIT" -> `QUIT`,
    "=" -> `=`,
    "+" -> `+`,
    "-" -> `-`,
    "*" -> `*`,
    "/" -> `/`,
    "^" -> `^`,
    ""->$end)
}
