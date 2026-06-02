
package tinyfun
object Scanner{


  import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
  import org.sufrin.utility.SourceTextCursor

  /**
     `Scanner(chars)` builds a lexical scanner using a `ScannerBuilder`. The implementations of
     the `mkXXX` functions bridge the fixed lexical categories of the builder, and the `%token`
     constructors declared here. The `withSymbolTokens(symbolToken)` clause initialises the
     internal symbol and token tables of the resulting builder.
  */
  object Scanner {
    def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
       def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(s"$openQuote${body.mkString}$closeQuote")
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
       locally {
         TinyFun.prompt = chars.prompt
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
case class STRING(value: Seq[Char @unchecked]) extends Token { val symbol = 13 }
case object NL extends Token { val value = (); val symbol = 14 }
case object `QUIT` extends Token { val value = (); val symbol = 15 }
case object `=` extends Token { val value = (); val symbol = 16 }
case object `+` extends Token { val value = (); val symbol = 17 }
case object `-` extends Token { val value = (); val symbol = 18 }
case object `*` extends Token { val value = (); val symbol = 19 }
case object `/` extends Token { val value = (); val symbol = 20 }
case object `^` extends Token { val value = (); val symbol = 21 }
case object UNARY extends Token { val value = (); val symbol = 22 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](32)
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
          arr(13) = "STRING"
          arr(14) = "NL"
          arr(15) = "`QUIT`"
          arr(16) = "`=`"
          arr(17) = "`+`"
          arr(18) = "`-`"
          arr(19) = "`*`"
          arr(20) = "`/`"
          arr(21) = "`^`"
          arr(22) = "UNARY"
          arr(23) = "$accept"
          arr(24) = "loop"
          arr(25) = "command"
          arr(26) = "expr"
          arr(27) = "simple"
          arr(28) = "expressions"
          arr(29) = "exprs"
          arr(30) = "NUM"
          arr(31) = "NAME"
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
