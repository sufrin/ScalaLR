
package expr.Expression
object Scanner{


  import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
  import org.sufrin.utility.SourceTextCursor

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
       def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
       def mkHex(source: Seq[Char]):   Token = HEX(source.mkString)
       def mkDec(source: Seq[Char]):   Token = DEC(source.mkString)
       def mkReal(source: Seq[Char]):  Token = REAL(source.mkString)
       def mkID(source: Seq[Char]):    Token = ID(source.mkString)
       def mkERROR(source: Seq[Char]): Token = ERROR(source.mkString)
       val ENDSTREAM: Token = $end
       val NEWLINE:   Option[Token] = Some(NL)
       def flush(): Unit = {
           while (chars.hasCurrent && chars.current != '\n') chars.next()
           print(chars.prompt); System.out.flush()
       }
  } withSymbolTokens(symbolToken)

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class HEX(value: String) extends Token { val symbol = 3 }
case class DEC(value: String) extends Token { val symbol = 4 }
case class REAL(value: String) extends Token { val symbol = 5 }
case class ID(value: String) extends Token { val symbol = 6 }
case class STRING(value: String) extends Token { val symbol = 7 }
case class ERROR(value: String) extends Token { val symbol = 8 }
case object NL extends Token { val value = (); val symbol = 9 }
case object `+` extends Token { val value = (); val symbol = 10 }
case object `-` extends Token { val value = (); val symbol = 11 }
case object `*` extends Token { val value = (); val symbol = 12 }
case object `/` extends Token { val value = (); val symbol = 13 }
case object `(` extends Token { val value = (); val symbol = 14 }
case object `)` extends Token { val value = (); val symbol = 15 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](25)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "HEX"
          arr(4) = "DEC"
          arr(5) = "REAL"
          arr(6) = "ID"
          arr(7) = "STRING"
          arr(8) = "ERROR"
          arr(9) = "NL"
          arr(10) = "`+`"
          arr(11) = "`-`"
          arr(12) = "`*`"
          arr(13) = "`/`"
          arr(14) = "`(`"
          arr(15) = "`)`"
          arr(16) = "$accept"
          arr(17) = "loop"
          arr(18) = "oneLine"
          arr(19) = "expr"
          arr(20) = "atom"
          arr(21) = "prim"
          arr(22) = "S_1"
          arr(23) = "S_2_L"
          arr(24) = "S_2"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    "+" -> `+`,
    "-" -> `-`,
    "*" -> `*`,
    "/" -> `/`,
    "(" -> `(`,
    ")" -> `)`,
    ""->$end)
}
