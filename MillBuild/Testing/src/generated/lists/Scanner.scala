
package lists.Lists
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
case class INT(value: Int) extends Token { val symbol = 3 }
case class HEX(value: String) extends Token { val symbol = 4 }
case class DEC(value: String) extends Token { val symbol = 5 }
case class REAL(value: String) extends Token { val symbol = 6 }
case class ID(value: String) extends Token { val symbol = 7 }
case class STRING(value: String) extends Token { val symbol = 8 }
case class ERROR(value: String) extends Token { val symbol = 9 }
case object NL extends Token { val value = (); val symbol = 10 }
case object `,` extends Token { val value = (); val symbol = 11 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](18)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "INT"
          arr(4) = "HEX"
          arr(5) = "DEC"
          arr(6) = "REAL"
          arr(7) = "ID"
          arr(8) = "STRING"
          arr(9) = "ERROR"
          arr(10) = "NL"
          arr(11) = "`,`"
          arr(12) = "$accept"
          arr(13) = "loop"
          arr(14) = "command"
          arr(15) = "ListInt"
          arr(16) = "S_1_E"
          arr(17) = "S_1"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    "," -> `,`,
    ""->$end)
}
