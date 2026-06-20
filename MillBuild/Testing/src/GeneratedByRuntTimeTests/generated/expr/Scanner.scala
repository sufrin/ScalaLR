
package expr.Expression
object Scanner{


        import org.sufrin.scalalr._
        import org.sufrin.utility._

        lazy val generated = this

        def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
             override val LONG       = generated.LONG
             override val DOUBLE     = generated.DOUBLE
             override val NEWLINE    = Some(generated.NL)
             override val IDENTIFIER = generated.ID
             override val STRING     = generated.QUOTE
             override def TOKENMAP   = TokenMap
        }

      
trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
val ENDSTREAM: Token = $end
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class LONG(value: Long) extends Token { val symbol = 3 }
case class DOUBLE(value: Double) extends Token { val symbol = 4 }
case class ID(value: String) extends Token { val symbol = 5 }
case class QUOTE(value: String) extends Token { val symbol = 6 }
case object NL extends Token { val value = (); val symbol = 7 }
case object `+` extends Token { val value = (); val symbol = 8 }
case object `-` extends Token { val value = (); val symbol = 9 }
case object `*` extends Token { val value = (); val symbol = 10 }
case object `/` extends Token { val value = (); val symbol = 11 }
case object `(` extends Token { val value = (); val symbol = 12 }
case object `)` extends Token { val value = (); val symbol = 13 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](22)
         locally {
          arr(0) = """$end"""
          arr(1) = """error"""
          arr(2) = """UNDEF"""
          arr(3) = """LONG"""
          arr(4) = """DOUBLE"""
          arr(5) = """ID"""
          arr(6) = """QUOTE"""
          arr(7) = """NL"""
          arr(8) = """`+`"""
          arr(9) = """`-`"""
          arr(10) = """`*`"""
          arr(11) = """`/`"""
          arr(12) = """`(`"""
          arr(13) = """`)`"""
          arr(14) = """$accept"""
          arr(15) = """loop"""
          arr(16) = """oneLine"""
          arr(17) = """expr"""
          arr(18) = """atom"""
          arr(19) = """S_1"""
          arr(20) = """S_2_E"""
          arr(21) = """S_2"""
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
lazy val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    """+""" -> `+`,
    """-""" -> `-`,
    """*""" -> `*`,
    """/""" -> `/`,
    """(""" -> `(`,
    """)""" -> `)`,
    ""->ENDSTREAM)
lazy val TokenMap: collection.immutable.Map[String, Token] = symbolToken
}
