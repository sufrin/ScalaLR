
package shortcut.Lists
object Scanner{


  import org.sufrin.scalalr._
  import org.sufrin.utility._
  lazy val generated: this.type = this
  def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
       override val LONG      = generated.LONG
       override val NEWLINE   = Some(generated.NL)
       override def TOKENMAP  = generated.TokenMap
       override val ENDSTREAM = generated.ENDSTREAM
  }

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
val ENDSTREAM: Token = $end
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class LONG(value: Long) extends Token { val symbol = 3 }
case object NL extends Token { val value = (); val symbol = 4 }
case object `.` extends Token { val value = (); val symbol = 5 }
case object `,` extends Token { val value = (); val symbol = 6 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](16)
         locally {
          arr(0) = """$end"""
          arr(1) = """error"""
          arr(2) = """UNDEF"""
          arr(3) = """LONG"""
          arr(4) = """NL"""
          arr(5) = """`.`"""
          arr(6) = """`,`"""
          arr(7) = """$accept"""
          arr(8) = """loop"""
          arr(9) = """aLine"""
          arr(10) = """aList"""
          arr(11) = """S_1"""
          arr(12) = """S_2_E"""
          arr(13) = """S_2"""
          arr(14) = """S_3_L"""
          arr(15) = """S_3"""
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
lazy val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    """.""" -> `.`,
    """,""" -> `,`,
    ""->ENDSTREAM)
lazy val TokenMap: collection.immutable.Map[String, Token] = symbolToken
}
