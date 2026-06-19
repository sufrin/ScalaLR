
package 
object Scanner{


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
val ENDSTREAM: Token = $end
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case object a extends Token { val value = (); val symbol = 3 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](8)
         locally {
          arr(0) = """$end"""
          arr(1) = """error"""
          arr(2) = """UNDEF"""
          arr(3) = """a"""
          arr(4) = """$accept"""
          arr(5) = """S"""
          arr(6) = """A"""
          arr(7) = """B"""
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
lazy val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    ""->ENDSTREAM)
lazy val TokenMap: collection.immutable.Map[String, Token] = symbolToken
}
