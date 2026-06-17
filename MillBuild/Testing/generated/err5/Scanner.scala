
package scalalr.err5
object Scanner{


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class INT(value: Int) extends Token { val symbol = 3 }
case class PIG(value: Pig) extends Token { val symbol = 4 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](12)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "INT"
          arr(4) = "PIG"
          arr(5) = "$accept"
          arr(6) = "ListInt"
          arr(7) = "ListPig"
          arr(8) = "S_1_E"
          arr(9) = "S_1"
          arr(10) = "S_2_E"
          arr(11) = "S_2"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    ""->$end)
}
