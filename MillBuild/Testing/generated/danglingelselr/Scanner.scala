
package 
object Scanner{


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case object IF extends Token { val value = (); val symbol = 3 }
case object THEN extends Token { val value = (); val symbol = 4 }
case object ELSE extends Token { val value = (); val symbol = 5 }
case object ID extends Token { val value = (); val symbol = 6 }
case object `+` extends Token { val value = (); val symbol = 7 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](10)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "IF"
          arr(4) = "THEN"
          arr(5) = "ELSE"
          arr(6) = "ID"
          arr(7) = "`+`"
          arr(8) = "$accept"
          arr(9) = "expr"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    "+" -> `+`,
    ""->$end)
}
