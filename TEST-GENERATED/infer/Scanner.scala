
package infer.Infer
object Scanner{


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
case class ID(value: String) extends Token { val symbol = 3 }
case object RETURN extends Token { val value = (); val symbol = 4 }
case object FOOTLE extends Token { val value = (); val symbol = 5 }
case object `(` extends Token { val value = (); val symbol = 6 }
case object `)` extends Token { val value = (); val symbol = 7 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](11)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "ID"
          arr(4) = "RETURN"
          arr(5) = "FOOTLE"
          arr(6) = "`(`"
          arr(7) = "`)`"
          arr(8) = "$accept"
          arr(9) = "expr"
          arr(10) = "pig"
         } // locally
         ArrayMap(arr)
     }
}
