
package SAB
object Scanner {

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object a extends Token { val value = (); val symbol = 3 }
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
// GLOSSARY OF SYMBOL NAMES
val symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](
0->"$end", 1->"error", 2->"UNDEF"
, 0 -> "$end"
, 1 -> "error"
, 3 -> "a"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 4 -> "$accept" 
, 5 -> "S" 
, 6 -> "A" 
, 7 -> "B" 
)

}
