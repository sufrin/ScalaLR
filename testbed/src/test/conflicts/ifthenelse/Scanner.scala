
package IfThenElse
object Scanner {

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object IF extends Token { val value = (); val symbol = 3 }
case object THEN extends Token { val value = (); val symbol = 4 }
case object ELSE extends Token { val value = (); val symbol = 5 }
case object ID extends Token { val value = (); val symbol = 6 }
case object `+` extends Token { val value = (); val symbol = 7 }
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
// GLOSSARY OF SYMBOL NAMES
val symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](
0->"$end", 1->"error", 2->"UNDEF"
, 0 -> "$end"
, 1 -> "error"
, 3 -> "IF"
, 4 -> "THEN"
, 5 -> "ELSE"
, 6 -> "ID"
, 7 -> "+"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 8 -> "$accept" 
, 9 -> "expr" 
, 10 -> "expr" 
, 11 -> "expr" 
)

}
