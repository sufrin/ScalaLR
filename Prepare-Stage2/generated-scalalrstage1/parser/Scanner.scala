
package scalalr.stage2
object Scanner{

  // Substantive lexical scanner is elsewhere

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case class ID(value: org.sufrin.scalalr.stage2.AST.Name) extends Token { val symbol = 3 }
case class NUM(value: String) extends Token { val symbol = 4 }
case class CODE(value: String) extends Token { val symbol = 5 }
case class COMMENT(value: String) extends Token { val symbol = 6 }
case class LEXICALERROR(value: String) extends Token { val symbol = 7 }
case object `[` extends Token { val value = (); val symbol = 8 }
case object `]` extends Token { val value = (); val symbol = 9 }
case object `;` extends Token { val value = (); val symbol = 10 }
case object `=` extends Token { val value = (); val symbol = 11 }
case object `|` extends Token { val value = (); val symbol = 12 }
case object `:` extends Token { val value = (); val symbol = 13 }
case object `{` extends Token { val value = (); val symbol = 14 }
case object `}` extends Token { val value = (); val symbol = 15 }
case object `(` extends Token { val value = (); val symbol = 16 }
case object `)` extends Token { val value = (); val symbol = 17 }
case object `,` extends Token { val value = (); val symbol = 18 }
case object SEPARATOR extends Token { val value = (); val symbol = 19 }
case object `%path` extends Token { val value = (); val symbol = 20 }
case object `%type` extends Token { val value = (); val symbol = 21 }
case object `%empty` extends Token { val value = (); val symbol = 22 }
case object `%notation` extends Token { val value = (); val symbol = 23 }
case object `%package` extends Token { val value = (); val symbol = 24 }
case object `%token` extends Token { val value = (); val symbol = 25 }
case object `%left` extends Token { val value = (); val symbol = 26 }
case object `%right` extends Token { val value = (); val symbol = 27 }
case object `%non` extends Token { val value = (); val symbol = 28 }
case object `%rules` extends Token { val value = (); val symbol = 29 }
case object `%include` extends Token { val value = (); val symbol = 30 }
case object `%prec` extends Token { val value = (); val symbol = 31 }
case object `%tables` extends Token { val value = (); val symbol = 32 }
case object `%dialect` extends Token { val value = (); val symbol = 33 }
case object `%scalalr` extends Token { val value = (); val symbol = 34 }
case object `%signature` extends Token { val value = (); val symbol = 35 }
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
// GLOSSARY OF SYMBOL NAMES
val symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](
0->"$end", 1->"error", 2->"UNDEF"
, 0 -> "$end"
, 1 -> "error"
, 3 -> "ID"
, 4 -> "NUM"
, 5 -> "CODE"
, 6 -> "COMMENT"
, 7 -> "LEXICALERROR"
, 8 -> "["
, 9 -> "]"
, 10 -> ";"
, 11 -> "="
, 12 -> "|"
, 13 -> ":"
, 14 -> "{"
, 15 -> "}"
, 16 -> "("
, 17 -> ")"
, 18 -> ","
, 19 -> "SEPARATOR"
, 20 -> "%path"
, 21 -> "%type"
, 22 -> "%empty"
, 23 -> "%notation"
, 24 -> "%package"
, 25 -> "%token"
, 26 -> "%left"
, 27 -> "%right"
, 28 -> "%non"
, 29 -> "%rules"
, 30 -> "%include"
, 31 -> "%prec"
, 32 -> "%tables"
, 33 -> "%dialect"
, 34 -> "%scalalr"
, 35 -> "%signature"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 36 -> "$accept" 
, 37 -> "Notation" 
, 38 -> "Prefix" 
, 39 -> "INCLUDE" 
, 40 -> "OPTNL" 
, 41 -> "TypedTerminals" 
, 42 -> "TypedTerminal" 
, 43 -> "Rules" 
, 44 -> "Rule" 
, 45 -> "OptBar" 
, 46 -> "LHS" 
, 47 -> "RHS" 
, 48 -> "Production" 
, 49 -> "NamedFields" 
, 50 -> "NamedField" 
, 51 -> "Action" 
, 52 -> "Precedence" 
, 53 -> "Type" 
, 54 -> "Types" 
)

}
