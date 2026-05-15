
package scalalr.stage2
object Scanner{

  // Substantive lexical scanner is elsewhere

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object `?` extends Token { val value = (); val symbol = 36 }
case object `*` extends Token { val value = (); val symbol = 37 }
case object `+` extends Token { val value = (); val symbol = 38 }
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
, 36 -> "?"
, 37 -> "*"
, 38 -> "+"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 39 -> "$accept" 
, 40 -> "Notation" 
, 41 -> "Prefix" 
, 42 -> "INCLUDE" 
, 43 -> "OPTNL" 
, 44 -> "TypedTerminals" 
, 45 -> "TypedTerminal" 
, 46 -> "Rules" 
, 47 -> "Rule" 
, 48 -> "OptBar" 
, 49 -> "LHS" 
, 50 -> "RHS" 
, 51 -> "Production" 
, 52 -> "NamedFields" 
, 53 -> "NamedField" 
, 54 -> "FIELD" 
, 55 -> "REPEAT" 
, 56 -> "Action" 
, 57 -> "Precedence" 
, 58 -> "Type" 
, 59 -> "Types" 
)

}
