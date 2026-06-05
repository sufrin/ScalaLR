
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
case object `.` extends Token { val value = (); val symbol = 19 }
case object `+` extends Token { val value = (); val symbol = 20 }
case object `*` extends Token { val value = (); val symbol = 21 }
case object `?` extends Token { val value = (); val symbol = 22 }
case object SEPARATOR extends Token { val value = (); val symbol = 23 }
case object `%path` extends Token { val value = (); val symbol = 24 }
case object `%type` extends Token { val value = (); val symbol = 25 }
case object `%empty` extends Token { val value = (); val symbol = 26 }
case object `%notation` extends Token { val value = (); val symbol = 27 }
case object `%package` extends Token { val value = (); val symbol = 28 }
case object `%token` extends Token { val value = (); val symbol = 29 }
case object `%left` extends Token { val value = (); val symbol = 30 }
case object `%right` extends Token { val value = (); val symbol = 31 }
case object `%non` extends Token { val value = (); val symbol = 32 }
case object `%rules` extends Token { val value = (); val symbol = 33 }
case object `%include` extends Token { val value = (); val symbol = 34 }
case object `%prec` extends Token { val value = (); val symbol = 35 }
case object `%tables` extends Token { val value = (); val symbol = 36 }
case object `%dialect` extends Token { val value = (); val symbol = 37 }
case object `%scalalr` extends Token { val value = (); val symbol = 38 }
case object `%signature` extends Token { val value = (); val symbol = 39 }
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
, 19 -> "."
, 20 -> "+"
, 21 -> "*"
, 22 -> "?"
, 23 -> "SEPARATOR"
, 24 -> "%path"
, 25 -> "%type"
, 26 -> "%empty"
, 27 -> "%notation"
, 28 -> "%package"
, 29 -> "%token"
, 30 -> "%left"
, 31 -> "%right"
, 32 -> "%non"
, 33 -> "%rules"
, 34 -> "%include"
, 35 -> "%prec"
, 36 -> "%tables"
, 37 -> "%dialect"
, 38 -> "%scalalr"
, 39 -> "%signature"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 40 -> "$accept" 
, 41 -> "Notation" 
, 42 -> "Prefix" 
, 43 -> "INCLUDE" 
, 44 -> "OPTNL" 
, 45 -> "TypedTerminals" 
, 46 -> "TypedTerminal" 
, 47 -> "Rules" 
, 48 -> "Rule" 
, 49 -> "OptBar" 
, 50 -> "LHS" 
, 51 -> "RHS" 
, 52 -> "Production" 
, 53 -> "NamedFields" 
, 54 -> "NamedField" 
, 55 -> "FIELD" 
, 56 -> "REPEAT" 
, 57 -> "Action" 
, 58 -> "Precedence" 
, 59 -> "Type" 
, 60 -> "Types" 
)

}
