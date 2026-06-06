
package scalalr.stage2
object Scanner{

  // Substantive lexical scanner is elsewhere

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object `=>` extends Token { val value = (); val symbol = 42 }
case object `$` extends Token { val value = (); val symbol = 43 }
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
case object `*` extends Token { val value = (); val symbol = 19 }
case object `?` extends Token { val value = (); val symbol = 20 }
case object SEPARATOR extends Token { val value = (); val symbol = 21 }
case object `%path` extends Token { val value = (); val symbol = 22 }
case object `%type` extends Token { val value = (); val symbol = 23 }
case object `%empty` extends Token { val value = (); val symbol = 24 }
case object `%notation` extends Token { val value = (); val symbol = 25 }
case object `%package` extends Token { val value = (); val symbol = 26 }
case object `%token` extends Token { val value = (); val symbol = 27 }
case object `%left` extends Token { val value = (); val symbol = 28 }
case object `%right` extends Token { val value = (); val symbol = 29 }
case object `%non` extends Token { val value = (); val symbol = 30 }
case object `%rules` extends Token { val value = (); val symbol = 31 }
case object `%include` extends Token { val value = (); val symbol = 32 }
case object `%prec` extends Token { val value = (); val symbol = 33 }
case object `%tables` extends Token { val value = (); val symbol = 34 }
case object `%dialect` extends Token { val value = (); val symbol = 35 }
case object `%scalalr` extends Token { val value = (); val symbol = 36 }
case object `%signature` extends Token { val value = (); val symbol = 37 }
case object `::` extends Token { val value = (); val symbol = 38 }
case object `+` extends Token { val value = (); val symbol = 39 }
case object `-` extends Token { val value = (); val symbol = 40 }
case object `.` extends Token { val value = (); val symbol = 41 }
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
, 19 -> "*"
, 20 -> "?"
, 21 -> "SEPARATOR"
, 22 -> "%path"
, 23 -> "%type"
, 24 -> "%empty"
, 25 -> "%notation"
, 26 -> "%package"
, 27 -> "%token"
, 28 -> "%left"
, 29 -> "%right"
, 30 -> "%non"
, 31 -> "%rules"
, 32 -> "%include"
, 33 -> "%prec"
, 34 -> "%tables"
, 35 -> "%dialect"
, 36 -> "%scalalr"
, 37 -> "%signature"
, 38 -> "::"
, 39 -> "+"
, 40 -> "-"
, 41 -> "."
, 42 -> "=>"
, 43 -> "$"
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 44 -> "$accept" 
, 45 -> "Notation" 
, 46 -> "Prefix" 
, 47 -> "INCLUDE" 
, 48 -> "OPTNL" 
, 49 -> "TypedTerminals" 
, 50 -> "TypedTerminal" 
, 51 -> "Rules" 
, 52 -> "Rule" 
, 53 -> "OptBar" 
, 54 -> "LHS" 
, 55 -> "RHS" 
, 56 -> "Production" 
, 57 -> "NamedFields" 
, 58 -> "NamedField" 
, 59 -> "FIELD" 
, 60 -> "REPEAT" 
, 61 -> "Precedence" 
, 62 -> "Type" 
, 63 -> "Types" 
, 64 -> "Action" 
, 65 -> "Scala" 
, 66 -> "Scalas" 
, 67 -> "ScalaPlus" 
, 68 -> "ScalaAtom" 
)

}
