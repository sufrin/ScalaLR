
package scalalr.stage2
object Scanner{


  // Substantive lexical scanner is elsewhere

trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case object $end extends Token { val value = (); val symbol = 0 }
case object error extends Token { val value = (); val symbol = 1 }
case object UNDEF extends Token { val value = (); val symbol = 2 }
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
case object `?` extends Token { val value = (); val symbol = 36 }
case object `*` extends Token { val value = (); val symbol = 37 }
case object `+` extends Token { val value = (); val symbol = 38 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](65)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "ID"
          arr(4) = "NUM"
          arr(5) = "CODE"
          arr(6) = "COMMENT"
          arr(7) = "LEXICALERROR"
          arr(8) = "`[`"
          arr(9) = "`]`"
          arr(10) = "`;`"
          arr(11) = "`=`"
          arr(12) = "`|`"
          arr(13) = "`:`"
          arr(14) = "`{`"
          arr(15) = "`}`"
          arr(16) = "`(`"
          arr(17) = "`)`"
          arr(18) = "`,`"
          arr(19) = "SEPARATOR"
          arr(20) = "`%path`"
          arr(21) = "`%type`"
          arr(22) = "`%empty`"
          arr(23) = "`%notation`"
          arr(24) = "`%package`"
          arr(25) = "`%token`"
          arr(26) = "`%left`"
          arr(27) = "`%right`"
          arr(28) = "`%non`"
          arr(29) = "`%rules`"
          arr(30) = "`%include`"
          arr(31) = "`%prec`"
          arr(32) = "`%tables`"
          arr(33) = "`%dialect`"
          arr(34) = "`%scalalr`"
          arr(35) = "`%signature`"
          arr(36) = "`?`"
          arr(37) = "`*`"
          arr(38) = "`+`"
          arr(39) = "$accept"
          arr(40) = "Notation"
          arr(41) = "Prefix"
          arr(42) = "INCLUDE"
          arr(43) = "TypedTerminals"
          arr(44) = "TypedTerminal"
          arr(45) = "Rule"
          arr(46) = "OptBar"
          arr(47) = "LHS"
          arr(48) = "RHS"
          arr(49) = "Production"
          arr(50) = "NamedFields"
          arr(51) = "NamedField"
          arr(52) = "FIELD"
          arr(53) = "REPEAT"
          arr(54) = "Action"
          arr(55) = "Precedence"
          arr(56) = "Type"
          arr(57) = "Types"
          arr(58) = "S_1_L"
          arr(59) = "S_1"
          arr(60) = "S_2"
          arr(61) = "S_3_L"
          arr(62) = "S_3"
          arr(63) = "S_4_L"
          arr(64) = "S_4"
         } // locally
         ArrayMap(arr)
     }


// MAP QUOTED SYMBOL NAMES TO TOKENS 
val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(
    "[" -> `[`,
    "]" -> `]`,
    ";" -> `;`,
    "=" -> `=`,
    "|" -> `|`,
    ":" -> `:`,
    "{" -> `{`,
    "}" -> `}`,
    "(" -> `(`,
    ")" -> `)`,
    "," -> `,`,
    "%path" -> `%path`,
    "%type" -> `%type`,
    "%empty" -> `%empty`,
    "%notation" -> `%notation`,
    "%package" -> `%package`,
    "%token" -> `%token`,
    "%left" -> `%left`,
    "%right" -> `%right`,
    "%non" -> `%non`,
    "%rules" -> `%rules`,
    "%include" -> `%include`,
    "%prec" -> `%prec`,
    "%tables" -> `%tables`,
    "%dialect" -> `%dialect`,
    "%scalalr" -> `%scalalr`,
    "%signature" -> `%signature`,
    "?" -> `?`,
    "*" -> `*`,
    "+" -> `+`,
    ""->$end)
}
