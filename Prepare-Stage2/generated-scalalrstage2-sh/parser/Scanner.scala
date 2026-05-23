
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
case object `%path` extends Token { val value = (); val symbol = 19 }
case object `%type` extends Token { val value = (); val symbol = 20 }
case object `%empty` extends Token { val value = (); val symbol = 21 }
case object `%notation` extends Token { val value = (); val symbol = 22 }
case object `%package` extends Token { val value = (); val symbol = 23 }
case object `%token` extends Token { val value = (); val symbol = 24 }
case object `%left` extends Token { val value = (); val symbol = 25 }
case object `%right` extends Token { val value = (); val symbol = 26 }
case object `%non` extends Token { val value = (); val symbol = 27 }
case object `%rules` extends Token { val value = (); val symbol = 28 }
case object `%include` extends Token { val value = (); val symbol = 29 }
case object `%prec` extends Token { val value = (); val symbol = 30 }
case object `%tables` extends Token { val value = (); val symbol = 31 }
case object `%dialect` extends Token { val value = (); val symbol = 32 }
case object `%scalalr` extends Token { val value = (); val symbol = 33 }
case object `%signature` extends Token { val value = (); val symbol = 34 }
case object `*` extends Token { val value = (); val symbol = 35 }
case object `?` extends Token { val value = (); val symbol = 36 }
case object `+` extends Token { val value = (); val symbol = 37 }
case object SEPARATOR extends Token { val value = (); val symbol = 38 }
case object HIGH extends Token { val value = (); val symbol = 39 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](66)
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
          arr(19) = "`%path`"
          arr(20) = "`%type`"
          arr(21) = "`%empty`"
          arr(22) = "`%notation`"
          arr(23) = "`%package`"
          arr(24) = "`%token`"
          arr(25) = "`%left`"
          arr(26) = "`%right`"
          arr(27) = "`%non`"
          arr(28) = "`%rules`"
          arr(29) = "`%include`"
          arr(30) = "`%prec`"
          arr(31) = "`%tables`"
          arr(32) = "`%dialect`"
          arr(33) = "`%scalalr`"
          arr(34) = "`%signature`"
          arr(35) = "`*`"
          arr(36) = "`?`"
          arr(37) = "`+`"
          arr(38) = "SEPARATOR"
          arr(39) = "HIGH"
          arr(40) = "$accept"
          arr(41) = "Notation"
          arr(42) = "Prefix"
          arr(43) = "INCLUDE"
          arr(44) = "OPTSEPARATOR"
          arr(45) = "TypedTerminals"
          arr(46) = "TypedTerminal"
          arr(47) = "Rules"
          arr(48) = "Rule"
          arr(49) = "LHS"
          arr(50) = "RHS"
          arr(51) = "Production"
          arr(52) = "Fields"
          arr(53) = "NamedField"
          arr(54) = "FIELD"
          arr(55) = "REPEAT"
          arr(56) = "Action"
          arr(57) = "Precedence"
          arr(58) = "Type"
          arr(59) = "Types"
          arr(60) = "S_1"
          arr(61) = "S_2_L"
          arr(62) = "S_2"
          arr(63) = "S_3"
          arr(64) = "S_4_L"
          arr(65) = "S_4"
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
    "*" -> `*`,
    "?" -> `?`,
    "+" -> `+`,
    ""->$end)
}
