
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
case class STRING(value: org.sufrin.scalalr.stage2.AST.Name) extends Token { val symbol = 6 }
case class COMMENT(value: String) extends Token { val symbol = 7 }
case class LEXICALERROR(value: String) extends Token { val symbol = 8 }
case object `[` extends Token { val value = (); val symbol = 9 }
case object `]` extends Token { val value = (); val symbol = 10 }
case object `;` extends Token { val value = (); val symbol = 11 }
case object `=` extends Token { val value = (); val symbol = 12 }
case object `|` extends Token { val value = (); val symbol = 13 }
case object `:` extends Token { val value = (); val symbol = 14 }
case object `{` extends Token { val value = (); val symbol = 15 }
case object `}` extends Token { val value = (); val symbol = 16 }
case object `(` extends Token { val value = (); val symbol = 17 }
case object `)` extends Token { val value = (); val symbol = 18 }
case object `,` extends Token { val value = (); val symbol = 19 }
case object `*` extends Token { val value = (); val symbol = 20 }
case object `?` extends Token { val value = (); val symbol = 21 }
case object SEPARATOR extends Token { val value = (); val symbol = 22 }
case object `=>` extends Token { val value = (); val symbol = 23 }
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
case object `::` extends Token { val value = (); val symbol = 40 }
case object `+` extends Token { val value = (); val symbol = 41 }
case object `-` extends Token { val value = (); val symbol = 42 }
case object `.` extends Token { val value = (); val symbol = 43 }
case object `$` extends Token { val value = (); val symbol = 44 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](73)
         locally {
          arr(0) = "$end"
          arr(1) = "error"
          arr(2) = "UNDEF"
          arr(3) = "ID"
          arr(4) = "NUM"
          arr(5) = "CODE"
          arr(6) = "STRING"
          arr(7) = "COMMENT"
          arr(8) = "LEXICALERROR"
          arr(9) = "`[`"
          arr(10) = "`]`"
          arr(11) = "`;`"
          arr(12) = "`=`"
          arr(13) = "`|`"
          arr(14) = "`:`"
          arr(15) = "`{`"
          arr(16) = "`}`"
          arr(17) = "`(`"
          arr(18) = "`)`"
          arr(19) = "`,`"
          arr(20) = "`*`"
          arr(21) = "`?`"
          arr(22) = "SEPARATOR"
          arr(23) = "`=>`"
          arr(24) = "`%path`"
          arr(25) = "`%type`"
          arr(26) = "`%empty`"
          arr(27) = "`%notation`"
          arr(28) = "`%package`"
          arr(29) = "`%token`"
          arr(30) = "`%left`"
          arr(31) = "`%right`"
          arr(32) = "`%non`"
          arr(33) = "`%rules`"
          arr(34) = "`%include`"
          arr(35) = "`%prec`"
          arr(36) = "`%tables`"
          arr(37) = "`%dialect`"
          arr(38) = "`%scalalr`"
          arr(39) = "`%signature`"
          arr(40) = "`::`"
          arr(41) = "`+`"
          arr(42) = "`-`"
          arr(43) = "`.`"
          arr(44) = "`$`"
          arr(45) = "$accept"
          arr(46) = "Notation"
          arr(47) = "Prefix"
          arr(48) = "INCLUDE"
          arr(49) = "OPTNL"
          arr(50) = "STRINGorID"
          arr(51) = "TypedTerminals"
          arr(52) = "TypedTerminal"
          arr(53) = "Rules"
          arr(54) = "Rule"
          arr(55) = "OptBar"
          arr(56) = "LHS"
          arr(57) = "RHS"
          arr(58) = "Production"
          arr(59) = "NamedFields"
          arr(60) = "NamedField"
          arr(61) = "FIELD"
          arr(62) = "REPEAT"
          arr(63) = "Precedence"
          arr(64) = "Type"
          arr(65) = "Types"
          arr(66) = "Action"
          arr(67) = "Scala"
          arr(68) = "Scalas"
          arr(69) = "ScalaAtom"
          arr(70) = "ScalaID"
          arr(71) = "S_1_L"
          arr(72) = "S_1"
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
    "*" -> `*`,
    "?" -> `?`,
    "=>" -> `=>`,
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
    "::" -> `::`,
    "+" -> `+`,
    "-" -> `-`,
    "." -> `.`,
    "$" -> `$`,
    ""->$end)
}
