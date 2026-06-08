
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
case object `*` extends Token { val value = (); val symbol = 36 }
case object `?` extends Token { val value = (); val symbol = 37 }
case object `+` extends Token { val value = (); val symbol = 38 }
case object `=>` extends Token { val value = (); val symbol = 39 }
case object `$` extends Token { val value = (); val symbol = 40 }
case object `::` extends Token { val value = (); val symbol = 41 }
case object `-` extends Token { val value = (); val symbol = 42 }
case object `.` extends Token { val value = (); val symbol = 43 }
case object SEPARATOR extends Token { val value = (); val symbol = 44 }
case object HIGH extends Token { val value = (); val symbol = 45 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](79)
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
          arr(36) = "`*`"
          arr(37) = "`?`"
          arr(38) = "`+`"
          arr(39) = "`=>`"
          arr(40) = "`$`"
          arr(41) = "`::`"
          arr(42) = "`-`"
          arr(43) = "`.`"
          arr(44) = "SEPARATOR"
          arr(45) = "HIGH"
          arr(46) = "$accept"
          arr(47) = "Notation"
          arr(48) = "Prefix"
          arr(49) = "INCLUDE"
          arr(50) = "OPTSEPARATOR"
          arr(51) = "STRINGorID"
          arr(52) = "TypedTerminals"
          arr(53) = "TypedTerminal"
          arr(54) = "Rules"
          arr(55) = "Rule"
          arr(56) = "LHS"
          arr(57) = "RHS"
          arr(58) = "Production"
          arr(59) = "Fields"
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
          arr(71) = "S_1"
          arr(72) = "S_2_L"
          arr(73) = "S_2"
          arr(74) = "S_3"
          arr(75) = "S_4_L"
          arr(76) = "S_4"
          arr(77) = "S_5_L"
          arr(78) = "S_5"
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
    "=>" -> `=>`,
    "$" -> `$`,
    "::" -> `::`,
    "-" -> `-`,
    "." -> `.`,
    ""->$end)
}
