
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
case object `=>` extends Token { val value = (); val symbol = 42 }
case object `$` extends Token { val value = (); val symbol = 43 }
// MAP SYMBOL NUMBERS TO NAMES
val symbolName: collection.immutable.Map[Int, String] = {
     import org.sufrin.utility.ArrayMap
    val arr = new Array[String](69)
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
          arr(19) = "`*`"
          arr(20) = "`?`"
          arr(21) = "SEPARATOR"
          arr(22) = "`%path`"
          arr(23) = "`%type`"
          arr(24) = "`%empty`"
          arr(25) = "`%notation`"
          arr(26) = "`%package`"
          arr(27) = "`%token`"
          arr(28) = "`%left`"
          arr(29) = "`%right`"
          arr(30) = "`%non`"
          arr(31) = "`%rules`"
          arr(32) = "`%include`"
          arr(33) = "`%prec`"
          arr(34) = "`%tables`"
          arr(35) = "`%dialect`"
          arr(36) = "`%scalalr`"
          arr(37) = "`%signature`"
          arr(38) = "`::`"
          arr(39) = "`+`"
          arr(40) = "`-`"
          arr(41) = "`.`"
          arr(42) = "`=>`"
          arr(43) = "`$`"
          arr(44) = "$accept"
          arr(45) = "Notation"
          arr(46) = "Prefix"
          arr(47) = "INCLUDE"
          arr(48) = "OPTNL"
          arr(49) = "TypedTerminals"
          arr(50) = "TypedTerminal"
          arr(51) = "Rules"
          arr(52) = "Rule"
          arr(53) = "OptBar"
          arr(54) = "LHS"
          arr(55) = "RHS"
          arr(56) = "Production"
          arr(57) = "NamedFields"
          arr(58) = "NamedField"
          arr(59) = "FIELD"
          arr(60) = "REPEAT"
          arr(61) = "Precedence"
          arr(62) = "Type"
          arr(63) = "Types"
          arr(64) = "Action"
          arr(65) = "Scala"
          arr(66) = "Scalas"
          arr(67) = "ScalaPlus"
          arr(68) = "ScalaAtom"
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
    "=>" -> `=>`,
    "$" -> `$`,
    ""->$end)
}
