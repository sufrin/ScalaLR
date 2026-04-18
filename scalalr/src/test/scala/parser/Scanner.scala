
package scalalr.parser.ScalaLR
object Scanner {

  import org.sufrin.utility.SourceTextCursor
  import org.sufrin.scalalr.SourceLocation

  object Scanner {
    def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
  }

  class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
    def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
    @inline def hasChar: Boolean = chars.hasCurrent
    @inline def theChar: Char = chars.current
    @inline def nextChar(): Unit = chars.next()
    @inline def afterNextChar(t: Token): Token = { nextChar(); t }

    def eatComment(): Unit = {
      var level = 0
      var go = true
      //println(s"start comment $theChar")
      nextChar() // skip the *
      while (go && hasChar) {
        //print(theChar)
        chars.dropWhile( c=>c!='*')
        // theChar=='*' or !hasChar
        //print(theChar)
        nextChar()
        //println(theChar)
        if (theChar=='/') go=false
      }
      //println("end comment")
      nextChar()
    }

    def isBisonic(c: Char): Boolean = c.isLetterOrDigit || c=='.' || c=='_'

    def hasNext: Boolean = chars.hasCurrent
    def next(): Token = {
      chars.current match {
        // NL at the start of a line is just a space
        //case '\n' if chars.chars>0    =>
        //         chars.current = ' '            // the subsequent next() skips this space without accounting
        //         NL                             // NL once

        case '(' => afterNextChar(`(`)
        case ')' => afterNextChar(`)`)
        case '[' => afterNextChar(`[`)
        case ']' => afterNextChar(`]`)
        case '|' => afterNextChar(`|`)
        case '=' => afterNextChar(`=`)
        case ',' => afterNextChar(`,`)
        case ':' => afterNextChar(`:`)
        case ';' => afterNextChar(`;`)
        case '%' =>
          nextChar()
          val directive = chars.takeWhile(_.isLetterOrDigit).mkString("")
          directive.toLowerCase match {
                  case "type"         => `%type`
                  case "empty"        => `%empty`
                  case "notation"     => `%notation`
                  case "package"      => `%package`
                  case "token"        => `%token`
                  case "left"         => `%left`
                  case "right"        => `%right`
                  case "non"          => `%non`
                  case "rules"        => `%rules`
                  case "include"      => `%include`
                  case "path"         => `%path`
                  case _ => LEXICALERROR(s"Unknown directive %$directive (at $sourceLocation)")
           }
        case '/' =>
          nextChar()
          theChar match {
            case '*' =>
              eatComment()
            case '/' =>
              chars.dropWhile( c=>c!='\n')
            case other =>
              //Syntax.Parser.warn(s"Malformed comment sentinel: \"/$other\" at $sourceLocation")
              chars.dropWhile( c=>c!='\n')
          }
          next()
        case '{' => // } to balance the %include
          nextChar(); afterNextChar(CODE(chars.takeNested('{', '}')  .mkString("")))
        case '«' => // » to balance the %include
          nextChar(); afterNextChar(CODE(chars.takeNested('«', '»')  .mkString("")))
        case '"'  => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='"')  .mkString("\"", "", "\"")))
        case '\'' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='\'') .mkString("\"", "", "\"")))
        case '`' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='`') .mkString("\"", "", "\"")))
        case c if c.isLetter =>
          val prefix = chars.takeWhile(isBisonic)
          ID((prefix).mkString(""))
        case c if c.isWhitespace =>
          while (hasChar && theChar.isWhitespace) nextChar()
          if (hasChar) next() else $end
        case other =>
          LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
      }
    }
  }


trait Token extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } 
case class ID(value: String) extends Token { val symbol = 3 }
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
case object NL extends Token { val value = (); val symbol = 19 }
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
, 19 -> "NL"
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
// GLOSSARY OF NONTERMINAL SYMBOL NAMES
, 32 -> "$accept" 
, 33 -> "command" 
, 34 -> "Notation" 
, 35 -> "OptInclude" 
, 36 -> "Tokens" 
, 37 -> "TokenSpec" 
, 38 -> "TypedTerminals" 
, 39 -> "TypedTerminal" 
, 40 -> "Rules" 
, 41 -> "Rule" 
, 42 -> "OptSemicolon" 
, 43 -> "LHS" 
, 44 -> "RHS" 
, 45 -> "Production" 
, 46 -> "NamedFields" 
, 47 -> "NamedField" 
, 48 -> "Action" 
, 49 -> "Precedence" 
, 50 -> "Type" 
, 51 -> "Types" 
)

}
