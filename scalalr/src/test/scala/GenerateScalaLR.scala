package org.sufrin.scalalr

object GenerateScalaLR extends App {
  { val source =
      """%notation  ScalaLR
        |%package   scalalr.parser.ScalaLR
        |%path      "scalalr/src/test/scala/parser"
        |
        |%include {
        |  import org.sufrin.utility.SourceTextCursor
        |  import org.sufrin.scalalr.SourceLocation
        |
        |  object Scanner {
        |    def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
        |  }
        |
        |  class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
        |    def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
        |    @inline def hasChar: Boolean = chars.hasCurrent
        |    @inline def theChar: Char = chars.current
        |    @inline def nextChar(): Unit = chars.next()
        |    @inline def afterNextChar(t: Token): Token = { nextChar(); t }
        |
        |    def eatComment(): Unit = {
        |      var level = 0
        |      var go = true
        |      //println(s"start comment $theChar")
        |      nextChar() // skip the *
        |      while (go && hasChar) {
        |        //print(theChar)
        |        chars.dropWhile( c=>c!='*')
        |        // theChar=='*' or !hasChar
        |        //print(theChar)
        |        nextChar()
        |        //println(theChar)
        |        if (theChar=='/') go=false
        |      }
        |      //println("end comment")
        |      nextChar()
        |    }
        |
        |    def isBisonic(c: Char): Boolean = c.isLetterOrDigit || c=='.' || c=='_'
        |
        |    def hasNext: Boolean = chars.hasCurrent
        |    def next(): Token = {
        |      chars.current match {
        |        // NL at the start of a line is just a space
        |        //case '\n' if chars.chars>0    =>
        |        //         chars.current = ' '            // the subsequent next() skips this space without accounting
        |        //         NL                             // NL once
        |        case '\u0004' => $end         // invariantly
        |        case '.'      => NL           // invariantly
        |        case '(' => afterNextChar(`(`)
        |        case ')' => afterNextChar(`)`)
        |        case '[' => afterNextChar(`[`)
        |        case ']' => afterNextChar(`]`)
        |        case '|' => afterNextChar(`|`)
        |        case '=' => afterNextChar(`=`)
        |        case ',' => afterNextChar(`,`)
        |        case ':' => afterNextChar(`:`)
        |        case ';' => afterNextChar(`;`)
        |        case '%' =>
        |          nextChar()
        |          val directive = chars.takeWhile(_.isLetterOrDigit).mkString("")
        |          directive.toLowerCase match {
        |                  case "type"         => `%type`
        |                  case "empty"        => `%empty`
        |                  case "notation"     => `%notation`
        |                  case "package"      => `%package`
        |                  case "token"        => `%token`
        |                  case "left"         => `%left`
        |                  case "right"        => `%right`
        |                  case "non"          => `%non`
        |                  case "rules"        => `%rules`
        |                  case "include"      => `%include`
        |                  case "path"         => `%path`
        |                  case _ => LEXICALERROR(s"Unknown directive %$directive (at $sourceLocation)")
        |           }
        |        case '/' =>
        |          nextChar()
        |          theChar match {
        |            case '*' =>
        |              eatComment()
        |            case '/' =>
        |              chars.dropWhile( c=>c!='\n')
        |            case other =>
        |              //Syntax.Parser.warn(s"Malformed comment sentinel: \"/$other\" at $sourceLocation")
        |              chars.dropWhile( c=>c!='\n')
        |          }
        |          next()
        |        case '{' => // } to balance the %include
        |          nextChar(); afterNextChar(CODE(chars.takeNested('{', '}')  .mkString("")))
        |        case '«' => // » to balance the %include
        |          nextChar(); afterNextChar(CODE(chars.takeNested('«', '»')  .mkString("")))
        |        case '"'  => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='"')  .mkString("\"", "", "\"")))
        |        case '\'' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='\'') .mkString("\"", "", "\"")))
        |        case '`' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='`') .mkString("\"", "", "\"")))
        |        case c if c.isLetter =>
        |          val prefix = chars.takeWhile(isBisonic)
        |          ID((prefix).mkString(""))
        |        case c if c.isWhitespace =>
        |          while (hasChar && theChar.isWhitespace) nextChar()
        |          if (hasChar) next() else $end
        |        case other =>
        |          LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
        |      }
        |    }
        |  }
        |
        |}
        |
        |%token
        |  ID(String)
        |  NUM(String)
        |  CODE(String)
        |  COMMENT(String)
        |  LEXICALERROR(String)
        |  `[` `]` `;` `=` `|`  `:`  // Left here as a test of diagnostics
        |  `{` `}` `(` `)` ',' NL
        |  `%path` `%type` `%empty` `%notation` `%package` `%token` `%left` `%right` `%non` `%rules` `%include` `%prec`
        |
        |%rules
        |
        |%include {
        | import org.sufrin.scalalr.AST._
        | import org.sufrin.scalalr.TranslateScalaLR._
        | import org.sufrin.utility.SourceTextCursor
        | import org.sufrin.scalalr.SourceLocation
        | import org.sufrin.utility.PrettyPrint._
        |
        | def makeTupleType(types: Seq[Type]): Type =
        |     types.size match {
        |       case 1 => types(0)
        |       case n => Type(s"Tuple$n", types)
        |     }
        |}
        |
        |
        |
        |
        |command: Unit = Notation { translate($Notation) };
        |
        |
        |Notation: Notation =
        |   `%notation`     theName:ID
        |   `%package`      thePackage:ID
        |   `%path`         thePath:ID
        |    tokensInclude: OptInclude
        |    Tokens
        |    `%rules`
        |    rulesInclude:  OptInclude
        |    Rules          OptSemicolon
        |    { Notation($thePackage, $theName, $thePath, "Tables", "Scanner", Type("Token", Nil), $Tokens, $Rules, $tokensInclude, $rulesInclude) };
        |
        |OptInclude: String = `%empty` { "" } | `%include` CODE { $CODE };
        |
        |Tokens:(List[TokenSpec]) = %empty           { Nil }
        |                         | TokenSpec Tokens { $TokenSpec :: $Tokens }
        |                         ;
        |
        |TokenSpec: TokenSpec =
        |        `%left`  TypedTerminals { Left($TypedTerminals) }
        |      | `%right` TypedTerminals { Right($TypedTerminals) }
        |      | `%non`   TypedTerminals { Nonassoc($TypedTerminals) }
        |      | `%token` TypedTerminals { Tokens($TypedTerminals) }
        |      ;
        |
        |TypedTerminals:(List[TypedTerminal]) =
        |         TypedTerminal { List($TypedTerminal) }
        |      |  TypedTerminal TypedTerminals  { $TypedTerminal :: $TypedTerminals }
        |      ;
        |
        |TypedTerminal:(TypedTerminal) = ID ':' Type {  TypedTerminal($ID, $Type) }  | ID {  TypedTerminal($ID, Untyped) } ;
        |
        |
        |Rules:(List[Rule]) = Rule           { List($Rule) }
        |                   | Rules ';' Rule { $Rule :: $Rules };
        |
        |Rule: Rule = LHS '=' RHS { Rule($LHS, $RHS) };
        |
        |OptSemicolon: Unit = {()} | `;` {()};
        |
        |LHS: TypedNonterminal  = ID ':' Type {  TypedNonterminal($ID, $Type) }
        |                       | ID          {  TypedNonterminal($ID, Untyped) }
        |                       ;
        |
        |RHS:(List[Production]) =
        |    | Production         { List($Production)   }
        |    | Production '|' RHS { $Production :: $RHS } ;
        |
        |Production: Production = NamedFields Action Precedence { Production($NamedFields, $Action, $Precedence) };
        |
        |NamedFields:(List[NamedField]) =
        |    | `%empty`               { Nil }
        |    | NamedField             { List($NamedField) }
        |    | NamedField NamedFields { $NamedField :: $NamedFields }
        |    ;
        |
        |NamedField: NamedField = ID                              { NamedField(theName = None, fieldSymbol = $ID) }
        |                       | theName: ID ':' fieldSymbol: ID { NamedField(Some($theName), $fieldSymbol) }
        |                       ;
        |
        |Action:(Option[Expression])    = %empty { None } | CODE { Some($CODE) };
        |
        |Precedence: (Option[Terminal]) =  %empty { None } | `%prec` ID { Some(new Terminal($ID)) };
        |
        |Type:(Type) = ID               { Type($ID, Nil) }
        |            | ID '[' Types ']' { Type($ID, $Types) }
        |            | '('    Types ')' { makeTupleType($Types) }
        |            | '(' ')'          { Type("Unit", Nil) }
        |            ;
        |
        |Types:(List[Type]) = Type            { List($Type) }
        |                   | Type  ',' Types { $Type :: $Types }
        |                   ;
        |
        |""".stripMargin
    import org.sufrin.scalalr.Notation.Lexical
    import org.sufrin.scalalr.Notation.Syntax._
    import org.sufrin.scalalr.SourceLocation
    import org.sufrin.utility.PrettyPrint._
    import org.sufrin.utility.SourceTextCursor
    val scanner = Lexical.Scanner(SourceTextCursor(source))
    val notation = Parser(scanner).parseNotation()
    //notation.prettyPrint()
    val translation = Translation(notation)
    translation.makeFiles()

  }
}
