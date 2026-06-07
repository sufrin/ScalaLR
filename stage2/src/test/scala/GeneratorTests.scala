package org.sufrin.scalalr
package stage2

/**
 *  Tests runnable directly (without installation) from IntelliJ.
 *  For best location reporting avoid
 */

object test0 extends Test("-h")("""
     %tables    ielr
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"
     %rules
     FOO = `bar`
     """)

object test1 extends Test("-pp")(
  """%tables    ielr
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"

     %rules
     Rule1: Unit = S1 S2 S3 {{ Co{de }};
     Rule2: Unit = a: S4 b: S5 { () };
     Rule3: Unit = a: S4 b: S5 {{ this is  unba} }lan{ced { }}


     """)

object test2 extends Test("-pp -Lsym")(
  """
     %tables    ielr
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"
     %include { This is the inclusion }
     %rules
     Rule1: Unit = S1 S2 S3 { Code }

     Rule2: Unit = a: S4 b: S5 { () }

     """)

object test2a extends Test("-pp -Lsym")(
  """// test2
     //  prologue lines in different order
     //  rules separated by a semicolon
     %include { This is the inclusion }
     %tables    ielr
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"
     %rules
     Rule1: Unit = S1 S2 S3 { Code };Rule2: Unit = a: S4 b: S5 { () };

     """.stripMargin)


object test3 extends Test("-Lsym")(
 """ %tables    ielrx
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"


     %include { This is the inclusion }

     %token S1 S2
     %left 'a' 'b'

     %rules

     %include { Rules inclusion }

     Rule1: Unit = S1 S2 S3 { Code }

     Rule2: Unit = a: S4 b: S5 { () }

     /* Tail comment */

     """)


object test4 extends Test()(
  """%tables    ielr
     %notation  stage2test
     %package   scalalr.stage2test
     %path      "parser"


     %include { This is the inclusion }

     %token A B C(D) E: F[G,H] Rule3
     %token I J K
     %left  '-'
     %right '+:' '-:'


     %rules

     %include { Rules inclusion }

     Rule1: Unit = S1 S2 S3 { Code }

     Rule2: Unit = a: S4 b: S5 { () }

     Rule3: Unit = Rule1 "+" Rule2 { "sum" }   Rule2 "-" Rule1 { () }

     Rule1: Unit = %empty

     '+':S = FOO

     /* Tail comment */


     """)

object test5 extends Test()(
  """
     %token T1 U1 V1 T2 U2 V2 T3 U3 V3
     %left '+' '-'
     %right '+:' '-:'

     %rules

     Rule1: T = T1 U1 { Tee1 } | T2 U2 { Tee2 }

     Rule2: T = T2;
     Rule3: T = T3 U3 V3;
     Rule2: V = T3 U3 V3 {xxx}
              | T4 T5
     """)

object test6 extends Test()(
  """
%tables    ielr
%notation  stage2test
%package   scalalr.stage2test
%path      "parser"


%include { This is the inclusion }

%token A B C(D) E: F[G,H] Rule3
%token I J K
%left  '-'
%right '+:' '-:'


%rules

%include { Rules inclusion }

Rule1: Unit = S1 S2 S3 { Code }

Rule2: Unit = a: S4 b: S5 { () }

Rule3: Unit = Rule1 "+" Rule2 { "sum" } | Rule2 "-" Rule1 { () }

Rule1: Unit = %empty

'+':S = FOO

/* Tail comment */

  """)

object textExpand extends Test("-Lsyn -html -Lsym -Lsyn")(
  """%notation  expand
     %package   expand
     %path      "expand"
     %token a b c
     %left `,`
     %rules
     TOP = A | B | C | D | E | F | G;
     A: ATYPE = a | B;
     B: BTYPE  = b me: (C)?;
     C: CTYPE  = a (A B)? c;
     D = a me: (A y:B)? c;
     E = (A y:B)?;
     F = (',' A B )+;
     G = (',' A B )*
  """)

object testSmall extends Test("-rose --output=examples/Lists/generated/ -html -Lsym -Lsyn")(
  """

%notation  Small
%package   small.Small
%path      "small"

%dialect   "host dialect: bootstrap syntax or stage1 or stage2 syntax"
%scalalr   ""


%include {
   import org.sufrin.utility.{SourceTextCursor}
   import org.sufrin.scalalr.SourceLocation


    def Scanner(chars: SourceTextCursor): Scanner = new Scanner(chars)


    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)

      @inline def hasChar: Boolean = chars.hasCurrent
      @inline def theChar: Char = chars.current
      @inline def nextChar(): Unit = chars.next()
      @inline def afterNextChar(t: Token): Token = {
        nextChar()
        t
      }

      def hasNext: Boolean = chars.hasCurrent

      def next(): Token = if (hasNext)
      {
          chars.current match {
            case ';' => afterNextChar(`;`)

            case c if c.isLetter =>
              val prefix = chars.takeWhile(_.isLetterOrDigit)
              ID((prefix).mkString(""))
            case c if c.isWhitespace =>
               while (hasChar && theChar.isWhitespace) nextChar()
               if (hasChar) next() else $end
            case other =>
               LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
          }
      } else $end
    }


}

%token ID(String) ";" LEXICALERROR(String)

%rules
%include {
 // after rules
}

top: Unit = ids { println($ids) };
ids:(List[String]) = (';' ID)+;



  """)

object testTiny extends Test("-c -Lsyn")("""
        %notation TinyFun
        %package  tinyfun
        %path     "tinyfun"
        %tables   "ielr"

        %include {
           import org.sufrin.scalalr.SourceLocation
           import org.sufrin.utility.SourceTextCursor

            def Scanner(chars: SourceTextCursor): Scanner = new Scanner(chars)


            class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
              def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
              @inline def hasChar: Boolean = chars.hasCurrent
              @inline def theChar: Char = chars.current
              @inline def nextChar(): Unit = chars.next()
              @inline def afterNextChar(t: Token): Token = {
                nextChar()
                t
              }

              def hasNext: Boolean = chars.hasCurrent
              def next(): Token = if (hasChar) {
                  chars.current match {
                    case '\n'     =>
                         chars.current = ' '            // the subsequent next() skips this space without accounting
                         NL                             // NL once
                    case 0004     => $end               // invariantly
                    case '.'      => $end               // invariantly

                    case '(' => afterNextChar(`(`)
                    case ')' => afterNextChar(`)`)
                    case '[' => afterNextChar(`[`)
                    case ']' => afterNextChar(`]`)
                    case '/' => afterNextChar(`/`)
                    case '-' => afterNextChar(`-`)
                    case '+' => afterNextChar(`+`)
                    case '*' => afterNextChar(`*`)
                    case ',' => afterNextChar(`,`)
                    case '=' => afterNextChar(`=`)
                    case c if c.isLetter =>
                      val prefix = chars.takeWhile(_.isLetterOrDigit)
                      prefix.mkString("") match {
                        case "quit" => QUIT
                        case other  => ID(other)
                      }
                    case c if c.isDigit =>
                      val prefix = chars.takeWhile(c=>c.isDigit||c=='.')
                      NUM((prefix).mkString(""))
                     case c if c.isWhitespace =>
                       while (hasChar && theChar.isWhitespace) nextChar()
                       if (hasChar) next() else $end
                     case other =>
                       LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")

                  }
              } else $end
            }
        }

        %token NUM: String ID: String  LEXICALERROR: (String) NL // QUIT //`(` `)` `[` `]` `,` `#` HIGH

        %right `=`
        %left `+` `-`
        %left `*` `/`
        %prec HIGH




        %rules

        %include {
         import org.sufrin.scalalr.SourceLocation
         import tinyfun.TinyFun._
        }

        /*
                This is typical of a grammar needed to run as a read-expr/run loop as it is parsed.

                The `command` production is a "hook" that is parsed by parsing an expr, then
                reduced when the NL appears to its right (as the lookahead symbol).
                It is at the reduction that the parsed $expr is run.

        */

        loop: Unit =
                  %empty          { () }
                | loop command NL { () }
                ;

        command: Unit = expr { run(List($expr)) } | "QUIT" { System.exit(0) };


        expr: Expr  = prim                //{ $prim }
                    | ID `=` expr         { Assign($ID, $expr, $START) }
                    | l:prim  r:prim      { Binop("*", $l, $r, $START) } %prec HIGH
                    | l:expr `*` r:expr   { Binop("*", $l, $r, $START) }
                    | l:expr `+` r:expr   { Binop("+", $l, $r, $START) }
                    | l:expr `/` r:expr   { Binop("/", $l, $r, $START) }
                    | l:expr `-` r:expr   { Binop("-", $l, $r, $START) }
                    | "(" expr ")"        { $expr }

         prim: Expr = ID               { Id($ID, $START) }
                    |number            { $number }
                    | ID `[` exprs `]` { Apply($ID, $exprs, $START) }


        exprs: (List[Expr]) =
                    expr            { List($expr) }
                |   exprs `,` expr  { $expr::$exprs }
       ;

        number: Expr = NUM      { Num($NUM.toDouble, $START) }
                     | `#`  NUM { Num($NUM.toInt, $START) }

       """
)


object testAuto extends Test("--output=TEST-GENERATED  -Lsym -Lsyn")(
  """
     %notation  Auto
     %package   auto.Auto
     %path      "auto"
     %token ID(String)
     %include {

     }
     %rules
       expr: Expr =
             ID '(' exprlist: (',' expr)* ')'   => Apply($exprlist, $expr)  // expr is not declared: it's part of the * expression
           |    '{' exprlist: (';' expr)+ '}'   => Sequence($ID, $exprlist) // ID is not declared; expr is not declared: it's part of the * expression
           |    "RETURN" optexpr:  (expr)? ';'  => Return($optexpr)
  """)

object testArrow extends Test("--output=TEST-GENERATED  -Lsym -Lsyn")(
  """
     %notation  Arrow
     %package   arrow.Arrow
     %path      "arrow"
     %token ID(String)
     %include {

     }
     %rules
       expr: Expr =
             ID '(' exprlist: (',' expr)* ')'   => Apply(exprlist, $expr)
           |    '{' exprlist: (';' expr)+ '}'   => Sequence(ID, $exprlist.length + 4)
           | "RETURN" optexpr:  (expr)? ';'     => Return($optexpr.get :: Nil)
           | "RETURN" optexpr:  (expr)? ';'     => Return($optexpr.get :: Nil)
           | l: expr '+' r: expr ';'     => Operate("+", $l, r)
           |  "RETURN" optexpr:  (expr)? ';'  => Return(optexpr.get)
  """)

object testAutoMisc extends Test("--output=TEST-GENERATED  -Lsym -Lsyn")(
  """
     %notation  Auto
     %package   auto.Auto
     %path      "auto"
     %token ID(String) RETURN FOOTLE
     %rules
       expr: Expr =
             ID '(' exprlist: (',' expr expr)* ')'      { Apply($ID, $exprlist) }
           |    '{' exprlist: (expr ';')+ '}'           { Sequence($ID, $exprlist) }
           |    RETURN optexpr:  ('->' expr )? ';'      { Return($optexpr) }
           |    RETURN optexpr:  (',' expr '<-')? ';'   { Return($optexpr) }
           |    RETURN optexpr:  (expr '<-')? ';'       { Return($optexpr) }
  """)

object testInfer extends Test("--output=TEST-GENERATED  -Lsym -Lsyn")(
  """
     %notation  Infer
     %package   infer.Infer
     %path      "infer"
     %token ID(String) RETURN FOOTLE '(' ')'
     %rules

     expr = ID
          | this:ID
          | '(' ID ')'
          | '(' ID that: pig')'

     pig = ID

  """)

object testSelf extends Test ()(
  """

 /**

     Forgiving syntax for grammars. THIS notation file is in the notation described HERE. In short
     parsers using the components that it describes are SELF-HOSTING.

     A ';' followed by any amount of whitespace everywhere denotes an SEPARATOR.

     In the %rules section rules are separated by SEPARATOR symbols, and these
     are represented either as ';' or as a visible vertical gap of any depth (>1).

     The detail is implemented in `LexicalScanner.next()` as it scans "leading" whitespace in a symbol.

 */


 %notation  stage2
 %package   scalalr.stage2
 %path      "parser"


 %dialect   "scalalr stage2 notation"
 %scalalr   "components made by stage1 or stage2"

 %include {
   // Substantive lexical scanner is elsewhere
 }

 %token
   ID:(org.sufrin.scalalr.stage2.AST.Name)
   NUM:(String)
   CODE:(String)
   COMMENT:(String)
   LEXICALERROR:(String)
   `[` `]` `;` `=` `|`  `:`  // Left here as a test of diagnostics
   `{` `}` `(` `)` ',' SEPARATOR
   `%path`       `%type`     `%empty`    `%notation`     `%package` `%token`
   `%left`       `%right`    `%non`      `%rules`
   `%include`    `%prec`     `%tables`   `%dialect`      `%scalalr` `%signature`

 %rules

 %include {
  import org.sufrin.scalalr.stage2.AST._
  import org.sufrin.scalalr.stage2.Generator._
  import scalalr.stage2.Scanner
  import org.sufrin.utility.SourceTextCursor
  import org.sufrin.scalalr.SourceLocation
  import org.sufrin.utility.PrettyPrint._

  def makeTupleType(types: Seq[Type], location: SourceLocation): Type =
      types.size match {
        case 1 => types(0)
        case n => Type(s"Tuple$n", types, location)
      }

  def mkTableType(tableTypeName: String): String =
      tableTypeName match {
         case "lr"          => "canonical-lr"
         case "canonical"   => "canonical-lr"
         case "ielr"        => "ielr"
         case "lalr"        => "lalr"
         case _      => println(s"Warning: wrong %tables type $tableTypeName; canonical assumed");  "canonical-lr"
      }

  implicit class StringOps(val string: String) extends AnyVal {
           def unQuoted: String = string  match {
               case s"\"$unquoted\"" => unquoted
               case unquoted => unquoted
           }

           def asPath: String = string.replace('/', '.').replace('.', '/') match {
               case s"\"$unquoted\"" => unquoted
               case unquoted => unquoted
           }
  }

  implicit class NotationUtilities(val p: Notation) extends AnyVal {
         def withTokenDeclaration(wrap: List[TypedTerminal] => TokenSpec)(terminals: List[TypedTerminal]): Notation =
                 p.copy(theTokens = wrap(terminals) :: p.theTokens)

         def withSignature(signature: String): Notation =
                  p.copy(theSignature = s"${p.theSignature} $signature")
  }
 }

 command: Unit = Notation { translate($Notation) }

 Notation: Notation =
              Prefix
              `%rules`
              INCLUDE
              Rules
              OPTSEPARATOR
              { $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) }

 // A left fold that accumulates incremental changes to the initial default notation AST

 Prefix: Notation =   %empty                                     { Notation() }
                      | p: Prefix `%notation` ID                 { $p.copy(theName=$ID.toString) }
                      | p: Prefix `%package`  ID                 { $p.copy(thePackage=$ID.toString) }
                      | p: Prefix `%path`     ID                 { $p.copy(theExplicitPath=$ID.asPath) }
                      | p: Prefix `%tables`   ID                 { $p.copy(tablesType=mkTableType($ID.unQuoted)) }
                      | p: Prefix `%include`  CODE               { $p.copy(theTokensInclude=$CODE) }
                      | p: Prefix `%token`    TypedTerminals     { $p.withTokenDeclaration(Tokens)($TypedTerminals) }
                      | p: Prefix `%left`     TypedTerminals     { $p.withTokenDeclaration(Left)($TypedTerminals) }
                      | p: Prefix `%right`    TypedTerminals     { $p.withTokenDeclaration(Right)($TypedTerminals) }
                      | p: Prefix `%non`      TypedTerminals     { $p.withTokenDeclaration(Nonassoc)($TypedTerminals) }
                      | p: Prefix `%prec`     TypedTerminals     { $p.withTokenDeclaration(Prec)($TypedTerminals) }
                      | p: Prefix `%dialect`   ID                { $p.withSignature($ID.unQuoted) }
                      | p: Prefix `%scalalr`   ID                { $p.withSignature($ID.unQuoted) }
                      | p: Prefix `%signature` ID                { $p.withSignature($ID.unQuoted) }


 INCLUDE:      String  = `%include` CODE SEPARATOR { $CODE } | %empty {""}

 OPTSEPARATOR: Unit    = %empty {()} | SEPARATOR {()}

 TypedTerminals:(List[TypedTerminal]) =
               |  %empty                         { Nil }
               |  TypedTerminal TypedTerminals   { $TypedTerminal :: $TypedTerminals }


 TypedTerminal:(TypedTerminal) = ID ':' Type      {  TypedTerminal($ID, $Type, $START)   }
                               | ID '(' Type ')'  {  TypedTerminal($ID, $Type, $START) }   // for compatibility with boot notation
                               | ID               {  TypedTerminal($ID, NoType, $START) }


 Rules:(List[Rule]) = Rule           { List($Rule) }
                    | Rules SEPARATOR Rule  { $Rule :: $Rules }

 Rule: Rule = LHS '=' OptBar RHS { Rule($LHS, $RHS, $START) }

 OptBar: Unit = '|' {()} | %empty {()}

 LHS: TypedNonterminal  = ID ':' Type {  TypedNonterminal($ID.warnQuoted, $Type, $START) }
                        | ID          {  TypedNonterminal($ID.warnQuoted, NoType, $START) }


 RHS:(List[Production]) =
       Production         { List($Production)   }
     | Production '|' RHS { $Production :: $RHS }

 Production: Production = NamedFields Action Precedence { Production($NamedFields, $Action, $Precedence, $START) }

 NamedFields:(List[NamedField]) =
       `%empty`               { Nil }
     | NamedField             { List($NamedField) }
     | NamedField NamedFields { $NamedField :: $NamedFields }


 NamedField: NamedField = ID                               { NamedField(theFieldName = None, theField = $ID, $START) }
                        | theFieldName: ID ':' theName: ID { NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) }


 Action:(Option[Expression])    = %empty { None } | CODE { Some($CODE) }

 Precedence: (Option[Terminal]) =  %empty { None } | `%prec` ID { Some(TypedTerminal($ID, NoType, $START))}

 Type:(Type) = ID               { Type($ID.withoutQuotes, Nil, $START) }
             | ID '[' Types ']' { Type($ID.withoutQuotes, $Types, $START) }
             | '('    Types ')' { makeTupleType($Types, $START) }
             | '(' ')'          { Type("Unit", Nil, $START) }


 Types:(List[Type]) = Type            { List($Type) }
                    | Type  ',' Types { $Type :: $Types }

 """
)

object testConflictSAB extends Test("-c")(
  """
    |%notation  SAB
    |%package   SAB
    |%path      "conflicts/SAB"
    |
    |%token a
    |
    |%rules
    |
    | S = A | B
    |
    | A = a
    |
    | B = a
    |
    |
    |""".stripMargin)

object testConflictIFTHEN extends Test("-c")(
  """
    |%notation  IfThenElse
    |%package   IfThenElse
    |%path      "conflicts/IfThenElse"
    |
    |%token IF THEN ELSE ID '+'
    |
    |%rules
    |
    |expr = ID
    |     | expr '+' ID
    |     | IF expr THEN expr
    |     | IF expr THEN expr ELSE expr
    |
    |""".stripMargin)

/* Exercises various warning features of ScalaLR */

object testRedOx extends Test("-Lsyn -Lsym")(
  """
%notation RedOx
%package  org.redox
%path     ""
%tables   lr

/* Exercise various warning features of ScalaLR */

%token NUM(String) ID(String)  `(` `)` `[` `]` `,`  NL LEXICALERROR(String) DOUBLE "DOUBLE" ";"
%right `|`
%right `;`
%left `,`

%right `=`
%left `+` `-`
%left `*` `/`
%right `^`
%non DOUBLE "DOUBLE"


%rules

DOUBLE: Proc = proc

proc: Proc =
           prim
           | l: proc `|` r: proc { Parallel(List($l,$r), $START, $END) }
           | l: proc ';' r: proc { Sequence(List($l,$r), $START, $END) }


prim: Proc = ID '=' expr  { Assign($ID, $expr, $START, $END) }
           | `{` proc `}` { Block(Nil, $proc, $START, $END) }


expr: Expr =
          ID                  { Id($ID, $START, $END) }
        | NUM                 { Num($NUM, $START, $END) }
        | l:expr `^` r:expr   { Binop("^", $l, $r, $START, $END) }
        | l:expr `*` r:expr   { Binop("*", $l, $r, $START, $END) }
        | l:expr `+` r:expr   { Binop("+", $l, $r, $START, $END) }
        | l:expr `/` r:expr   { Binop("/", $l, $r, $START, $END) }
        | l:expr `-` r:expr   { Binop("-", $l, $r, $START, $END) }
        | "(" expr ")"        { $expr }



exprs: (List[Expr]) =
            expr            { List($expr) }
        |   exprs `,` expr  { $expr :: $exprs }

 """)

object testEllipsis extends Test("-Lsym -Lsyn")(
  """
%notation Ell
%package  org.ell
%path     ""
%tables   lr

%token a: String b: String c: String ';'

%rules

letter: String = a | b | c

ellipsis: List[String]  = ls: (letter ';')... '.' {$ls}

foo: Option[String] = l: (letter)? {$l}

ellipsis1 = ls: (letter ';')* '.' {$ls}

ellipsis2 = ls: (';' letter)+..  '.' {$ls}

ellipsis3 = first: (letter)?     { $first.toList }
          | ls: (';' letter)+  { $ls}

thing: List[String] = '{' ellipsis3 '}' { $ellipsis3 }

  """)

object testEllipsis2 extends Test("-Lsym -Lsyn")(
  """
%notation Ell
%package  org.ell
%path     ""
%tables   lr

%token a: String b: String c: String ';'

%rules

letter: String = a | b | c

LetterSemiEllipsis: List[String]  = '{' ls: (letter ';')...  => $ls

LetterSemiStar: List[String]  = '{' ls: (letter ';')*  => $ls

LetterLetterStar: List[String]  = '{' ls: (letter letter)*  => $ls

PunctPunctStar: List[String]  = '{' ls: (';' '%')*  => $ls

LetterSemiPlus: List[String]  = '{' ls: (letter ';')+  => $ls

LetterSemiStarDot: List[String]  = '{' ls: (letter ';')*..  => $ls

LetterStarDot: List[String]  = '{' ls: (LetterSemiStarDot)*..  => $ls

LetterSemiPlusDot: List[String]  = '{' ls: (letter ';')+..  => $ls



ellipsos: List[String]  = '{' ls: (letter )...  {$ls};

ellipsas: List[String]  = '{' ls: (letter letter letter)...  {$ls}


  """)

