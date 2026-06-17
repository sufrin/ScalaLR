package org.sufrin.scalalr
package stage2
package runtime


object  lists extends TestRUN("-Lsyn -Lsym -html", "lists.Lists")(
  """1,2,3,4,5
    |6,7,8,9,0
    |
    |""".stripMargin
)(
"""
%notation  Lists
%package   lists.Lists
%path      "lists"

%token     LONG(Long) NL

%include {
  import org.sufrin.scalalr.{Scanner,ScannerAdapter}
  import org.sufrin.utility.SourceTextCursor

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerAdapter[Token](chars) {
       val self = lists.Lists.Scanner
       val ENDSTREAM: Token                  = self.$end
       val UNDEF: Token                      = self.UNDEF
       val symbolToken                       = self.symbolToken
       override def LONG(value: Long): Token = self.LONG(value)
       override val NEWLINE                  = Some(self.NL)
  } withSymbolTokens(symbolToken)
}

%token LONG(Long) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._
}

topLevel:  Unit = ListOfLists => println($ListOfLists)

ListOfLists: List[List[Long]] = list: (NL List) ... => $list

List:  List[Long]             = list: (',' LONG) ... => $list

""")

/**
 *  This tests demonstrates top-level loops with
 *  shortcut-result transmission to the top level
 *  running parser.
 *
 *  Comparing it with interactive.Lists you will find
 *  that (1) it no longer has the problem of having to
 *  do a system exit (program closedown) within the parser;
 *  and (2) the type of the production `aLine` is
 *  `Interactive.Continuation`, and such continuations
 *  take effect when productions yield their result.
 */
object  shortcutLists extends TestRUN("-Lsyn -Lsym -html", "shortcut.Lists")()(
  """
%notation  Lists
%package   shortcut.Lists
%path      "shortcut"

%token     LONG(Long) NL

%include {
  import org.sufrin.scalalr._
  import org.sufrin.utility._

  lazy val self = this

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerAdapter[Token](chars) {
       val ENDSTREAM: Token                  = self.$end
       val UNDEF: Token                      = self.UNDEF
       val symbolToken                       = self.symbolToken
       override def LONG(value: Long): Token     = self.LONG(value)
       override def DOUBLE(value: Double): Token = self.LONG(value.toLong)
       override val NEWLINE                      = Some(self.NL)
  }    withSymbolTokens(symbolToken)


}

%token LONG(Long) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import shortcut.Lists._
 import org.sufrin.scalalr.Interactive._
}


loop:   Unit        = (NL)? (NL aLine)... => ()

aLine:  Continuation  = theList: aList { println($theList); Continue }
                      | "."            { Accept("Finished") }

aList:  List[Long] = theList: (',' LONG)+ => $theList

""")

/**
 *  This test demonstrates top-level loops of
 *  the kind that are useful in read-eval-print settings.
 *
 *  Comparing it with shortcut.Lists you will find
 *  that (1) it has the problem of having to
 *  do a system exit (program closedown) within the parser;
 *  and (2) the type of the production `aLine` is
 *  more or less arbitrarily constructed to be a type
 *  that has only a single inhabitant.
 *
 *  TODO: Unit is also such a type (inhabitant is {}), and would be more elegant to use here, but
 *  at present (June 2026) the ScalaLR code generator uses that for something else.
 */
object  interactiveLists extends TestRUN("-Lsyn -Lsym -html", "interactive.Lists")()(
  """
%notation  Lists
%package   interactive.Lists
%path      "interactive"

%token     LONG(Long) NL

%include {
  import org.sufrin.scalalr._
  import org.sufrin.utility._

  lazy val self = this

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerAdapter[Token](chars) {

       val ENDSTREAM: Token                  = self.$end
       val UNDEF: Token                      = self.UNDEF
       val symbolToken                       = self.symbolToken
       override def LONG(value: Long): Token     = self.LONG(value)
       override def DOUBLE(value: Double): Token = self.LONG(value.toLong)
       override val NEWLINE                      = Some(self.NL)
  }    withSymbolTokens(symbolToken)


}

%token LONG(Long) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import interactive.Lists._
 trait NONE
 case object NONE extends NONE
}


loop : NONE    = (NL)? (NL aLine)... => NONE

aLine: NONE    = theList: aList { println($theList); NONE }
               | "."            { println("Finished"); System.exit(0); NONE }

aList:  List[Long] = theList: (',' LONG)+ => $theList

""")


object expression extends TestRUN("-Lsyn -Lsym -html", "expr.Expression")()(
"""
%notation  Expression
%package   expr.Expression
%path      "expr"
%signature "arithmetic expressions"

%include {
  import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
  import org.sufrin.utility.SourceTextCursor

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
       def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
       def mkHex(source: Seq[Char]):   Token = HEX(source.mkString)
       def mkDec(source: Seq[Char]):   Token = DEC(source.mkString)
       def mkReal(source: Seq[Char]):  Token = REAL(source.mkString)
       def mkID(source: Seq[Char]):    Token = ID(source.mkString)
       def mkERROR(source: Seq[Char]): Token = { System.err.println(source.mkString); throw new Throwable(source.mkString) }
       val ENDSTREAM: Token = $end
       val NEWLINE:   Option[Token] = Some(NL)
       def flush(): Unit = {
           while (chars.hasCurrent && chars.current != '\n') chars.next()
           print(chars.prompt); System.out.flush()
       }
  } withSymbolTokens(symbolToken)
}

%token HEX(String) DEC(String) REAL(String) ID(String) STRING(String) NL
%left '+' '-'
%left '*' '/'

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import expr.Expression._
 import org.sufrin.utility.PrettyPrint._

 trait Expr
 case class Bin(op: String, l: Expr, r: Expr) extends Expr
 case class Atom(string: String)              extends Expr
 case class Bra(e: Expr)                      extends Expr
 case class Quoted(string: String)            extends Expr { override val toString = s"$string" }

 def reHexify(string: String): String = s"0x$string"
}

loop:    Unit   = (NL)? (NL oneLine)* => ()         ;// top-level loop ignores empty lines

oneLine: String = expr { $expr.prettyPrint(); "" }  ;// a line is just an expression

expr: Expr = atom
           | l: expr '+' r: expr => Bin("+", $l, $r)
           | l: expr '*' r: expr => Bin("*", $l, $r)
           | l: expr '-' r: expr => Bin("-", $l, $r)
           | l: expr '/' r: expr => Bin("/", $l, $r)
           | '(' expr ')' => Bra($expr)


atom: Expr = prim         => Atom($prim)
           | STRING       => Quoted($STRING)

prim: String = HEX => reHexify($HEX) | DEC | ID | REAL | ID

""")

object readmeExpression extends TestSRC("-html")(
"""
%notation  Expr                                             // §0
%package   expr.Expr                                        // §0
%path      "expr"
%tables    lalr                                             // §0

%include {
   // Scala source here is included in the generated object expr.Expr.Scanner
}

%token ID(String) `(` `)` `[` `]` `;`                       // §1
%left `+`                                                   // §2
%left `*`

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 // Scala source here is included in the generated object expr.Expr.Reduction
 // that defines the result value for each production.
 // It must import or implement the abstract syntax of the language.
 // Here we do the latter

 trait Expr { val loc: SourceLocation }                                 // §4
 case class Id(s: String, loc: SourceLocation) extends Expr
 case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr
 case class Bra(expr: Expr, loc: SourceLocation)extends Expr

}

exprs: List[Expr] = expr           { List($expr) }                   // §3, §4
                  | exprs `;` expr { $expr::$exprs }                 // §4


expr: Expr = ID                  { Id($ID, $START) }                   // §4,
           | l:expr `*` r:expr   { Binop("*", $l, $r, $START) }
           | l:expr `+` r:expr   { Binop("+", $l, $r, $START) }
           | "(" expr ")"        { Bra($expr, $START) }                //§5
           | `[` expr `]`        { $expr }

"""
)

object readMeExample {
    import expr.Expr.Components
    import expr.Expr.Scanner._
    import org.sufrin.utility.PrettyPrint._
    import org.sufrin.utility._

    class Scanner(source: SourceTextCursor) extends SimpleScanner[Token](source) {
      override val NAME       = expr.Expr.Scanner.ID
      override val ENDSTREAM  = $end
      locally { defineSymbolTokens(expr.Expr.Scanner.symbolToken) }
    }

    def main(args: Array[String]): Unit = {
      val scanner = new Scanner(SourceTextCursor("a*b + c*d; a+b * c+d"))
      val parser  = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.run(scanner.next).prettyPrint()
    }
}




