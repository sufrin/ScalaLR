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
%signature "trivial grammar of lists"
%token INT(Int)

%include {
  import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
  import org.sufrin.utility.SourceTextCursor

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
       def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
       def mkHex(source: Seq[Char]):   Token = HEX(source.mkString)
       def mkDec(source: Seq[Char]):   Token = DEC(source.mkString)
       def mkReal(source: Seq[Char]):  Token = REAL(source.mkString)
       def mkID(source: Seq[Char]):    Token = ID(source.mkString)
       def mkERROR(source: Seq[Char]): Token = ERROR(source.mkString)
       val ENDSTREAM: Token = $end
       val NEWLINE:   Option[Token] = Some(NL)
       def flush(): Unit = {
           while (chars.hasCurrent && chars.current != '\n') chars.next()
           print(chars.prompt); System.out.flush()
       }
  } withSymbolTokens(symbolToken)
}

%token HEX(String) DEC(String) REAL(String) ID(String) STRING(String) ERROR(String) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._
}

loop: Unit =
          %empty          { () }
        | loop command NL { () }

command: Unit = ListInt => println($ListInt)

ListInt: List[Int] = list: (',' DEC) ... => List($list)

""")

object  ttylists extends TestRUN("-Lsyn -Lsym -html", "lists.Lists")()(
  """
%notation  Lists
%package   lists.Lists
%path      "lists"
%signature "trivial grammar of lists"
%token INT(Int)

%include {
  import org.sufrin.scalalr.{SourceLocation,ScannerBuilder,Scanner}
  import org.sufrin.utility.SourceTextCursor

  def apply(chars: SourceTextCursor): Scanner[Token] = new ScannerBuilder[Token](chars) {
       def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
       def mkHex(source: Seq[Char]):   Token = HEX(source.mkString)
       def mkDec(source: Seq[Char]):   Token = DEC(source.mkString)
       def mkReal(source: Seq[Char]):  Token = REAL(source.mkString)
       def mkID(source: Seq[Char]):    Token = ID(source.mkString)
       def mkERROR(source: Seq[Char]): Token = ERROR(source.mkString)
       val ENDSTREAM: Token = $end
       val NEWLINE:   Option[Token] = Some(NL)
       def flush(): Unit = {
           while (chars.hasCurrent && chars.current != '\n') chars.next()
           print(chars.prompt); System.out.flush()
       }
  } withSymbolTokens(symbolToken)
}

%token HEX(String) DEC(String) REAL(String) ID(String) STRING(String) ERROR(String) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._
}

loop: Unit = (NL)? (NL command)* => ()

command: String = ListInt { println($ListInt); "" }

ListInt: List[Int] = list: (',' DEC) ... => List($list)

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
       def mkERROR(source: Seq[Char]): Token = ERROR(source.mkString)
       val ENDSTREAM: Token = $end
       val NEWLINE:   Option[Token] = Some(NL)
       def flush(): Unit = {
           while (chars.hasCurrent && chars.current != '\n') chars.next()
           print(chars.prompt); System.out.flush()
       }
  } withSymbolTokens(symbolToken)
}

%token HEX(String) DEC(String) REAL(String) ID(String) STRING(String) ERROR(String) NL
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




