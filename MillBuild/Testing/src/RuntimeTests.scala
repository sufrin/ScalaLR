package org.sufrin.scalalr
package stage2
package runtime


object  lists extends TestRUN("-Lsyn -Lsym -html", "lists.Lists")(
  """0,1,2,3,4,
    |5,6,7,8,9
    |
    |
    |
    |10,11,12,13,14
    |20,21,22,23,24
    |""".stripMargin
)(
  """
%notation  Lists
%package   lists.Lists
%path      "lists"

%token     LONG(Long) NL

%include {
  import org.sufrin.scalalr._
  import org.sufrin.utility._
  lazy val generated = this

  def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
       override val LONG     = generated.LONG
       override val NEWLINE  = Some(generated.NL)
       override def TOKENMAP = TokenMap
  }
}

%token LONG(Long) NL

%rules

%include {
 import org.sufrin.scalalr.SourceLocation
 import lists.Lists._
}


lists: List[List[Long]] = (NL)? theLists: (NL aList)... => $theLists

aList:  List[Long] = theList: (',' LONG)... => $theList

"""
)

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

%include {
  import org.sufrin.scalalr._
  import org.sufrin.utility._
  val generated: this.type = this
  def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
       override val LONG     = generated.LONG
       override val NEWLINE  = Some(generated.NL)
       override def TOKENMAP = TokenMap
  }
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

  lazy val generated = this

  def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
       override val LONG     = generated.LONG
       override val NEWLINE  = Some(generated.NL)
       override def TOKENMAP = TokenMap
  }


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

"""
)

object SharedSpecification {
  import org.sufrin.SourceLocation._
  val loc: SourceLocation = sourcePath
  val expressionNotation =  """
      %notation  Expression
      %package   expr.Expression
      %path      "expr"
      %signature "arithmetic expressions"

      %include {
        import org.sufrin.scalalr._
        import org.sufrin.utility._

        lazy val generated = this

        def apply(chars: SourceTextCursor): Scanner[Token] = new SimpleScannerCore[Token](chars) {
             override val LONG       = generated.LONG
             override val DOUBLE     = generated.DOUBLE
             override val NEWLINE    = Some(generated.NL)
             override val IDENTIFIER = generated.ID
             override val STRING     = generated.QUOTE
             override def TOKENMAP   = TokenMap
        }

      }

      %token LONG(Long) DOUBLE(Double) ID(String) QUOTE(String) NL
      %left '+' '-'
      %left '*' '/'

      %rules

      %include {
       import org.sufrin.scalalr.SourceLocation
       import expr.Expression._
       import org.sufrin.utility.PrettyPrint._
       trait Void
       case object Void extends Void

       trait Expr
       case class Bin(op: String, l: Expr, r: Expr) extends Expr
       case class Bra(e: Expr)                      extends Expr
       case class Quoted(string: String)            extends Expr { override val toString = s"\"$string\"" }
       case class Id(string: String)                extends Expr
       case class Num(double: Double)               extends Expr
      }

      loop: ()   = (NL)? (NL oneLine)... => ()       // top-level loop ignores empty lines

      oneLine: Void  = expr { $expr.prettyPrint(); println("> "); Void }  // a line is just an expression

      expr: Expr =
       | atom
       | l: expr '+' r: expr => Bin("+", $l, $r)
       | l: expr '*' r: expr => Bin("*", $l, $r)
       | l: expr '-' r: expr => Bin("-", $l, $r)
       | l: expr '/' r: expr => Bin("/", $l, $r)
       | '(' expr ')' => Bra($expr)


      atom: Expr =
      | ID      => Id($ID)
      | LONG    => Num($LONG.toDouble)
      | DOUBLE  => Num($DOUBLE)
      | QUOTE   => Quoted($QUOTE)
"""
}

object expression extends TestRUN("-Lsyn -Lsym -html", "expr.Expression")("a+b*c+d")(SharedSpecification.expressionNotation)(loc = SharedSpecification.loc)

object readmeExpression extends TestSRC("-html")(SharedSpecification.expressionNotation)


object readMeExample {
    import expr.Expression.Components
    import expr.Expression.Scanner._
    import org.sufrin.utility.PrettyPrint._
    import org.sufrin.utility._

    val generated = expr.Expression.Scanner

    class MainScanner(chars: SourceTextCursor) extends SimpleScannerCore[Token](chars) {
      override val LONG       = generated.LONG
      override val DOUBLE     = generated.DOUBLE
      override val NEWLINE    = Some(generated.NL)
      override val IDENTIFIER = generated.ID
      override val STRING     = generated.QUOTE
      override def TOKENMAP   = TokenMap
  }

    def main(args: Array[String]): Unit = {
      val scanner = new MainScanner(SourceTextCursor.console.withPrompt("> "))
      print("> ")
      val parser  = LRParser.Pull[Components.Token](Components)(scanner.sourceLocation)
      parser.run(scanner.next).prettyPrint()
    }
}









