package org.sufrin.scalalr
package stage2

/**
 * Feature tests: that include deliberate errors
 */

object Err1 extends Test.COMPONENTS("-Lsyn -Lsym -html")(
"""
// Incomplete grammar
%notation  err1
%package   scalalr.err1
%path      "err1"
%signature "err1"
%tables    lr
/* A nested comment
   /* that is not
   */
   completely closed

%rules

foo: Int = 'x' => 43
""")

object Err2 extends Test.COMPONENTS("-Lsyn -Lsym -html")(
  """
// Wrong table type: still generates code
%notation  err2
%package   scalalr.err2
%path      "err2"
%signature "err2 -- unknown table type"
%tables    ielrx

%rules

foo: Int = 'x' => 43
""")

object Err3 extends Test.COMPONENTS("-Lsyn -Lsym -Lred -html")(
  """
// Undollared rule result
%notation  err3
%package   scalalr.err3
%path      "err3"
%signature "err3 -- undollared parameter in result"
%token INT(Int)
%rules

ListInt: List[Int] = list: (INT) ... => list
""")


object Err4 extends Test.COMPONENTS("-Lsyn -Lsym -html")(
  """
// Known scala operator in result"
%notation  err4
%package   scalalr.err4
%path      "err4"
%signature "err4 -- known scala operator in result"
%token INT(Int)
%rules

ListInt: List[Int] = list: (INT)... => List($list.length / 2)
""")

object Err5 extends Test.COMPONENTS("-Lsyn -Lsym -html")("""
//Checking
%notation  err5
%package   scalalr.err5
%path      "err5"
%signature "err5 -- check // after productions doesn't kill SEPARATOR"
%token INT(Int) PIG(Pig)
%rules


ListInt: List[Int] = list: (INT)... => List($list.length - 2) // this should not prevent the gap below being a separator

ListPig: List[Pig] = list: (PIG)... => List($list.length - 2)
""")

object ErrConflict extends Test.COMPONENTS("-Lsyn -Lsym -html -c")("""
                                %token a
                                %rules
                                S = A
                                  | B;
                                A = a;
                                B = a;
""")

object SloshErr extends Test.COMPONENTS("-Lsyn -Lsym -html -c")("""
                                %token a b `\/`
                                %rules
                                S = A `\/` B

                                A = a;
                                B = a;
""")


object DanglingElseLR extends Test.COMPONENTS("-c -html")(
"""  %path  "danglingelselr"
     %notation If
     %tables lr
     %token IF THEN ELSE ID '+'

     %rules

     expr: List[Any] = ID                                     { List("ID") }
                     | expr '+' ID                            { List($expr, "+", "ID") }
                     | IF g: expr THEN l: expr                { List("IF", $g, $l, Nil) }
                     | IF g: expr THEN l: expr                { List("IF", $g, $l, Nil) }
                     | IF g: expr THEN l: expr ELSE r: expr   { List("IF", $g, $l, $r) }

""")

object DanglingElseLALR extends Test.COMPONENTS("-c -html -Lsyn -Lsym -Lred")(
  """%path  "danglingelselalr"
     %notation If
     %tables   lalr
     %token IF THEN ELSE ID(String) '+'
     /* This demonstrates pattern generation contexts,
        the merging of identical states by Bison,
        and that lalr yields fewer conflicts than lr.
     */

     %rules

      expr: String = ID
        | l:expr '+' r:ID       => $r
        | expr '+' r: ID        => $r
        | l:expr '+' ID         => $ID
        | expr '+' ID           => $ID
        | IF guard: expr THEN expr => $guard
        | IF guard: expr x:THEN expr ELSE expr => $guard

""")

object TwoIncludes extends Test.COMPONENTS("-c -html -Lsyn -Lsym -Lred")(
  """
                                   %token a
                                   %rules
                                   %include { this is the first include }

                                   S = A | B

                                   A = a

                                   B = a

                                   %include { this is the second include };

  """.stripMargin
)







