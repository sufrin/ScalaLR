package org.sufrin.scalalr
package stage2

/**
 * Feature tests: that include deliberate errors
 */

object Err1 extends TestLR("-Lsyn -Lsym -html")(
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

object Err2 extends TestLR("-Lsyn -Lsym -html")(
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

object Err3 extends TestLR("-Lsyn -Lsym -html")(
  """
// Undollared rule result
%notation  err3
%package   scalalr.err3
%path      "err3"
%signature "err3 -- undollared parameter in result"
%%token INT(Int)
%rules

ListInt: List[Int] = list: (INT) ... => list
""")


object Err4 extends TestLR("-Lsyn -Lsym -html")(
  """
// Unknown scala operator in result"
%notation  err4
%package   scalalr.err4
%path      "err4"
%signature "err4 -- unknown scala operator in result"
%token INT(Int)
%rules

ListInt: List[Int] = list: (INT) ... => List($list.length * 2)
""")

object ErrConflict extends TestLR("-Lsyn -Lsym -html -c")("""
                                %token a
                                %rules
                                S = A
                                  | B;
                                A = a;
                                B = a;
""")


object DanglingElseLR extends TestLR("-c -html")(
"""  %path  "danglingelselr"
     %notation If
     %tables lr
     %token IF THEN ELSE ID '+'

     %rules

     expr: List[Any] = ID                                     { List("ID") }
                     | expr '+' ID                            { List($expr, "+", "ID") }
                     | IF g: expr THEN l: expr                { List("IF", $g, $l, Nil) }
                     | IF g: expr THEN l: expr ELSE r: expr   { List("IF", $g, $l, $r) }

""")

object DanglingElseLALR extends TestLR("-c -html")(
  """%path  "danglingelselalr"
     %notation If
     %tables   lalr
     %token IF THEN ELSE ID '+'

     %rules

      expr = ID
        | expr '+' ID
        | IF expr THEN expr
        | IF expr THEN expr ELSE expr

""")







