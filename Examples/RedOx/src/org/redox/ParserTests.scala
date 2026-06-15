
package org.redox


import org.sufrin.SourceLocation._

object test0 extends Test("")("""-PROC x:=3 /*fo/* nested*/*/""".stripMargin)
object test0a extends Test("")("""-PROC x:=3 /*fo/* nested*/""")
object test1 extends Test("")("""-PROC x=0+0+0""")
object test1a extends Test("")("-PROC {xx123:=yy123>z123}")
object test1b extends Test("")("""-PROC
  x := Phun(3,4) + Bun()
""")

object test1c extends Test("")("""-PROC
     SEQ{
        x := (g1 -> c1 | g2 -> c2 | _ -> c3);
        y := (g1 -> c1 | g2 -> c2 | c3);
        x, y, z := y, z, x
     }
""")

object test2 extends Test("")("""-EXPR x == 3  || y == x || z==y """)
object test2a extends Test("")("""-PROC SEQ { x := 3 ; y := x ; z:=y }""")

object test3 extends Test("")("""-PROC PAR { x := 3+4+5 ;  y:=42 ;  y := x  ; z:=y ; STOP } """)
object test4 extends Test("")(
  """-PROC
     // Two's company
    { PAR{ x := 3+4+5 ; y:=42 };
      /* three is a crowd */
      y := x  ; z:=y+0xff ;
      STOP
    }
  """)
object testalt extends Test("")(
  """-PROC
    ALT {
      (watchingInput) input ? x   -> a := x ;
      (watchingReady) ready ? ANY -> out ! a ;
      (dreaming)      obfuscate ? x ->  { x:= x+1[1]; y := RAM[x] }
    }
  """)

object testLocal extends Test("")(
  """-PROC
    ALT {
      (watchingInput) input ? x   -> DECLARE { INT x[2]; INT y[3] } a := x  ;
      (watchingReady) ready ? ANY -> out ! a ;
      (dreaming)      obfuscate ? x ->  { x:= x+1[1]; y := RAM[x] };
      (swapping)      l?x -> DECLARE { INT t[4] } { r?t; out!t; out!x }
    }
  """)

object testif extends Test("")(
  """-PROC
    IF {
      a==b -> a := x ;
      c==d -> out ! a ;
      e==f -> {x:= x+1[1]; y := RAM[x] };
      _    -> STOP
    }
  """)

object  testNEW extends Test("")(
  """-PROC
    DECLARE {
      INT x[1] := 0
    }
    STOP
  """)

///////////////////////////////////////////////////////////

/**
 * Simple tests of parser components. Prefix indicates the nonterminal
 * -PROC  ... text of a process ...
 * -EXPR  ... text of an expr ...
 * otherwise ... text of a complete program (ie var { declarations } ....
 *
 *
 */
class Test(args: String="")(source: String)(implicit loc: SourceLocation) extends App {
  // calculate starting location of source string in this file
  val line   = loc.line-1
  val offset = (loc.offset+s"Test($args) (".length)
  val effectiveArgs = args.split(' ').dropWhile(_.isEmpty).toList ++
                      List("-#", line.toString, "-##", offset.toString, "-literally", source)
  println(s"$loc ${effectiveArgs.mkString("RedOx ", " ", "")}")
  RedOx.main(effectiveArgs.toArray)
}

