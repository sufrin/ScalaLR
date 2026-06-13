
package org.redox


import org.sufrin.SourceLocation._

object test0 extends Test("")("""-proc x:=3 /*fo/* nested*/*/""".stripMargin)
object test0a extends Test("")("""-proc x:=3 /*fo/* nested*/""")
object test1 extends Test("")("""-proc x=0+0+0""")
object test1a extends Test("")("-proc {xx123:=yy123>z123}")
object test1b extends Test("")("""-proc
  x := Phun(3,4) + Bun()
""")

object test1c extends Test("")("""-proc
     seq{
        x := (T <| Phun(3,4) |>  Bun());
        y := (L1 <| guard1 |> RL1 <| guard2 |> RR1)
     }
""")

object test1d extends Test("")("""-proc
      seq{
        x := (T <| Phun(3,4) |>  Bun());
        y := (L1 <| guard1 |> RL1 <| guard2 |> RR1);
        p, q := q, r
       }
""")

object test2 extends Test("")("""-proc x := 3  || y := x || z:=y """)
object test2a extends Test("")("""-proc seq { x := 3 || y := x || z:=y }""")

object test3 extends Test("")("""-proc { x := 3+4+5  || y:=42 ;  y := x  ; z:=y ; stop } """)
object test4 extends Test("")(
  """-proc
     // Two's company
    { x := 3+4+5  || y:=42 ;
      /* three is a crowd */
      y := x  ; z:=y+0xff ;
      stop
    }
  """)
object testalt extends Test("")(
  """-proc
    alt {
      (watchingInput) input ? x   -> a := x ;
      (watchingReady) ready ? ANY -> out ! a ;
      (dreaming)      obfuscate ? x ->  { x:= x+1[1]; y := RAM[x] }
    }
  """)

object testLocal extends Test("")(
  """-proc
    alt {
      (watchingInput) input ? x   -> var { int x[2]; int y[3] } a := x  ;
      (watchingReady) ready ? ANY -> out ! a ;
      (dreaming)      obfuscate ? x ->  { x:= x+1[1]; y := RAM[x] };
      (swapping)      l?x -> var { int t[4] } { r?t; out!t; out!x }
    }
  """)

object testif extends Test("")(
  """-proc
    if {
      a==b -> a := x ;
      c==d -> out ! a ;
      e==f -> {x:= x+1[1]; y := RAM[x] };
      _    -> stop
    }
  """)

///////////////////////////////////////////////////////////

/**
 * Simple tests of parser components. Prefix indicates the nonterminal
 * -proc  ... text of a process ...
 * -expr  ... text of an expr ...
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

