//> using scala "2.13"
//> using jar "Scalalr.jar"

package org.sufrin.scalalr


object runexpr  {
  import expr.Expr.Components
  import expr.Expr.Scanner._
  import org.sufrin.utility.PrettyPrint._
  import org.sufrin.utility._

  def main(args: Array[String]): Unit = {


    if (true) {
      println("PULL")
      val source = """a; a+b; a*b+c*d*(e+f)*[g+h]; p+q*r"""
      val scanner = Scanner(SourceTextCursor(source))

      def next(): Token = if (scanner.hasNext) scanner.next() else $end

      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      println(parser.run(next))
    }


    if (true) {
      println("PUSH")
      val scanner  = Scanner(SourceTextCursor("a; a * (c + b) + c"))
      val parser   = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = false
      while (state == LRParser.NEXTSTEP) {
        val input = if (scanner.hasNext) scanner.next() else $end
        state = parser.step(input)
      }
      println(state)
    }

    if (true) {
      println("PUSH")
      val scanner = Scanner(SourceTextCursor(
        """a ;
          |(a+b) ; c+d; e+f""".stripMargin))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = false
      while (state == LRParser.NEXTSTEP) {
        val input = if (scanner.hasNext) scanner.next() else $end
        state = parser.step(input)
      }
      println(state)
    }

    if (true) {
      println("PARSETREE")
      val scanner = Scanner(SourceTextCursor(
        """
          |(a+b)*(c+d)""".stripMargin))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = false
      while (state == LRParser.NEXTSTEP) {
        val input = if (scanner.hasNext) scanner.next() else $end
        state = parser.step(input, scanner.sourceLocation())
      }
      prettyPrint(state)
    }
  }
}

