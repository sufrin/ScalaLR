

  //> using scala "2.13"
  //> using jar "scalalr.jar"

  //
  // From testbed/scala/small: scala-cli runsmall.scala small

package org.sufrin.scalalr


object runsmall  {
  import org.sufrin.utility._
  import PrettyPrint._
  import small.Small.Reduction._
  import small.Small.Scanner._
  import small.Small.Tables._


  def main(args: Array[String]): Unit = {
    val source = """a;b;c;
    d;e;f;g"""
    if (true) {
      val scanner: Scanner = Scanner(SourceTextCursor(source))

      def next(): Token = if (scanner.hasNext) scanner.next() else $end

      val parser = new LRParser.Pull[Token](ACTIONTABLE, GOTOTABLE, reduction, symbolName, scanner.sourceLocation)
      parser.run(next).prettyPrint()
    }

    if (true) {
      val scanner = Scanner(SourceTextCursor(source))
      val parser = new LRParser.Push[Token](ACTIONTABLE, GOTOTABLE, reduction, symbolName, scanner.sourceLocation)
      parser.logState = true
      var state = parser.start()
      while (state == LRParser.NEXTSTEP) {
        val input = if (scanner.hasNext) scanner.next() else $end
        state = parser.step(input)
      }
      state.prettyPrint()
    }
  }
}

