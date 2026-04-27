


package org.sufrin.scalalr


object runsmall  {
  import org.sufrin.utility._
  import PrettyPrint._
  import small.Small.Components
  import small.Small.Scanner._


  def main(args: Array[String]): Unit = {
    val source = """a;b;c;
    d;e;f;g"""
    if (true) {
      val scanner: Scanner = Scanner(SourceTextCursor(source))

      //def next() = if (scanner.hasNext) scanner.next() else $end

      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.run(scanner.next).prettyPrint()
    }

    if (true) {
      val scanner: Scanner = Scanner(SourceTextCursor(source))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
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

