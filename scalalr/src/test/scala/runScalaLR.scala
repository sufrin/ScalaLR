//> using scala "2.13"
//> using jar "scalalr.jar"

/*
        The is the driver for the post-bootstrap runScalaLR
*/



object runScalaLR  {
  import org.sufrin.scalalr._
  import org.sufrin.utility.PrettyPrint._
  import org.sufrin.utility.SourceTextCursor
  import scalalr.parser.ScalaLR.Reduction._
  import scalalr.parser.ScalaLR.Scanner._
  import scalalr.parser.ScalaLR.Tables._

  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {
    val log  = args.contains("-l")
    val pull = !args.contains("-push")
    val file = (args.toList.filterNot(_.startsWith("-")) ++ List("/dev/tty")).head
    println(s"runScalaLR generating code for $file")

    if (pull) {
      val scanner = Scanner(SourceTextCursor(Paths.get(file)))
      def next(): Token = if (scanner.hasNext) scanner.next() else $end
      val parser = new LRParser.Pull[Token](ACTIONTABLE, GOTOTABLE, reduction, symbolName, scanner.sourceLocation)
      parser.logState = log
      parser.run(next)
    }

    if (!pull) {
      val scanner = Scanner(SourceTextCursor(Paths.get(file)))
      val parser = new LRParser.Push[Token](ACTIONTABLE, GOTOTABLE, reduction, symbolName, scanner.sourceLocation)
      var state = parser.start()
      parser.logState = log
      while (state == LRParser.NEXTSTEP) {
        val input = if (scanner.hasNext) scanner.next() else $end
        state = parser.step(input, scanner.sourceLocation())
        if (log) System.out.println(parser.mkString)
        System.out.flush()
      }
    }
  }
}

object generateTinyFun extends App {
  runScalaLR.main(Array("scalalr/src/test/tinyfun.scalalr"))
}












