//> using scala "2.13"
//> using jar "scalalr.jar"

/*
        First construct the lr.jar in the ScalaLalr/testbed/ directory
        I use IntelliJ to build it as an artefact in ScalaLalr/out/artifacts [sic]
        then symbolically link that to ScalaLalr/testbed/. You can also do this
        with scala "package" from ScalaLalr/, but then you'll have rename the
        resulting jar appropriately.

        CODE GENERATION:
                scala -cp ScalaLalr.jar org.sufrin.scalalr.ScalaLalr fun.l
        COMPILATION AND RUNNING
                 scala-cli run fun.scala funsem.scala fun --
*/



object generator  {
  import org.sufrin.scalalr._
  import org.sufrin.utility.PrettyPrint._
  import org.sufrin.utility.SourceTextCursor
  import scalalr.parser.ScalaLR.Reduction._
  import scalalr.parser.ScalaLR.Scanner._
  import scalalr.parser.ScalaLR.Tables._

  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {
    val log  = args.contains("-l")
    val pull = args.contains("-p")
    val file = (args.toList.filterNot(_.startsWith("-")) ++ List("/dev/tty")).head

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
  generator.main(Array("-p", "scalalr/src/test/tinyfun.scalalr"))
}








