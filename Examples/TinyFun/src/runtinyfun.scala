
//> using scala 2.13
//> using dep org.scala-lang.modules::scala-xml::2.4.0
//> using jar ROOT/scalalrruntime.jar

/* WAS
//> using jar ROOT/bootstrap/target/bootstrap-0.8.0.jar
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar
*/

/**
 *  Top level read-eval-print interpreter for TinyFun.
 *
 *  Notice that the top-level parser(s) are reinitialised when a parse error is
 *  encountered. This is the simplext way of recovering from such errors without
 *  using the (immature) error-recovery features of the parser.
 *
 */

object runtinyfun  {

  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {
    import org.sufrin.scalalr._
    import org.sufrin.utility._

    import tinyfun.Components
    import tinyfun.Scanner._

    val log = args.contains("-l")
    val push = args.contains("-p")
    val recover = args.contains("-r")
    val file = (args.toList.filterNot(_.startsWith("-")) ++ List("/dev/console")).head
    if (args.contains("-h")) {
      println(
        """Usage: runtinyfun [flags]
          |  -l log the parse
          |  -r attempt parser recovery
          |  -p use the "push" parser automaton
          |""".stripMargin)
      scala.sys.exit()
    }

    println("TinyFun")
    if (push) {
      import LRParser._
      var state: ParseState = RUNNING
      while (state == RUNNING) {
        val scanner = makeScanner(SourceTextCursor(file))
        val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
        parser.logState = log
        parser.attemptRecovery = recover
        state = parser.start()
        scanner.prompt()
        while (state == LRParser.NEXTSTEP) {
          //println(parser.mkString)
          state = parser.step(scanner.next())
        }
        state match {
          case ERRONEOUS(message) =>
            println(message)
            state = RUNNING
          case st: ACCEPTED =>
          case _ => state = RUNNING
        }
      }
    } else {
      import LRParser._
      var state: ParseState = RUNNING
      while (state == RUNNING) {
        val scanner = makeScanner(SourceTextCursor(Paths.get(file)))
        val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
        parser.logState = log
        parser.attemptRecovery = recover
        scanner.prompt()
        state = parser.run(scanner.next)
        state match {
          case ERRONEOUS(message) =>
            println(message)
            state = RUNNING
          case st: ACCEPTED =>
          case _ =>
            state = RUNNING
        }
      }
    }
  }
}

