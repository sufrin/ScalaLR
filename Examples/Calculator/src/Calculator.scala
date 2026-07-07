

object Calculator  {

  import java.nio.file.Path
  import org.sufrin.scalalr._
  import org.sufrin.utility._

  import calculator.Calc.Components
  import calculator.Calc.Scanner._

  var log = false
  var push = false
  var recover = false

  def main(args: Array[String]): Unit = {


    if (args.isEmpty) inputFrom("/dev/console") else
    for {arg <- args} arg match {
      case "-l" => log = true
      case "-p" => push = true
      case "-r" => recover = true
      case "-h" =>
        println(
          """Usage: runtinyfun [flags]
            |  -l log the parse
            |  -r attempt parser recovery
            |  -p use the "push" parser automaton
            |""".stripMargin)
        scala.sys.exit()

      case "-"   => inputFrom("/dev/console")
      case _ if arg startsWith("-") =>
      case _ => inputFrom(arg)
    }

  }

  def inputFrom(path: String): Unit = {
      val interactive = path=="/dev/console"
      println(s"Calculator: $path")
      if (push) {
        import LRParser._
        var state: ParseState = RUNNING
        val scanner = makeScanner(SourceTextCursor(Path.of(path)))
        while (state == RUNNING) {
          val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
          parser.logState = log
          parser.attemptRecovery = recover
          state = parser.start()
          while (state == LRParser.NEXTSTEP) {
            state = parser.step(scanner.next())
          }
          state match {
            case ERRONEOUS(message) =>
              println(message)
              if (path=="/dev/console") state = RUNNING
            case ACCEPTED(message) =>
              message match {
                case List("syntax", newSym: String, oldSym: calculator.syntax.Named) =>
                  calculator.Calc.Scanner.Extensibility.extend(scanner, newSym, oldSym.name)
                  state = RUNNING
                case _ =>
                  println(message)
              }
            case _ =>
              state = RUNNING
          }
        }
      } else {
      import LRParser._
      var state:   ParseState = RUNNING
      var prompts: Int        = 0
      def prompt: String      = { prompts += 1; f"${prompts}%3d: "}
      val input               = SourceTextCursor(Path.of(path))
      if (interactive) {
        input.withPrompt(prompt)
      }
      val scanner = makeScanner(input)
      while (state == RUNNING) {
        val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)

        parser.logState         = log
        parser.attemptRecovery  = recover
        parser.logRecovery      = recover
        parser.reductionOnError = true

        state = parser.run(scanner.next)

        state match {
          case ERRONEOUS(message) =>
            println(message)
            if (interactive) state = RUNNING
          case INSTRUCTED(message) =>
            message match {
              case List("syntax", newSym: String, oldSym: calculator.syntax.Named) =>
                   calculator.Calc.Scanner.Extensibility.extend(scanner, newSym, oldSym.name)
                   state = RUNNING
              case _ =>
                   state = RUNNING
            }
          case ACCEPTED(()) =>
            if (path=="/dev/console") println("Bye")
          case _ =>
            state = RUNNING
        }
      }
    }
  }
}

