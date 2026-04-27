//> using scala "2.13"
//> using jar "scalalr.jar"

import org.sufrin.utility.SourceTextCursor

object runtinyfun  {
  import org.sufrin.scalalr._
  import tinyfun.Components
  import tinyfun.Scanner._
  import tinyfun.TinyFun._

  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {
    val log  = args.contains("-l")
    val pull = !args.contains("-push")
    val file = (args.toList.filterNot(_.startsWith("-")) ++ List("/dev/tty")).head

    print("Welcome to TinyFun\n> ")

    while (pull) {
      val scanner = Scanner(SourceTextCursor(Paths.get(file)))
      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.logState = log
      try parser.run(scanner.next) catch {
        case err: java.lang.Error => println(err)
      }
    }

    while (!pull) {
      val scanner = Scanner(SourceTextCursor(Paths.get(file)))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = log
      try {
        while (state == LRParser.NEXTSTEP) {
          val input = scanner.next()
          state = parser.step(input, scanner.sourceLocation())
          if (log) System.out.println(parser.mkString)
          System.out.flush()
        }
      }
      catch {
        case err: java.lang.Error => println(err)
      }
    }
  }
}

