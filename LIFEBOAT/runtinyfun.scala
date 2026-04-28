
//> using scala 2.13
//> using jar ../bootstrap/target/bootstrap-0.8.0.jar
//> using jar ../shared/target/shared-0.8.0.jar
//> using jar ../utilities/target/utilities-0.8.0.jar
//> using jar ../logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0

import org.sufrin.utility.SourceTextCursor

object runtinyfun  {

  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {
    import org.sufrin.scalalr._
    import tinyfun.Components
    import tinyfun.Scanner._

    val log  = args.contains("-l")
    val file = (args.toList.filterNot(_.startsWith("-")) ++ List("/dev/tty")).head

    print("Welcome to TinyFun\n> ")

      val scanner = Scanner(SourceTextCursor(Paths.get(file)))
      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.logState = log
      try parser.run(scanner.next) catch {
        case err: java.lang.Error => println(err)
      }
  }
}

