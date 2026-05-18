//> using scala 2.13
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0
//> using options -deprecation -feature -unchecked


object runsmall  {
  import org.sufrin.scalalr.LRParser
  import org.sufrin.utility._
  import PrettyPrint._
  import small.Small.Components
  import small.Small.Scanner._


  def main(args: Array[String]): Unit = {
    val source = """a;b;c;
    d;e;f;g"""
    if (true) {
      println("-------\nPull parser: showing each transition\n-------")
      val scanner: Scanner = Scanner(SourceTextCursor(source))
      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.logState = true
      parser.run(scanner.next).prettyPrint()
    }

    if (true) {
      println("-------\nPush parser: showing internals at each transition\n-------")
      val scanner: Scanner = Scanner(SourceTextCursor(source))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
      parser.logState = true
      var state = parser.start()
      while (state == LRParser.NEXTSTEP) {
        state = parser.step(scanner.next())
        println(parser.mkString)
      }
      state.prettyPrint()
    }
  }
}

