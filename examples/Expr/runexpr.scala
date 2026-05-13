//> using scala 2.13
//> using jar ../../shared/target/shared-0.8.0.jar
//> using jar ../../utilities/target/utilities-0.8.0.jar
//> using jar ../../logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0
//> using options -deprecation -feature -unchecked

package org.sufrin.scalalr


object runexpr  {
  import expr.Expr.Components
  import expr.Expr.Scanner._
  import org.sufrin.utility.PrettyPrint._
  import org.sufrin.utility._

  def main(args: Array[String]): Unit = {


    

    if (true) {
      println("-------PUSH PARSER-------")
      val scanner  = Scanner(SourceTextCursor("a; a * (c + b) + c"))
      val parser   = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = false
      while (state == LRParser.NEXTSTEP) {
        state = parser.step(scanner.next())
      }
      println(state)
    }

    if (true) {
      println("-------PUSH PARSER-------")
      val scanner = Scanner(SourceTextCursor(
        """a ;
          |(a+b) ; c+d; e+f""".stripMargin))
      val parser = LRParser.Push[Token](Components)(scanner.sourceLocation)
      var state = parser.start()
      parser.logState = false
      while (state == LRParser.NEXTSTEP) {
        state = parser.step(scanner.next())
      }
      println(state)
    }


    val moreSource =
        """a*b+c*d*
        |  (e+f)*
        |  [g+h]""".stripMargin
        
    if (true) {
      println("-------PULL PARSER-------")
      val source = moreSource
      val scanner = Scanner(SourceTextCursor(source))

      val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
      parser.run(scanner.next).prettyPrint()
    }

  }
}

