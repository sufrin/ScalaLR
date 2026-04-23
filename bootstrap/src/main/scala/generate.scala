package org.sufrin.scalalr

object generate {
  def main(args: Array[String]): Unit = {
    import Notation.{Lexical, Syntax}
    import Syntax.Parser
    import org.sufrin.utility._
    import java.nio.file.Path
    var output = "generated"
    for  { arg <- args } if (arg.startsWith("--output=")) {
      output = arg.replace("--output=", "")
    }
    else if (arg.startsWith("-")) {
      println(
        """Usage: org.sufrin.scalalr.generate [--output=<outputpath] [<file> ...]
          |Treat each <file> as a scalalr source files and generate the
          |scala files corresponding to the %notation it defines.
          |Place the generated files under the directory named by <outputpath>
          |catenated with the %path (if any) declared in the scalalr source.
          |The default outputpath is "./generated".
          |""".stripMargin)
          System.exit(0)
    }
    else
    {
      val scanner = Lexical.Scanner(SourceTextCursor(Path.of(arg)))
      val notation = Parser(scanner).parseNotation()
      val translation = Translation(notation, output)
      translation.makeFiles()
    }
  }
}
