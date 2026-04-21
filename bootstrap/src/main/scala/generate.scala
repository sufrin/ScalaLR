package org.sufrin.scalalr

object generate {
  def main(args: Array[String]): Unit = {
    import Notation.{Lexical, Syntax}
    import Syntax.Parser
    import org.sufrin.utility._
    import java.nio.file.Path
    for  { arg <- args } {
      println(s"scalalr notation from $arg")
      val scanner = Lexical.Scanner(SourceTextCursor(Path.of(arg)))
      val notation = Parser(scanner).parseNotation()
      val translation = Translation(notation, "generated")
      translation.makeFiles()
    }
  }
}
