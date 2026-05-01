/**
 * Stage 2 language
 *
 * Parser:     from slab-notation.scalalr
 * Tree:       slab.AST
 * Generator:  slab.AST => Scala
 */

package org.sufrin.scalalr
package stage2


object Generator {
  import java.nio.file.Paths
  import org.sufrin.utility.PrettyPrint._

  var pretty: Boolean = true
  var output: String = "generated"

  def translate(notation: AST.Notation): Unit = {
    if (pretty) notation.prettyPrint()
    else {
      //val translation = bootstrap.Generator(notation.toBootstrapNotation, output)
      //translation.makeFiles()
    }
  }

  def main(args: Array[String]): Unit = {
    import org.sufrin.utility._
    import scalalr.stage2.{Components, Scanner}

    var log = false
    for  { arg <- args } if (arg.startsWith("--output=")) {
      output = arg.replace("--output=", "")
    }
    else if (arg == "-log") log = true
    else if (arg == "-p") pretty = true
    else if (arg.startsWith("-")) {
      println(
        """Usage: stage2 [--output=<outputpath] [-p | -l]* [<file> ...]
          |Treat each <file> as a scalalr source files and generate the
          |scala files corresponding to the %notation it defines.
          |Place the generated files under the directory named by <outputpath>
          |catenated with the %path (if any) declared in the scalalr source.
          |The default outputpath is "./generated".
          |
          |-p prettyprint only
          |-l log the input source parse
          |""".stripMargin)
      System.exit(0)
    }
    else
    {
      val scanner = Scanner(SourceTextCursor(Paths.get(arg)))
      val parser = LRParser.Pull[Scanner.Token](Components)(scanner.sourceLocation)
      parser.logState = log
      parser.run(scanner.next)
    }
  }
}

