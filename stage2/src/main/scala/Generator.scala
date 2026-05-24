/**
 * Manages code generation for the Stage 2 language
 *
 * Parser:     built by stage1 or stage2
 * Tree:       stage2.AST
 * Generator:  stage2.AST => Scala
 *
 *
 *
 */


package org.sufrin.scalalr
package stage2

import org.sufrin.utility.SourceTextCursor

object Generator extends org.sufrin.logging.SourceLoggable {
  locally { level = org.sufrin.logging.INFO }
  import AST._
  import org.sufrin.utility.PrettyPrint._

  import java.nio.file.Paths

  var pretty: Boolean = false
  var prefix: String  = "generated"
  var bisonCounterexamples: Boolean = false
  var bisonHtml: Boolean = false
  var logParse: Boolean = false
  var logGeneration: List[String] = Nil
  var roseTree: Boolean = false

  @inline def bisonTokenToInt(token: String): Int   = token match {
    case s"T-$num"  => num.toInt
    case "$default" => Int.MinValue
    case "$end"     => 0
    case "error"    => 1
  }
  @inline def numberToBisonToken(num: Int): String  = num match {
    case 0 => "$end"
    case 1 => "error"
    case _ => f"T-${(num)}%03d"
  }

  def warning(what: String): Unit = println(s"WARNING: $what")


  def generateCode(notation: Notation): Unit = {
    if (pretty) notation.prettyPrint()
    val symbolTables = new SymbolTables(notation)
    val generator    = new CodeGenerator(notation, symbolTables)
    if (symbolTables.sanityCheck()) generator.generateScalaFiles() else println(s"${symbolTables.fatalErrors} (*) warnings -- no code generation")
  }

  def processScalaLR(cursor: SourceTextCursor): Unit = {
    import org.sufrin.scalalr.LRParser._
    import scalalr.stage2.{Components, Scanner}
    val scanner = LexicalScanner(cursor)
    val parser  = LRParser.Pull[LexicalScanner.Token](Components)(scanner.sourceLocation)
    parser.logState = logParse
    parser.run(scanner.next) match {
      case ACCEPTED(notation: Notation) => generateCode(Normalization.normalize(notation))
      case ERRONEOUS(why) => error(why)
      case other =>
    }
  }


  def main(args: Array[String]): Unit = {
    import org.sufrin.utility._
    import scalalr.stage2.{Components, Scanner}
    var arguments = args.toList
    def nextArgument(): String = { // pre arguments.nonEmpty; post arguments = old(arguments).tail; returns arguments.head
      val arg = arguments.head
      arguments = arguments.tail
      arg
    }
    var startLineNumber = 1
    var startColNumber = 0

    while (arguments.nonEmpty) {
      val arg = nextArgument()
      if      (arg.startsWith("--prefix=")) prefix = arg.replace("--prefix=", "")
      else if (arg.startsWith("--output=")) prefix = arg.replace("--output=", "")
      else if (arg == "-p" && arguments.nonEmpty) prefix = nextArgument()
      else if (arg == "-o" && arguments.nonEmpty) prefix = nextArgument()
      else if (arg == "-#" && arguments.nonEmpty)  { startLineNumber = nextArgument().toInt }
      else if (arg == "-##" && arguments.nonEmpty) { startColNumber = nextArgument().toInt }
      else if (arg == "-s" && arguments.nonEmpty) processScalaLR(SourceTextCursor(nextArgument().iterator).withStartLocation(startLineNumber, startColNumber))
      else if (arg == "-log")               logParse = true
      else if (arg == "-rose")              roseTree = true
      else if (arg.startsWith("-L"))        logGeneration ::= arg.replace("-L", "")
      else if (arg == "-pp")                pretty = true
      else if (arg == "-html")              bisonHtml = true
      else if (arg == "-c")                 bisonCounterexamples = true
      else if (arg.startsWith("-")) {
        println(
          """Usage: scalalr OPTION ... PATH ...
            |Treat (each) PATH as the  path in the filestore to scalalr SOURCE  and generate the
            |scala files corresponding to the %notation it defines.
            |
            |Place the generated files under the directory named by OUTPUTPATH
            |catenated with the %path (if any) declared in the scalalr source.
            |The default OUTPUTPATH is "./generated".
            |
            |OPTIONS:
            |-pp        prettyprint only
            |-log       log the input source parse
            |-html      output grammar report in html form
            |-c         generate detailed conflict report
            |-rose      generate a RoseTreeReduction.reduction
            |
            |LOGGING OPTIONS
            |-Lsym      show an inventory of the symbols, their types, and their definitions
            |-Lsyn      show the rules after code synthesis for repeated constructions
            |
            |OUTPUTPATH is set by one of
            |-p         OUTPUTPATH
            |-o         OUTPUTPATH
            |--output=OUTPUTPATH
            |--prefix=OUTPUTPATH
            |
            |LITERAL SOURCE (reserved for programmatic testing) a notation may be defined directly in an argument
            |-#         INT     first SOURCE line number
            |-##        INT     first SOURCE column number
            |-s         SOURCE
            |""".stripMargin)
        System.exit(0)
      }
      else if (arg.isEmpty) {}
      else try { processScalaLR(SourceTextCursor(Paths.get(arg))) } catch { case exn: Throwable => exn.printStackTrace() }
    }
  }
}

