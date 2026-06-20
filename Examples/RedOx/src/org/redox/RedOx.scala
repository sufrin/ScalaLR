package org.redox

/**
 * RedOx is a variant of Handel.
 *
 * As I write this (June 2026)  all its "compiler" (`exploreSource`) does is to parse and prettyprint a RedOx program.
 * Its main program is also set up to interpret flags set by `ParserTests.Test` , namely `-#`, `##`,
 * and `-literally`. Together these provide support for an IDE (I use IntelliJ) to turn around
 * small tests of the notation quickly.
 */
object RedOx {

  import org.redox.Scanner._
  import org.sufrin.scalalr.LRParser._
  import org.sufrin.scalalr._
  import org.sufrin.utility.SourceTextCursor

  import java.nio.file.Paths

  var startLineNumber = 0
  var startColNumber = 0
  var logParse = false

  def exploreSource(cursor: SourceTextCursor): Unit = {
    val scanner = makeScanner(cursor)
    val parser  = LRParser.Pull[Token](Components)(scanner.sourceLocation)
    parser.logState = logParse
    try {
      parser.run(scanner.next) match {
        case ACCEPTED(values) =>
          import org.sufrin.utility.PrettyPrint._
          values.prettyPrint()
          values match {
            case ast: AST => println(s"$ast (${ast.start} ...  ${ast.end})")
            case other => println(other)
          }
        case ERRONEOUS(diagnosis) =>
          println(diagnosis)
        case _ => 
      }
    } catch {
      case err: java.lang.Error => println(err)
    }
  }

  def main(args: Array[String]): Unit = {
    import org.redox.Components
    import org.redox.Scanner._
    import org.sufrin.utility._

    var arguments = args.toList
    def nextArgument(): String = { // pre arguments.nonEmpty; post arguments = old(arguments).tail; returns arguments.head
      val arg = arguments.head
      arguments = arguments.tail
      arg
    }

    while (arguments.nonEmpty) {
      val arg = nextArgument()
      if (arg.isEmpty) {}
      else if (arg == "-#" && arguments.nonEmpty)  { startLineNumber = nextArgument().toInt }
      else if (arg == "-##" && arguments.nonEmpty) { startColNumber = nextArgument().toInt }
      else if (arg == "-literally" && arguments.nonEmpty)
           exploreSource(SourceTextCursor(nextArgument().iterator).withStartLocation(startLineNumber, startColNumber))
      else if (arg == "-log")      logParse = true
      else if (arg startsWith "-") println(s"No such flag: $arg")
      else try exploreSource(SourceTextCursor(Paths.get(arg)))
      catch { case exn: Throwable => exn.printStackTrace() }
    }
  }
}


