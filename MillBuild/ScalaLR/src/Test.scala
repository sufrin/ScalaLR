package org.sufrin.scalalr
package stage2

/**
 * Support for testing ScalaLR.
 *
 * All the classes herein generate parser components from source provided as
 * (long) strings, using the ScalaLR generator with arguments provided.
 *
 * also generate programs that /use/ the components in parsers, and
 * can be started from IntelliJ and/or the command line.
 */
object Test {

  import org.sufrin.SourceLocation._
  import org.sufrin.utility._

  /**
   * An App subclass to simplify construction of `scalalr` tests of notation processing. For example:
   * {{{
   *   object test1 extends TestLR("")("""
   *    %tables    ielr
   *    %notation  stage2test
   *    %package   scalalr.stage2test
   *    %path      "parser"
   *
   *    %rules
   *    Rule1: Unit = S1 S2 S3 { Code };
   *    Rule2: Unit = a: S4 b: S5 { () }
   * """)
   *
   * The source of the resulting parser components (if any) is placed
   * in `Testing/generated`, and this directory may be deleted
   * at any time. Indeed it MUST be deleted (or excluded from processing some other way)
   * if for any reason it turns out not to be compileable.
   *
   * }}}
   *
   * @param args            (command-line) arguments to be supplied to the ScalaLR generator
   * @param source          text of the source: this should be a """long string""" starting on the line after the subclass definition.
   * @param loc             IMPLICIT location of the Test in Scala source (to get scalalr error reports correct)
   * @see ErrorReportTests for longer examples.
   */

  class COMPONENTS(args: String = "")(source: String)(implicit loc: SourceLocation = sourcePath) extends
        Generate("--output=Testing/GeneratedByStaticTests/generated " + args)(source)(loc)


  /**
   * Workhorse generator for parser components described by the given `notation`.
   * One of the args MUST specify the destination directory for the components
   * otherwise the generator provides its own.
   *
   * @param args   (command-line) arguments to be supplied to the ScalaLR generator
   * @param source text of the source: this should be a """long string""" starting on the line after the class
   * @param loc
   */
  class Generate(args: String = "")(notationSource: String)(loc: SourceLocation) extends App {
    val effectiveArgs = args.split(' ').toList ++ List("-#", (loc.line + 1).toString, "-##", "0" /*(loc.offset).toString*/ , "-s", notationSource)
    println("**************************************")
    println(s"Generating parser components from  ${loc.file}:${loc.line} $args")
    println("**************************************")
    Generator.main(effectiveArgs.toArray)
  }


  /**
   * Same as `COMPONENTS`, except that  the source of the resulting parser components (if any) is placed
   * in `Testing/src/GeneratedByRuntTimeTests/generated`, and this directory may be deleted
   * at any time. Indeed, if you are using IntelliJ, it MUST be deleted (or excluded from processing some other way)
   * if for any reason it turns out not to be compileable: for otherwise IntelliJ
   *
   */
  class SOURCE(args: String = "")(notationSource: String)(implicit loc: SourceLocation = sourcePath) extends
    Generate("--output=Testing/src/GeneratedByRuntTimeTests/generated/ " + args)(notationSource)(loc)
    
  class SCALA(definedPackage: String)(scalaSource: String)(implicit loc: SourceLocation = sourcePath) extends App {
    CodeGenerator.writeToFile(s"Testing/src/GeneratedByRuntTimeTests/generated/$definedPackage.scala")(scalaSource)
  }

  class OBJECT(definedPackage: String)(scalaSource: String)(implicit loc: SourceLocation = sourcePath) extends App {
    CodeGenerator.writeToFile(s"Testing/src/GeneratedByRuntTimeTests/generated/$definedPackage.scala")(s"object $definedPackage {\n\n$scalaSource\n\n}\n")
  }

  
  /**
   * Build parser components for a notation, as well as a small test that runs a parser
   * based on these components, using the literal text `input` as the input to the parser, or
   * (if that is empty) using the terminal console.
   *
   * The test program and the generated scanner components are all placed "under"  `Testing/src/GeneratedByRuntTimeTests/generated`
   *
   * @param args           scalalr flags
   * @param definedPackage the name of the package the source notation defines
   * @param notationSource text of the notation description
   * @param testinput      literal text of the input to the test program, or "" if the input is to come from stdin or the console
   * @param loc            IMPLICIT location of the Test in Scala source  (to get scalalr error reports correct)
   */
  class RUN(args: String = "", definedPackage: String = "")(testinput: String = "")(notationSource: String)(implicit loc: SourceLocation = sourcePath) extends
    Generate("--output=Testing/src/GeneratedByRuntTimeTests/generated/ " + args)(notationSource)(loc.copy(line = loc.line + testinput.split('\n').length + 3)) {
    val testSourceText = if (testinput.isEmpty) "SourceTextCursor.console" else s"SourceTextCursor(\"\"\"$testinput\"\"\")"
    val testMain = definedPackage.replace(".", "-")
    val testPath = definedPackage.split('.')(0)

    def testProgram: String =
      s"""
         |//> using scala 2.13
         |//> using jar ROOT/ScalaLR/bin/scalalr.jar
         |//> using jar ROOT/Runtime/scalalrlibrary.jar
         |package ${definedPackage}
         |import org.sufrin.scalalr.stage2.Test.Runner
         |import org.sufrin.scalalr._
         |import org.sufrin.utility.SourceTextCursor
         |object run extends Runner [Scanner.Token] {
         |  val    components:  LRParserComponents = Components
         |  val    scanner:     Scanner[Scanner.Token] = Scanner($testSourceText)
         |}
         |""".stripMargin

    def testCommand: String =
      s"""cd Testing/src/GeneratedByRuntTimeTests/generated/$testPath/
         |ln -sF ~/GitHomes/ScalaLR/MillBuild ROOT
         |scala-cli run $testMain-run.scala $testPath
         |""".stripMargin

    CodeGenerator.writeToFile(s"Testing/src/GeneratedByRuntTimeTests/generated/$testMain.scala")(testProgram)
    CodeGenerator.writeToFile(s"Testing/src/GeneratedByRuntTimeTests/generated/$testMain.sh")(testCommand)
    println("**************************************")
    println(s"IntelliJ: Testing/src/GeneratedByRuntTimeTests/generated/$testMain.scala")
    println(s"Shell:    Testing/src/GeneratedByRuntTimeTests/src/generated/$testMain.sh")
    println("**************************************")
  }


  /**
   *  A (stereotyped) abstract runnable application, parameterised
   *  by a token type, generated `Components`, and a scanner.
   */
  trait Runner[Token <: Lexeme] {

    import org.sufrin.scalalr.{Scanner, ScannerCore, SourceLocation}

    val components: LRParserComponents
    def scanner:    Scanner[Token]

    val logState: Boolean = false
    val attemptRecovery: Boolean = false

    import org.sufrin.scalalr._
    import org.sufrin.utility.PrettyPrint._
    import org.sufrin.utility._

    import java.nio.file.Paths

    def main(args: Array[String]): Unit = {
      import LRParser._
      val parser = LRParser.Pull[Token](components)(scanner.sourceLocation)
      parser.logState = logState
      parser.attemptRecovery = attemptRecovery
      scanner.prompt()
      parser.run(scanner.next) match {
        case ERRONEOUS(message) =>
          println(message)
        case it: ACCEPTED =>
          it.prettyPrint()
        case _ =>
      }
    }
  }

}

