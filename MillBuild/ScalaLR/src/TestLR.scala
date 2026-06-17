package org.sufrin.scalalr
package stage2


import org.sufrin.SourceLocation._
import org.sufrin.utility._

/**
 * An App subclass to simplify construction of `scalalr` tests of notaton processing. For example:
 * {{{
 *   object test1 extends Test("")("""
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
 * }}}
 *
 * @param counterExamples generate detailed report on conflicts
 * @param html    generate grammar report as html as well as text
 * @param log     log the parse
 * @param pretty  pretty print the tree
 * @param output  prefix files directory
 * @param source  text of the source
 * @param loc     IMPLICIT location of the Test in Scala source (to get scalalr error reports correct)
 *
 * @see GeneratorTests for longer examples.
 */
class TestLR(args: String="")(source: String)(implicit loc: SourceLocation) extends App {
  val effectiveArgs = List("--output=Testing/generated")++args.split(' ').toList ++ List("-#", (loc.line).toString, "-##", (loc.offset).toString, "-s", source)
  Generator.main(effectiveArgs.toArray)
}

class TestSRC(args: String="")(source: String)(implicit loc: SourceLocation) extends App {
  val effectiveArgs = List("--output=Testing/src/generated")++args.split(' ').toList ++ List("-#", (loc.line).toString, "-##", (loc.offset).toString, "-s", source)
  Generator.main(effectiveArgs.toArray)
}



/**
 * Build parser components for a notation, as well as a small test program named `test` that runs a parser
 * based on these components, using the literal text `input` as the input to the parser.
 *
 * The test program and the generated scanner components are all placed "under"  `Testing/src/generated`
 *
 *
 * @param args scalalr flags
 * @param definedPackage the name of the package the source notation defines
 * @param notationsource text of the notation description
 * @param testinput literal text of the input to the test program, or "" if the input is to come from stdin or the console
 * @param loc IMPLICIT location of the Test in Scala source  (to get scalalr error reports correct)
 */
class TestRUN(args: String="", definedPackage: String="")(testinput: String="")(notationsource: String)(implicit loc: SourceLocation) extends App {
  val effectiveArgs = List("--output=Testing/src/generated")++args.split(' ').toList ++ List("-#", (loc.line).toString, "-##", (loc.offset).toString, "-s", notationsource)
  Generator.main(effectiveArgs.toArray)
  val testInput =  if (testinput.isEmpty) "java.nio.file.Path.of(\"/dev/tty\")" else s"\"\"\"$testinput\"\"\""
  val testMain  = definedPackage.replace(".", "-")
  val testPath  = definedPackage.split('.')(0)
  def testProgram: String =
    s"""
      |//> using scala 2.13
      |//> using jar ROOT/ScalaLR/bin/scalalr.jar
      |//> using jar ROOT/Runtime/scalalrlibrary.jar
      |package ${definedPackage}
      |package runner
      |import org.sufrin.scalalr.stage2.TestRunner
      |import org.sufrin.scalalr._
      |import org.sufrin.utility.SourceTextCursor
      |object runner extends TestRunner [Scanner.Token] {
      |  val    components:  LRParserComponents = Components
      |  val    scanner:     Scanner[Scanner.Token] = Scanner(SourceTextCursor($testInput))
      |}
      |""".stripMargin
  def testCommand: String =
    s"""cd Testing/src/generated/
       |ln -sF ~/GitHomes/ScalaLR/MillBuild ROOT
       |scala-cli $testMain-runner.scala $testPath
       |""".stripMargin

  CodeGenerator.writeToFile(s"Testing/src/generated/$testMain-runner.scala")(testProgram)
  CodeGenerator.writeToFile(s"Testing/src/generated/$testMain-runner.sh")(testCommand)
  println(s"Use your IDE to run the program at Testing/src/generated/$testMain-runner.scala")
}


trait TestRunner[Token <: Lexeme] {
  import org.sufrin.scalalr.{Scanner, ScannerBuilder, SourceLocation}
  val logState:        Boolean = false
  val attemptRecovery: Boolean = false

  val    components: LRParserComponents
  def    scanner:    Scanner[Token]

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
      parser.run(scanner.next)  match {
        case ERRONEOUS(message) =>
          println(message)
        case it: ACCEPTED =>
          it.prettyPrint()
        case _ =>
      }
    }
}

