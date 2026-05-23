package org.sufrin.scalalr
package stage1

/**
 * Stage 1: First Language Atop Bootstrap
 *
 * Parser:     from stage1-notation.scalalr
 * Tree:       stage1.AST
 * Generator:  stage1.AST => bootstrap.Parser.Parser.Notation => Scala
 */

import org.sufrin.utility.PrettyPrint.AnyPretty

import java.nio.file.Paths


object Generator {
   import org.sufrin.scalalr.bootstrap.Syntax.Parser.{TokenSpec, Notation => BootstrapNotation}
   import org.sufrin.scalalr.stage1.AST.{Notation => NewNotation}
   implicit class AsBootstrapNotation(val notation: NewNotation) extends AnyVal {
     def toBootstrapNotation: BootstrapNotation = {
       import notation._
       BootstrapNotation(
           notation.thePackage,
           notation.theName,
           notation.theExplicitPath,
           notation.tablesType,
           notation.theScannerName,
           theTokenType     = theTokenType.toBootstrapNotation,
           theTokens        = theTokens.map(_.toBootstrapNotation),
           theRules         = theRules.map(_.toBootstrapNotation),
           theTokensInclude = theTokensInclude,
           theRulesInclude  = theRulesInclude,
           theNotationDialect = theDialects._1,
           theScalalrDialect = theDialects._2
       )
     }
   }

  var pretty: Boolean = false
  var output: String = "generated"

  def translate(notation: NewNotation): Unit = {
     if (pretty) notation.toBootstrapNotation.prettyPrint()
     else {
       val translation = bootstrap.Generator(notation.toBootstrapNotation, output)
       translation.makeFiles()
     }
   }

  def main(args: Array[String]): Unit = {
    import org.sufrin.utility._
    import stage1.ScalaLR._

    var log = false
    var lastArg = ""
    for  { arg <- args } {
      if (arg.startsWith("--output=")) {
        output = arg.replace("--output=", "")
      }
      else if (arg == "-log") log = true
      else if (arg == "-p") pretty = true
      else if (arg == "-o") {}
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
      else if (lastArg=="-o") output=arg
      else
      {
        val scanner = Scanner(SourceTextCursor(Paths.get(arg)))
        val parser = LRParser.Pull[Scanner.Token](Components)(scanner.sourceLocation)
        parser.logState = log
        parser.run(scanner.next) match {
          case org.sufrin.scalalr.LRParser.ACCEPTED(notation) => translate(notation.asInstanceOf[org.sufrin.scalalr.stage1.AST.Notation])
          case _ =>
        }
      }
      lastArg=arg
    }
  }

}

/* Bootstrap
case class Parser
    (thePackage: String,
     theName: String,
     explicitPath: String, // the destination for all generated files
     tablesType: String,
     theScannerName: String,
     theTokenType: Type,
     theTokens: Seq[TokenSpec],
     theRules: Seq[Rule],
     theTokensInclude: String,
     theRulesInclude: String)
 */

/* AST
 case class Parser
  (thePackage: String,
   theName: String,
   theExplicitPath: String,
   tablesType: String,
   theScannerName: String,
   theTokenType: Type,
   theTokens: Seq[TokenSpec],
   theRules: Seq[Rule],
   theTokensInclude: String,
   theRulesInclude: String)

 */