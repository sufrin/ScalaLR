package org.sufrin.scalalr

import org.sufrin.utility.PrettyPrint.AnyPretty
import scalalr.parser.ScalaLR.Reduction.reduction
import scalalr.parser.ScalaLR.Tables.{ACTIONTABLE, GOTOTABLE}

import java.nio.file.Paths

object translateFLAB {
   import org.sufrin.scalalr.AST.{Notation => NewNotation}
   import org.sufrin.scalalr.Notation.Syntax.{TokenSpec, Notation => BootstrapNotation}
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
           theRulesInclude  = theRulesInclude
       )
     }
   }

  var pretty: Boolean = false
  var output: String = "generated"

  def translate(notation: NewNotation): Unit = {
     if (pretty) notation.toBootstrapNotation.prettyPrint()
     else {
       val translation = Translation(notation.toBootstrapNotation, output)
       translation.makeFiles()
     }
   }

  def main(args: Array[String]): Unit = {
      import org.sufrin.utility._
      import scalalr.parser.ScalaLR.Scanner
      import scalalr.parser.ScalaLR.Scanner._

      import java.nio.file.Path
      var log = false
      for  { arg <- args } if (arg.startsWith("--output=")) {
        output = arg.replace("--output=", "")
      }
      else if (arg == "-log") log = true
      else if (arg == "-p") pretty = true
      else if (arg.startsWith("-")) {
        println(
          """Usage: org.sufrin.scalalr.transFLAB [--output=<outputpath] [-p | -l]* [<file> ...]
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
        def next(): Token = if (scanner.hasNext) scanner.next() else $end
        val parser = new LRParser.Pull[Token](ACTIONTABLE, GOTOTABLE, reduction, symbolName, scanner.sourceLocation)
        parser.logState = log
        parser.run(next)
      }
    }

}

/* Bootstrap
case class Notation
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
 case class Notation
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