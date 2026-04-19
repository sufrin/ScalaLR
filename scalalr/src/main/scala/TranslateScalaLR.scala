package org.sufrin.scalalr
import org.sufrin.utility.PrettyPrint.AnyPretty
object TranslateScalaLR {
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

  def translate(notation: NewNotation): Unit = {
     notation.toBootstrapNotation.prettyPrint()
     //val translation = Translation(notation)
     //translation.makeFiles()
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