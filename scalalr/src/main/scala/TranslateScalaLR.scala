package org.sufrin.scalalr
import AST._
import org.sufrin.utility.PrettyPrint.AnyPretty
object TranslateScalaLR {
   def translate(notation: Notation): Unit = {
     notation.prettyPrint()
   }
}
