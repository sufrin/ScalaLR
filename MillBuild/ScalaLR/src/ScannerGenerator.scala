package org.sufrin.scalalr
package stage2

import org.sufrin.scalalr.stage2.AST.{Notation, TypedTerminal}
import org.sufrin.scalalr.stage2.Generator.finest
import org.sufrin.utility.SourceCode

object ScannerGenerator {
  def apply(notation: Notation, symbolTables: SymbolTables): SourceCode = new ScannerGenerator(notation, symbolTables)
}

class ScannerGenerator(notation: Notation, symbolTables: SymbolTables) extends SourceCode {
  import notation.{thePackage, theTokens, theTokensInclude}
  import symbolTables.{ALLTERMINALS, ALLTYPEDTERMINAL, nameToNumber}
  val theUnion = "Token"
  out(s"package $thePackage")
  out(s"object Scanner{")
  out("")
  out(theTokensInclude)
  out(s"trait $theUnion extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } ")


  // Synthetic terminals
  out(s"case object $$end extends $theUnion { val value = (); val symbol = 0 }") // TERMINAL
  out(s"case object error extends $theUnion { val value = (); val symbol = 1 }")
  out(s"case object UNDEF extends $theUnion { val value = (); val symbol = 2 }")

  for { terminal: TypedTerminal   <-  ALLTYPEDTERMINAL } {
    val theName = terminal.theName
    val symbol  = nameToNumber(theName)
    val name    = theName.forScala
    finest(s"${terminal.toString} ${symbol}")//**
    if (terminal.isTyped)
      out(s"case class ${name}(value: ${terminal.theScalaTypeName}) extends $theUnion { val symbol = $symbol }")
    else
      out(s"case object ${name} extends $theUnion { val value = (); val symbol = $symbol }")
  }


  out("// MAP SYMBOL NUMBERS TO NAMES")
  out(s"val symbolName: collection.immutable.Map[Int, String] = {")
  out("     import org.sufrin.utility.ArrayMap")
  out(s"    val arr = new Array[String](${nameToNumber.size})")
  out("         locally {")
  for { (name, number) <- nameToNumber }  out(s"          arr($number) = \"$name\"")
  out("         } // locally")
  out("         ArrayMap(arr)")
  out("     }")
  out("\n")

  out("// MAP QUOTED SYMBOL NAMES TO TOKENS ")
  out(s"val symbolToken: collection.immutable.Map[String, Token] =  collection.immutable.ListMap(")
  for { name <- symbolTables.declaredTerminalNames if name.isQuoted }  out(s"    \"${name.unQuoted}\" -> $name,")
  out("""    ""->$end)""")


  out("}\n")

}
