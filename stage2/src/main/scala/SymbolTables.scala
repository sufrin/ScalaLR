package org.sufrin.scalalr
package stage2

import AST.{Name, NoType, Notation, Production, SymbolType, TypedNonterminal, TypedTerminal}
import Generator.{logGeneration, prefix, warning}

import java.nio.file.Path
import scala.collection.mutable

class SymbolTables(notation: Notation) {
  lazy val declaredNonterminals: Seq[TypedNonterminal] = notation.declaredNonterminals
  lazy val usedSymbolNames: Seq[Name] = {
    val allSymbolNames = for { rule <-notation.theRules; rhs <- rule.rhs; symb <- rhs.symbols } yield symb.theField
    allSymbolNames.distinct
  }

  lazy val quotedTerminals: Seq[TypedTerminal] = usedSymbolNames.filter(_.isQuoted).distinct.map{
    case name: Name => TypedTerminal(name, NoType, SourceLocation(-1, -1))
  }

  lazy val declaredTerminals: Seq[TypedTerminal] = notation.declaredTerminals // ++ quotedTerminals

  lazy val declaredTerminalNames      = (declaredTerminals.map(_.theName)).distinct
  lazy val declaredNonterminalNames   = declaredNonterminals.map(_.theName).distinct


  /** Map Name to definition (as a production sequence) */
  val nonTerminalDefinition = mutable.LinkedHashMap[Name, Seq[Production]]()
  locally {
    for { rule <-notation.theRules; rhs <- rule.rhs }
      nonTerminalDefinition(rule.lhs.theName) = rule.rhs
  }



  val BISONPREDEFINES: List[Name]  = List(Name("$end", false), Name("error", false), Name("UNDEF", false))
  val BISONACCEPT:  List[Name]     = List(Name("$accept", false)) // Injected by Bison as the first non-terminal symbol
  val ALLTERMINALS: List[Name]     = BISONPREDEFINES++declaredTerminalNames
  val ALLDECLARED:  Seq[Name]      = (ALLTERMINALS++BISONACCEPT++declaredNonterminalNames).distinct.toSeq
  val ALLTYPEDTERMINAL: Seq[TypedTerminal] = declaredTerminals

  /** Map Name to declared Type */
  val symbolType = mutable.LinkedHashMap[Name, SymbolType]()
  locally {
    for { sym <- declaredNonterminals }  {
      symbolType(sym.theName) = sym.theType
    }
    for { newSymbol <- declaredTerminals }  {
      symbolType(newSymbol.theName) = newSymbol.theType
    }
    // ALL symbols must have types
    for { name <- BISONPREDEFINES }  symbolType(name) = NoType
  }

  val numberToName: Seq[Name] = ALLDECLARED.toSeq
  val nameToNumber: mutable.LinkedHashMap[Name, Int] = new mutable.LinkedHashMap[Name, Int]
  locally {
    for { i <- 0 until numberToName.size } nameToNumber(numberToName(i)) = i
  }


  val thePackage = if (notation.thePackage.isEmpty) notation.theName else notation.thePackage
  val thePath =
    if (notation.theExplicitPath.isEmpty)
      Path.of(prefix, thePackage.replace('/', '.').replace('.', '/')).getParent().toString // Normalize
    else
      Path.of(prefix, notation.theExplicitPath)
  val theNotationName = Path.of(prefix,notation.theName.replace('/', '.').replace('.', '/')).getFileName.toString // Normalize

  var fatalErrors: Int = 0

  def sanityCheck(): Boolean = {
    def fatal(message: String): Unit = {
      warning(s"(*) $message")
      fatalErrors += 1
    }

    val nonTerminalSymbol = mutable.LinkedHashMap[String, TypedNonterminal]()
    locally {
      for { newSymbol <- declaredNonterminals } nonTerminalSymbol.get(newSymbol.theName.toString) match {
        case None => nonTerminalSymbol(newSymbol.theName.toString) = newSymbol
        case Some(symbol) =>
          warning(s"Redefining ${symbol.theName} ${symbol.location} by ${newSymbol.theName} ${newSymbol.location} ")
          nonTerminalSymbol(symbol.theName.toString) = newSymbol
      }
    }

    val ambiguousSymbols = declaredTerminalNames.intersect(declaredNonterminalNames).distinct


    for  { symbol <- usedSymbolNames if !ALLDECLARED.contains(symbol)}
         if (symbol.isQuoted)
            fatal(s"Undeclared quoted ${symbol.toFullString}") // TODO: autodeclare quoted symbols
         else
            fatal(s"Undeclared ${symbol.toFullString}")
    for  {symbol <- ALLDECLARED if ambiguousSymbols.contains(symbol)}    warning(s"Ambiguously defined $symbol")

    if (logGeneration contains "sym") {
      println("\n// Symbols and their types in order of appearance")
      val width = (for {(name, ty) <- symbolType} yield name.toString.size).max
      for {(name, ty) <- symbolType} println(s"$name: ${" " * (width - name.toString.size)} $ty")
    }

    if (logGeneration.contains("")) {
        println("\n// Nonterminals and their definitions")
          val width = (for {(name, rhs) <- nonTerminalDefinition} yield name.toString.size).max
          for {(name, rhs) <- nonTerminalDefinition; prod <- rhs} {
            println(s"$name ${" " * (width - name.toString.size)} = $prod")
          }
    }

    fatalErrors==0
  }

}
