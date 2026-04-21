package org.sufrin.scalalr
import Action._
import Notation.mangleDollar
import Notation.Syntax._
import org.sufrin.logging.FINEST

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.collection.mutable


object Translation extends org.sufrin.logging.SourceLoggable
{ import org.sufrin.logging._
  level = WARN

  def apply(notation: Notation): Translation = new Translation(notation)

  object Auto extends org.sufrin.logging.SourceLoggable {
    level = WARN
  }
}

class Translation(val notation: Notation) {
  import Translation._
  import notation._
  import java.nio.file.Path

  val thePackage = if (thePackageName.isEmpty) theName else thePackageName
  val thePath =
    if (explicitPath.isEmpty)
         Path.of(thePackage.replace('/', '.').replace('.', '/')).getParent().toString // Normalize
    else Path.of(explicitPath)
  val theNotationName = Path.of(theName.replace('/', '.').replace('.', '/')).getFileName.toString // Normalize

  def makeFiles(): Unit = {
    val translation = this
    println(s"Path:     ${thePath}\nPackage:  ${thePackage}\nNotation: $theNotationName")
    if (translation.makeBisonTables(s"${translation.thePath}/$theNotationName")) {
      translation.writeSource(translation.scannerSource, s"${translation.thePath}/${translation.notation.theScannerName}.scala")
      translation.writeSource(translation.reduction,     s"${translation.thePath}/Reduction.scala")
      translation.writeSource(translation.makeScalaTables(s"${translation.thePath}/$theNotationName"), s"${translation.thePath}/Tables.scala")
    }
    else error("No files generated")
  }


  implicit class NamedFieldExtension(field: NamedField) {
    def isQuoted: Boolean = field.fieldSymbol.isQuoted
    def hasFieldName: Boolean = field.theName.isDefined
    def theFieldName: String = field.theName.get
    def hasFieldType: Boolean = symbolType.isDefinedAt(field.fieldSymbol)
    def theFieldType: String = symbolType(field.fieldSymbol)
  }

  // Enumeration of all the declared terminal symbols
  val tokenMap = new mutable.LinkedHashMap[String, Int]
  locally {
    tokenMap("$end") = 0
    tokenMap("error") = 1
  }

  // Enumeration of all the aliased terminal symbols
  val aliasMap = new mutable.LinkedHashMap[String, Int]

  def unalias(name: String): Int =
    aliasMap.getOrElse(name, tokenMap(name))


  var numberOfTerminals = 2

  def declareToken(name: String): Unit = {
    tokenMap getOrElseUpdate(name, { numberOfTerminals+= 1; numberOfTerminals })
    finest(s"declare: $name = $numberOfTerminals")//**
  }

  val symbolType = new mutable.LinkedHashMap[String, String]

  def declareSymbol(name: String, aType: String): Unit = {
    if (aType.nonEmpty) {
      symbolType.get(name) match {
        case None => symbolType(name) = aType
        case Some(other) =>
          warn(s"$name ambiguously typed $other and $aType ")
      }
      finer(s"$name: $aType")
    }
  }

  Auto.finest(s"declared tokens: ${theTokens.mkString("\n  ")}")//**

  for { t <- theTokens; theTyped <- t.terminals} declareToken (theTyped.theName)

  // A synthetic %token spec to warn about undeclared tokens used in the body
  val automaticallyDeclared: TokenSpec =
  {
    val allQuoted  =
           for {rule <- theRules; production <- rule.rhs; symbol <- production.symbols if symbol.isQuoted} yield (symbol.fieldSymbol)
    val automatic: Seq[String]  =
           for { symbol <- allQuoted if (!tokenMap.isDefinedAt(symbol))} yield symbol

    if (automatic.nonEmpty) {
        warn (s"%token declarations of the following quoted tokens will be made automatically:\n  ${automatic.mkString(" ")}")
    }

    Tokens(automatic.map(TypedTerminal(_)))
  }


  for { t <- theTokens; theTyped <- t.terminals} declareSymbol (theTyped.theName, theTyped.theType.name)
  for { rule <- theRules } declareSymbol(rule.lhs.theName, rule.lhs.theType.name)


  finest(s"declared: $tokenMap")//**

  def forBison(name: String): String = {
    import org.sufrin.scalalr.Notation.isBisonic
    if (name.forall(isBisonic(_))) name else {
      val trans = tokenMap(name)
      val bison = s"TOK-$trans"
      aliasMap(bison)=trans
      bison
    }
  }

  def forBison(symbol: Symbol): String = forBison(symbol.theName)

  def forBison(symbol: NamedField): String = forBison(symbol.fieldSymbol)

  def forScala(symbol: Symbol): String = forScala(symbol.theName)

  def forScala(theName: String): String = {
    theName match {
      case s"\"$name\"" => s"`$name`"
      case _ => theName
    }
  }

  def forScalaQuoted(theName: String): String = {
    theName match {
      case s"\"$name\"" => theName
      case _ => s"\"$theName\""
    }
  }


  def forScalaType(theName: String): String = { // generate an @unchecked annotation
    theName match {
      case s"\"$name\"" => s"`$name`"
      case _ => theName.replaceAll("""\[([^],]+)([\],])""", "[$1 @unchecked$2")
    }
  }

  //
  def bisonSource: String = bisonDeclarations(theTokens.toList)

  def bisonDeclarations(theTokens: Seq[TokenSpec]): String  =
  {
    val s = new StringBuilder()
    def out(string: String): Unit = s.append(string)

    out(s"\n// notation $theName")
    out(s"\n%define lr.type $tablesType")

    // Calculate the given token declarations, and those implicit in the priority declarations
    for { t <- theTokens } t match {
      case Tokens(toks) =>
        if (toks.nonEmpty) out("\n%token ")
        out(toks.map(forBison(_)).mkString(" "))
      case Left(toks) =>
        if (toks.nonEmpty) out("\n%token ")
        out(toks.map(forBison(_)).mkString(" "))
      case Right(toks) =>
        if (toks.nonEmpty) out("\n%token ")
        out(toks.map(forBison(_)).mkString(" "))
      case Nonassoc(toks) =>
        if (toks.nonEmpty) out("\n%token ")
        out(toks.map(forBison(_)).mkString(" "))
    }

    // Catch  symbolic tokens first seen in the rules -- to be automatically declared
    //
    for { rule <- theRules } {
      for { production <- rule.rhs } {
        for { symbol <- production.symbols } {
          if (symbol.isQuoted && !tokenMap.isDefinedAt(symbol.fieldSymbol)) {
            declareToken(symbol.fieldSymbol)
            out(s"\n%token ${forBison(symbol.fieldSymbol)} // ${symbol.fieldSymbol} (appears in the definition of ${rule.lhs.theName})")
            finest(s"catch: $tokenMap")//**
          }
        }
      }
    }



    finest(s"complete: $tokenMap")//**


    // Calculate the token declarations
    for { t <- theTokens } t match {
      case Tokens(toks) =>
      case Left(toks) =>
        out("\n%left ")
        out(toks.map(forBison(_)).mkString(" "))
      case Right(toks) =>
        out("\n%right ")
        out(toks.map(forBison(_)).mkString(" "))
      case Nonassoc(toks) =>
        out("\n%nonassoc ")
        out(toks.map(forBison(_)).mkString(" "))
    }


    // Show the special symbols
    val quoted = for { (name, num) <- tokenMap if (name.isQuoted) } yield (name, num)
    if (quoted.nonEmpty)
    { out("\n// Special symbols")
      quoted.foreach { case (name, num) => out(s"\n// $name TOK-${num}") }
    }

    out("\n%%")

    // RULES
    for { rule <- theRules } {
      for { production <- rule.rhs } {
        out("\n")
        out(rule.lhs.theName);
        out(": ")
        for { symbol <- production.symbols } {
          out(" ")
          out(forBison(symbol))
        }
        out(";")
      }
    }

    s.toString
  }

  def writeBisonSource(name: String): Unit = {
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Path}

    val path = Path.of(s"$name.y")
    println(s"Writing   $path")
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(
      path,
      bisonSource.getBytes(StandardCharsets.UTF_8)
    )
  }

  case class StateEntry(number: Int, transitions: Seq[(Int,Action)], reductions: Seq[(Int, Action)], gotos: Seq[(Int, Action)], conflicts: Int)


  def makeBisonTables(name: String): Boolean = {
    import scala.sys.process._
    writeBisonSource(name)
    val output = new StringBuilder
    val logger = ProcessLogger(line => output.append(line + "\n"))

    val exit = Process(Seq("bison", "-v", s"--html=$name.html", s"--xml=$name.xml", s"--output=$name.tab.c", s"-Wcounterexamples", s"$name.y")).!(logger)
    val rmExit = Process(Seq("rm", s"$name.tab.c")).!(logger)


    fine(s"Bison tables in ${name}.xml")
    if (output.nonEmpty) {
      var report = output.toString()
      for {(name, num) <- tokenMap if name.isQuoted} {
        report = report.replace(s"TOK-$num", name)
      }
      report = report.replace("[-Wcounterexamples]","").replace("[--Wconflicts-sr]","").replace("[--Wconflicts-rr]","")
      warn(s"Bison Warnings:\n${report}")
    }

    if (exit!=0) warn(s"Bison exit: $exit")
    if (rmExit!=0) warn(s"Removing .c: $rmExit")

    try {
      println(s"Rewriting $name.output using symbolic tokens from source")
      var report = Files.readString(Path.of(s"$name.output"))
      for {(name, num) <- tokenMap if name.isQuoted} {
        report = report.replace(s"TOK-$num", name)
      }
      Files.write(Path.of(s"$name.output"), report.getBytes(StandardCharsets.UTF_8))
    } catch {
      case exn: Exception =>
        println(s"$exn\nRewriting $name.output (for diagnostics, etc)")
    }

    try {
      def escapeHtml(s: String): String =
        s.replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")
      println(s"Rewriting $name.html using symbolic tokens from source")
      var report = Files.readString(Path.of(s"$name.html"))
      for {(name, num) <- tokenMap if name.isQuoted} {
        report = report.replace(s"TOK-$num", escapeHtml(name))
      }
      Files.write(Path.of(s"$name.html"), report.getBytes(StandardCharsets.UTF_8))
    } catch {
      case exn: Exception =>
        println(s"$exn\nRewriting $name.output (for diagnostics, etc)")
    }

    exit==0
  }



  def readBisonStateEntries(name: String): Seq[StateEntry] = {
    import scala.xml._
    val root      = XML.loadFile(s"$name.xml")
    val grammar   = root \\ "grammar"
    val automaton = root \\ "automaton"

    val xmlTerminals    = (grammar \\ "terminals" \\ "terminal") . toList
    val xmlNonterminals = (grammar \\ "grammar"   \\ "nonterminals" \\ "nonterminal") . toList


    val states      = automaton \\ "state"
    val stateCount  = states.length
    val symbolCount = xmlTerminals.size + xmlNonterminals.size

    /** for each nonterminal: name -> symbol number */
    val nonterminalsymbol = mutable.LinkedHashMap[String, Int]()
    locally {
      for { node <- xmlNonterminals } nonterminalsymbol(node \@ "name") = (node \@ "symbol-number").toInt
    }
    finer(s"for each nonterminal: name -> symbol number\n  ${nonterminalsymbol.toList.mkString("\n  ")}")

    /** for each production: lhs name -> rhs length  */
    val info: Seq[(String,Int)]=
      for { rule <- theRules; production <- rule.rhs } yield
        (rule.lhs.theName, production.symbols.length)
    finer(s"for each production: lhs name -> rhs length\n  ${info.mkString("\n  ")}")

    def symbolNumber(name: String): Int =
      nonterminalsymbol.getOrElse(name, aliasMap.getOrElse(name, tokenMap.getOrElse(name, Int.MinValue)))


    def readState(node: xml.Node): StateEntry = {
      val number      = (node \ "@number").text.toInt
      val actions     = node \\ "actions"
      val transitions = actions \\ "transitions" \\ "transition"
      val reductions  = actions \\ "reductions" \\ "reduction"

          /** The reduction corresponding to Bison's rule numbered `rule` */
          def makeREDUCE(rule: Int): REDUCE = {
            // REDUCE(symbol: Int, production: Int, size: Int) extends Action
            // the info table has origin 0
            val (name, length) = info(rule - 1)
            REDUCE(nonterminalsymbol(name), rule, length)
          }

          lazy val allActions: Seq[(String, Action)]  =
            for {node <- (transitions)} yield {
              val symbol = (node \ "@symbol").text
              (node \ "@type").text match {
                //case "accept" => ACCEPT
                case "error"  => ((symbol), ERROR)
                case "reduce" => ((symbol), makeREDUCE((node \@ "rule").toInt))
                case "shift"  => ((symbol), SHIFT((node \ "@state").text.toInt))
                case "goto"   => ((symbol), GOTO(inState = number, toState = (node \ "@state").text.toInt))
              }
            }

          lazy val theActions: Seq[(String, Action)]  =
            for { (sy, tr) <- allActions if !tr.isInstanceOf[GOTO] } yield (sy, tr)

          lazy val theGotos: Seq[(String, Action)] =
            for { (sy, tr) <- allActions if tr.isInstanceOf[GOTO] } yield (sy, tr)

          lazy val theReductions: Seq[(String, Action)] =
            for {node <- (reductions) if (node \ "@enabled").text=="true" } yield {
              val symbol = (node \ "@symbol").text
              (node \ "@rule").text match {
                case "accept" => ((symbol), ACCEPT )
                case "error"  => ((symbol), ERROR )
                case rule     => ((symbol), makeREDUCE (rule.toInt) )
              }
            }

          val conflicts = (for { node <- reductions if (node \ "@enabled").text=="false" } yield 1).sum // s"${(node \ "@symbol").text}, ${(node \ "@rule").text}"


      fine(s"State $number $theActions / $theReductions / $theGotos")

      def encodeSymbolic(table: Seq[(String, Action)]): Seq[(Int, Action) ] = table.map{ case (name, tr) => (symbolNumber(name), tr) }

      val result = StateEntry(number, encodeSymbolic(theActions), encodeSymbolic(theReductions), encodeSymbolic(theGotos), conflicts)
      //fine(result.toString)
      result
    }

    val result: Seq[StateEntry] = states map readState
    result
  }


  def makeScalaTables(name: String): String = {
    val entries = readBisonStateEntries(name)

    val conflicts = entries.map(_.conflicts).sum
    if (conflicts>0) {
      warn(s"There were $conflicts conflicts: some have been resolved in favour of shift")
    }

    {
      val output = new StringBuilder

      @inline def out(s: String): Unit = output.append(s)

      fine(s"Making tables for: $name")

      if (level == FINEST) out(entries.mkString("/*\n  ", "\n  ", "\n*/\n"))
      out(s"\npackage $thePackage\nobject Tables {")

      // GOTO TABLES
      out(s"\nval GOTOTABLE: Int => Int => Int = {")
      for {entry <- entries if entry.gotos.nonEmpty} {
        fine(entry.toString)
        out(
          s"\n  case ${entry.number} => { ")
        for {(sy, GOTO(from, to)) <- entry.gotos} out(s"case $sy => $to;  ")
        out("}")
      }
      out("\n  case _ => { case _ => throw new Throwable(\"BAD GOTO\")}")
      out("\n  }\n")

      // Action TABLES
      out(s"\nimport org.sufrin.scalalr.Action._")
      out(s"\nval ACTIONTABLE: Int => Int => Action = {")
      for {entry <- entries} {
        fine(entry.toString)
        out(
          s"\n  case ${entry.number} => { ")
        // for { (sy, GOTO(from, to)) <- entry.gotos } out(s"case $sy => $to;  ")
        for {(sy, act) <- entry.transitions} {
          out(s"case $sy => $act;  ")
        }

        for {(sy, act) <- entry.reductions if (sy >= 0)} {
          out(s"case $sy => $act;  ")
        }
        var needsDefault = true
        for {(sy, act) <- entry.reductions if (sy < 0)} {
          out(s"case _ => $act;  ")
          needsDefault = false
        }

        if (needsDefault) out(s"case _ => ERROR;  ")

        out("}")
      }
      out("\n  case _ => { case _ => ERROR }")
      out("\n  }\n")

      entries.foreach(e => fine(e.toString))
      out("}\n")
      output.toString()
    }
  }


  lazy val scannerSource: String = scannerDeclarations(automaticallyDeclared :: theTokens.toList)

  def scannerDeclarations(theTokens: Seq[TokenSpec]) = {
    val output = new StringBuilder
    @inline def out(s: String) : Unit = output.append(s)
    val theUnion = if (theScannerName!="") theTokenType.name else "Token"
    out(s"\npackage $thePackage")
    out(s"\nobject $theScannerName {")
    out("\n")
    out(theTokensInclude)
    out(s"\ntrait $theUnion extends org.sufrin.scalalr.Lexeme { val value: Any ; val symbol: Int } ")

    for { specs <- theTokens } {
      for { terminal: TypedTerminal   <- specs.terminals } {
         val theName = terminal.theName
         val symbol = tokenMap(theName)
         val name = forScala(theName)
         finest(s"${terminal.toString} ${tokenMap(theName)}")//**
         if (terminal.isTyped)
            out(s"\ncase class ${forScala(theName)}(value: ${terminal.theTypeName}) extends $theUnion { val symbol = $symbol }")
         else
           out(s"\ncase object ${forScala(theName)} extends $theUnion { val value = (); val symbol = $symbol }")
      }
    }

    // Synthetic terminals
    out(s"\ncase object $$end extends $theUnion { val value = (); val symbol = 0 }") // TERMINAL
    out(s"\ncase object error extends $theUnion { val value = (); val symbol = 1 }")
    out(s"\ncase object UNDEF extends $theUnion { val value = (); val symbol = 2 }")


    out("\n// GLOSSARY OF SYMBOL NAMES")
    out("\nval symbolName: Map[Int, String] = collection.immutable.ListMap[Int, String](")
    out("\n0->\"$end\", 1->\"error\", 2->\"UNDEF\"\n")

    // TERMINAL SYMBOL GLOSSARY
    for {  (name, symbol) <- tokenMap } {
      finer(s"${symbol} ${name}")//**
      out(s", ${symbol} -> ${forScalaQuoted(name)}\n")
    }

    var nextNonTerminal = numberOfTerminals
    // $ACCEPT
    // THIS SIMULATES A RULE SYNTHESISED BY BISON TO CAPTURE THE FIRST PRODUCTION AS THE ROOT
    val acceptRule =
     Rule(TypedNonterminal(theName="$accept", theType=theRules.head.lhs.theType),
                           List(Production(List(NamedField(Some("ACCEPTED"), theRules.head.lhs.theName)),
                                           Some(new org.sufrin.scalalr.Notation.Expression("$ACCEPTED")), None)))

    out("// GLOSSARY OF NONTERMINAL SYMBOL NAMES\n")
    for { rule <-  acceptRule +: theRules } {
      val lhs = rule.lhs
      nextNonTerminal += 1
      finer(s"$nextNonTerminal ${rule.lhs}")//**
      out(s", ${nextNonTerminal} -> ${forScalaQuoted(rule.lhs.theName)} \n")
    }
    out(")\n")

    out("\n}\n")
    output.toString
  }

  /**
   * TODO: reduction table could avoid overflowing the code bounds on functions if
   *       writtten in the following form:
   * for the ith production:
   *   def red#i(dol$START: SourceLocation, dol$END: SourceLocation): PartialFunction[List[Any], Any] = { case pattern#i => expr#i }
   * and
   *   def reduction(START: SourceLocation, END: SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
   *     ... for each production #1
   *     case i => red#i(START, END)
   *   }
   */
  lazy val reduction: String = {
    val output = new StringBuilder
    @inline def out(s: String) : Unit = output.append(s)
    val theUnion = theTokenType.name

    def toPattern(field: NamedField): String = {
      (field.hasFieldName, field.hasFieldType) match {
        case (true, true)   =>  s"$mangleDollar${forScala(field.theFieldName)}: ${forScalaType(field.theFieldType)}"
        case (false, true)  =>
          if (field.isQuoted) "_" else  s"$mangleDollar${forScala(field.fieldSymbol)}: ${forScalaType(field.theFieldType)}" // invent a name
        case (true, false)  =>
          warn(s"Named symbol ${field.theFieldName}: ${field.fieldSymbol} carries no value")
          "_"
        case (false, false) => "_"
      }
    }

    def outReduction(): Unit = {
      out("\ndef reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
      var productionNum = 0
      for {rule <- theRules} {
        for {production <- rule.rhs} {
          productionNum += 1
          out(s"\n // ${rule.lhs} = ${production}")
          out(s"\n case $productionNum => \n  { case ")
          out(production.symbols.map(toPattern).mkString("List(", ", ", ") => "))

          production.reduction match {
            case None => out("None }")
            case Some(expression) =>
              if (expression.mangle.size < 20) out(s" ${expression.mangle} } ") else out(s"\n        ${expression.mangle}\n  }")
          }

        }
      }


      out("\n }\n")
    }

    def outTreeReduction(): Unit = {
      out("\ncase class PARSETREE(prod: String, rule: Int, trees:List[Any])")
      out("\ndef parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
      var ruleNum = 0
      for {rule <- theRules} {
        // val lhsName = s"\"${rule.lhs.theName}\""
        for {production <- rule.rhs} {
          ruleNum += 1
          val wholeProduction = s"${rule.lhs} = ${production}"
          //out(s"\n // ${wholeProduction}")
          out(s"""\n case $ruleNum => \n  { case trees$$trees => PARSETREE(\"\"\"$wholeProduction\"\"\", $ruleNum, trees$$trees ) }""" +
            s"")
        }
      }
      out("\n }\n")
    }

    out(s"\npackage $thePackage\nobject Reduction {")

    out("\n")

    out(theRulesInclude)

    outReduction()
    outTreeReduction()
    out("\n}\n")
    output.toString
  }

  def writeSource(source: String, path: String): Unit = {
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Path}
    println(s"Writing   $path")
    Files.write(
      Path.of(path),
      source.getBytes(StandardCharsets.UTF_8)
    )
  }



}

