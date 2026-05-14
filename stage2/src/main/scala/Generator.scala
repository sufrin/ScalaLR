/**
 * Stage 2 language
 *
 * Parser:     built by stage1
 * Tree:       stage2.AST
 * Generator:  stage2.AST => Scala
 */

// TODO:  repackage mapping construction out of CodeGenerator into a separate class: NotationInformation
//

package org.sufrin.scalalr
package stage2

import org.sufrin.scalalr.Action.Action
import org.sufrin.utility.{SourceCode, SourceTextCursor}

import java.nio.file.{Files, Path}
import scala.collection.mutable


object Generator extends org.sufrin.logging.SourceLoggable {
  locally { level = org.sufrin.logging.INFO }
  import AST._
  import org.sufrin.utility.PrettyPrint._

  import java.nio.file.Paths

  var pretty: Boolean = false
  var prefix: String  = "generated"
  var bisonCounterexamples: Boolean = false
  var bisonHtml: Boolean = false
  var logParse: Boolean = false
  var logGeneration: List[String] = Nil

  def warning(what: String): Unit = println(s"WARNING: $what")

  class SymbolTables(notation: Notation) {
    lazy val declaredTerminals:    Seq[TypedTerminal]    = notation.declaredTerminals
    lazy val declaredNonterminals: Seq[TypedNonterminal] = notation.declaredNonterminals
    lazy val usedSymbolNames: Seq[Name] = {
      val allSymbolNames = for { rule <-notation.theRules; rhs <- rule.rhs; symb <- rhs.symbols } yield symb.theField
      allSymbolNames.distinct
    }

    lazy val declaredTerminalNames      = declaredTerminals.map(_.theName).distinct
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


      for  { symbol <- usedSymbolNames if !ALLDECLARED.contains(symbol)} fatal(s"Undeclared ${symbol.toFullString}")
      for  {symbol <- ALLDECLARED if ambiguousSymbols.contains(symbol)}    warning(s"Ambiguously defined $symbol")

      if (logGeneration contains "sym") {
        println("\n// Symbols and their types in order of appearance")

        {
          val width = (for {(name, ty) <- symbolType} yield name.toString.size).max
          for {(name, ty) <- symbolType} println(s"$name: ${" " * (width - name.toString.size)} $ty")
        }

        println("\n// Nonterminals and their definitions")

        {
          val width = (for {(name, rhs) <- nonTerminalDefinition} yield name.toString.size).max
          for {(name, rhs) <- nonTerminalDefinition; prod <- rhs} {
            println(s"$name ${" " * (width - name.toString.size)} = $prod")
          }
        }
      }

      fatalErrors==0
    }

  }

  def ScannerGenerator(notation: Notation, symbolTables: SymbolTables): SourceCode = new SourceCode {
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
    out("}\n")
  }


  class CodeGenerator(notation: Notation, symbolTables: SymbolTables) {
    import symbolTables._



    @inline def bisonTokenToInt(token: String): Int   = token match {
      case s"T-$num"  => num.toInt
      case "$default" => Int.MinValue
      case "$end"     => 0
      case "error"    => 1
    }
    @inline def numberToBisonToken(num: Int): String    = num match {
      case 0 => "$end"
      case 1 => "error"
      case _ => f"T-${(num)}%03d"
    }

    @inline def bisonToken(name: Name): String          = numberToBisonToken(nameToNumber(name))
    @inline def bisonTokenOf(symbol: Symbol): String    = bisonToken(symbol.theName)

    def bisonTokenized(production: Production): String = {
        production.symbols.map(_.theField).map(bisonToken).mkString(" ")
    }

    def sourceText(production: Production): String = {
      val prec: String = production.precedence match {
        case Some(name) => s" %prec ${(name)}"
        case None       => ""
      }
      production.symbols.map(_.toString).mkString("", " ", prec)
    }


    lazy val forBison: SourceCode = new SourceCode() {
      out(s"// Notation: ${notation.theName} generated by ScalaLR, with symbols represented uniformly")
      out(s"// ScalaLR translates these representations back to their source forms in all Bison outputs. ")
      out(s"%define lr.type ${notation.tablesType}")
      out(s"// Source Translation ")
      for {name <- declaredTerminalNames} out(s"// ${bisonToken(name)}\t${name} ")

      // TOKEN and PRIORITY SPECS
      for {spec: TokenSpec <- notation.theTokens.reverse if spec.terminals.nonEmpty} {
        val prefix = spec match {
          case _: Tokens => "%token "
          case _: Left => "%left "
          case _: Right => "%right "
          case _: Nonassoc => "%nonassoc "
          case _: Prec => "%prec "
          case _ => ""
        }
        val terms: Seq[TypedTerminal] = spec.terminals
        out(terms.map(bisonTokenOf).mkString(prefix, " ", ""))
      }
      out("%%")
      // PRODUCTIONS
      for {(name, rhs) <- nonTerminalDefinition; prod <- rhs} {
        out(s"${bisonToken(name)}: ${bisonTokenized(prod)} // ${name} = ${sourceText(prod)}")
      }
      out("")
    }

    def sourceToFile(fileName: String)(sourceCode: SourceCode): Unit = writeToFile(fileName)(sourceCode.toString)

    def writeToFile(fileName: String)(text: String): Unit = {
        import java.nio.charset.StandardCharsets
        import java.nio.file.{Files, Path}

        val path = Path.of(fileName)
        println(s"Writing   $path")
        Option(path.getParent).foreach(Files.createDirectories(_))
        Files.write(
          path,
          text.getBytes(StandardCharsets.UTF_8)
        )
      }

   def processGrammarWithBison(name: String): Boolean = {
      import scala.sys.process._
      writeToFile(s"$name.y")(forBison.toString)
      val output = new StringBuilder

      def escapeHtml(s: String): String =
        s.replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")

      def toSource(s: String, html: Boolean = false): String = {
        var report = s
        for { num <- 3 until numberToName.length } {
          val trans =  numberToName(num).toString
          report = report.replace(numberToBisonToken(num), if (html) escapeHtml(trans) else trans)
        }
        report
      }

      var lines = 0
      def logLine(line: String): Unit = {
        output.append(toSource(line) + "\n")
        lines += 1
        lines match {
          case 1 => println(s"Bison     $line")
          case 2 =>    print("Bison     generating diagnostics ")
          case _ => if (lines%10==0) print(".")
        }
      }

      val logger = ProcessLogger(line => logLine(toSource(line)))

      val htmlArg    = if (bisonHtml) List(s"--html=$name.html") else Nil
      val counterArg = if (bisonCounterexamples) List(s"-Wcounterexamples") else Nil
      var bisonArgs  = List("bison", "-v")++htmlArg++counterArg++List(s"--xml=$name.xml", s"--output=$name.tab.c", s"$name.y")

      val exit   = Process(bisonArgs).!(logger)
      val rmExit = Process(List("rm", s"$name.tab.c")).!(logger)


      fine(s"Bison tables in ${name}.xml")
      if (output.nonEmpty) {
        var report = output.toString()
        report = report.replace("[-Wcounterexamples]","").replace("[--Wconflicts-sr]","").replace("[--Wconflicts-rr]","")
        println()
        writeToFile(s"$name.log")(report)
      }

      if (exit!=0) warn(s"Bison exit: $exit")
      if (rmExit!=0) warn(s"Removing .c: $rmExit")

      try {

        //println(s"Rewriting $name.output using symbolic tokens from source")
        var report: String = toSource(Files.readString(Path.of(s"$name.output")))
        writeToFile(s"$name.output")(report)
      } catch {
        case exn: Exception =>
          println(s"$exn\nRewriting $name.output (for diagnostics, etc)")
      }

      if (bisonHtml) try {
        //println(s"Rewriting $name.html using symbolic tokens from source")
        var report = toSource(Files.readString(Path.of(s"$name.html")), true)
        writeToFile(s"$name.html")(report)
      } catch {
        case exn: Exception =>
          println(s"$exn\nRewriting $name.output (for diagnostics, etc)")
      }

      exit==0
    }


    /**
     * TODO: reduction table could avoid overflowing the code bounds on functions if
     * writtten in the following form:
     * for the ith production:
     * def red#i(dol$START: SourceLocation, dol$END: SourceLocation): PartialFunction[List[Any], Any] = { case pattern#i => expr#i }
     * and
     * def reduction(START: SourceLocation, END: SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
     * ... for each production #1
     * case i => red#i(START, END)
     * }
     */
    def ReductionGenerator(notation: Notation): SourceCode = new SourceCode {
      val theUnion = notation.theTokenType.name
      val theRules = notation.theRules
      val theRulesInclude = notation.theRulesInclude



      def toPattern(field: NamedField): String = {
        val Type = symbolType(field.theField)
        val scalaType = Type.scalaTypeName
        field.theFieldName match {
          case Some(name) =>
            if (Type == NoType) {
              warn(s"Named symbol ${name}: ${Type} carries no value")
              "_"
            }
            else
              s"${mangle(name)}: ${scalaType}"

          case None =>
            if (Type == NoType) "_" else s"${mangle(field.theField)}: ${scalaType}"

        }
      }

      /*
       *  Suppress the match for duplicated symbols: they need naming
       */
      val matchAll = Some("_")

      def sameFieldType(thisField: NamedField)(thatField: NamedField): Boolean =
        symbolType(thisField.theField).scalaTypeName==symbolType(thatField.theField).scalaTypeName

      /** Avoid giving gratis names to fields  */
      def toPatterns(fields: Seq[NamedField]): Seq[String] = {
        val anonfields = fields.filter(_.isAnonymous)
        for {field <- fields} yield
          if (anonfields.filter(sameFieldType(field)(_)).length <= 1) toPattern(field) else "_"
      }

      def outReduction(): Unit = {
        out("def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
        var productionNum = 0
        for {rule <- theRules} {
          for {production <- rule.rhs} {
            productionNum += 1
            out(s" /* ${rule.lhs} = ${production} */")
            val pat = toPatterns(production.symbols).mkString("List(", ", ", ") => ")
            out(s" case $productionNum => \n  { case ${pat}")

            production.reduction match {
              case None => out("None }")
              case Some(expression) =>
                val mangled = expression.mangle
                if (mangled.size + pat.size < 80) out(s" ${mangled} } ", false) else out(s"        ${mangled}\n  }")
            }
          }
        }


        out("\n }\n")
      }

      def outTreeReduction(): Unit = {
        out("case class PARSETREE(prod: String, rule: Int, trees:List[Any])")
        out("def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {")
        var ruleNum = 0
        for {rule <- theRules} {
          // val lhsName = s"\"${rule.lhs.theName}\""
          for {production <- rule.rhs} {
            ruleNum += 1
            val wholeProduction = s"${rule.lhs} = ${production}"
            //out(s"\n // ${wholeProduction}")
            out(s""" case $ruleNum => \n  { case trees$$trees => PARSETREE(\"\"\"$wholeProduction\"\"\", $ruleNum, trees$$trees ) }""")
          }
        }
        out(" }\n")
      }

      out(s"\npackage $thePackage\nobject Reduction {")

      out("\n")

      out(theRulesInclude)

      outReduction()
      //outTreeReduction()
      out("}\n")
    }

    /**
     * Generates a `Components` object that aggregates all the generated LR Parser components. This is
     * done because it only makes sense for the table parameters of the LRParser constructors to be generated
     * consistently from the same ScalaLR run.
     *
     * Used already in the small and expr tests
     * TODO: extend it use to other bootstrap components [Low priority]
     */
    def ComponentsGenerator(notation: Notation): SourceCode = new SourceCode {
      val thePackage = notation.thePackage
      out(s"""
             |// Generated by scalalr bootstrap generator ${java.time.LocalDateTime.now()}
             |
             |package $thePackage
             |
             |object NotationInformation {
             |  val signature: String  = "${notation.theSignature}"
             |  val name: String       = "${notation.theName}"
             |}
             |
             |object Components extends org.sufrin.scalalr.LRParserComponents {
             |  import org.sufrin.scalalr.Action.Action
             |  import org.sufrin.scalalr.LRParser.{NonTerminal, State, Symbol, Terminal}
             |  import org.sufrin.scalalr.SourceLocation
             |  type Reduce =  PartialFunction[List[Any], Any]
             |  val action:     State                                   => Terminal=>Action      = $thePackage.Tables.action
             |  val goto:       State                                   => NonTerminal => State  = $thePackage.Tables.goto
             |  val reduction:  (SourceLocation, SourceLocation, State) => Reduce                = $thePackage.Reduction.reduction
             |  val symbolName: Map[Symbol, String]                                              = $thePackage.Scanner.symbolName
             |}
             |
             |
             |
             |""".stripMargin
      )
    }

    def TableGenerator(targetPath: String, notation: Notation): SourceCode = new SourceCode {

        case class StateEntry(number: Int, transitions: Seq[(Int,Action)], reductions: Seq[(Int, Action)], gotos: Seq[(Int, Action)], disabled: Int)

        def readBisonStates(name: String, theRules: Seq[Rule]): Seq[StateEntry] = {
        import Action._

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
        val info: Seq[(Name,Int)]=
          for { rule <- theRules; production <- rule.rhs } yield
            (rule.lhs.theName, production.symbols.length)
        finer(s"for each production: lhs name -> rhs length\n  ${info.mkString("\n  ")}")

        def symbolNumber(name: Name): Int = nameToNumber(name)

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
            REDUCE(symbolNumber(name), rule, length)
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

          lazy val disabled = (for { node <- reductions if (node \ "@enabled").text=="false" } yield 1).sum
          fine(s"State $number $theActions / $theReductions / $theGotos")

          def encodeSymbolic(table: Seq[(String, Action)]): Seq[(Int, Action) ] = table.map{  case (token, tr) => (bisonTokenToInt(token), tr) }

          val result = StateEntry(number, encodeSymbolic(theActions), encodeSymbolic(theReductions), encodeSymbolic(theGotos), disabled)
          fine(result.toString)
          result
        }
        val result: Seq[StateEntry] = states map readState
        result
      }

        import Action._
        fine(s"Making tables for: ${notation.theName}")
        val entries: Seq[StateEntry] = readBisonStates(targetPath, notation.theRules)

          out(s"package $thePackage\nobject Tables {")

          // GOTO TABLES
          gen(s"\nval goto: Int => Int => Int = {")
          for {entry <- entries if entry.gotos.nonEmpty} {
            fine(entry.toString)
            gen(
              s"\n  case ${entry.number} => { ")
            for {(sy, GOTO(from, to)) <- entry.gotos} gen(s"case $sy => $to;  ")
            gen("}")
          }
          gen("\n  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}")
          gen("\n  }\n")

          // Action TABLES
          gen(s"\nimport org.sufrin.scalalr.Action._")
          gen(s"\nval action: Int => Int => Action = {")
          for {entry <- entries} {
            fine(entry.toString)
            gen(
              s"\n  case ${entry.number} => { ")
            // for { (sy, GOTO(from, to)) <- entry.gotos } gen(s"case $sy => $to;  ")
            for {(sy, act) <- entry.transitions} {
              gen(s"case $sy => $act;  ")
            }

            for {(sy, act) <- entry.reductions if (sy >= 0)} {
              gen(s"case $sy => $act;  ")
            }
            var needsDefault = true
            for {(sy, act) <- entry.reductions if (sy < 0)} {
              gen(s"case _ => $act;  ")
              needsDefault = false
            }

            if (needsDefault) gen(s"case _ => ERROR;  ")

            gen("}")
          }
          gen("\n  case _ => { case _ => ERROR }")
          gen("\n  }\n")

          entries.foreach(e => fine(e.toString))
          gen("}\n")
    }


    def generateScalaFiles(): Unit = {
      val targetPath = s"$thePath/$theNotationName"
      if (processGrammarWithBison(targetPath)) {
         sourceToFile(s"$thePath/Reduction.scala")(ReductionGenerator(notation))
         sourceToFile(s"$thePath/Components.scala")(ComponentsGenerator(notation))
         sourceToFile(s"$thePath/Tables.scala")(TableGenerator(targetPath, notation))
         sourceToFile(s"$thePath/Scanner.scala")(ScannerGenerator(notation, symbolTables))
      }
    }

  }


  def generateCode(notation: Notation): Unit = {
    if (pretty) notation.prettyPrint()
    val symbolTables = new SymbolTables(notation)
    val generator    = new CodeGenerator(notation, symbolTables)
    if (symbolTables.sanityCheck()) generator.generateScalaFiles() else println(s"${symbolTables.fatalErrors} (*) warnings -- no code generation")
  }

  def processScalaLR(cursor: SourceTextCursor): Unit = {
    import org.sufrin.scalalr.LRParser._
    import scalalr.stage2.{Components, Scanner}
    val scanner = LexicalScanner(cursor)
    val parser  = LRParser.Pull[LexicalScanner.Token](Components)(scanner.sourceLocation)
    parser.logState = logParse
    parser.run(scanner.next) match {
      case ACCEPTED(notation: Notation) => generateCode(notation)
      case other =>
    }
  }


  def main(args: Array[String]): Unit = {
    import org.sufrin.utility._
    import scalalr.stage2.{Components, Scanner}
    var arguments = args.toList
    def nextArgument(): String = { // pre arguments.nonEmpty; post arguments = old(arguments).tail; returns arguments.head
      val arg = arguments.head
      arguments = arguments.tail
      arg
    }
    var startLineNumber = 1
    var startColNumber = 0

    while (arguments.nonEmpty) {
      val arg = nextArgument()
      if      (arg.startsWith("--prefix=")) prefix = arg.replace("--prefix=", "")
      else if (arg.startsWith("--output=")) prefix = arg.replace("--output=", "")
      else if (arg == "-p" && arguments.nonEmpty) prefix = nextArgument()
      else if (arg == "-o" && arguments.nonEmpty) prefix = nextArgument()
      else if (arg == "-#" && arguments.nonEmpty)  { startLineNumber = nextArgument().toInt }
      else if (arg == "-##" && arguments.nonEmpty) { startColNumber = nextArgument().toInt }
      else if (arg == "-s" && arguments.nonEmpty) processScalaLR(SourceTextCursor(nextArgument().iterator).withStartLocation(startLineNumber, startColNumber))
      else if (arg == "-log")               logParse = true
      else if (arg.startsWith("-L"))        logGeneration ::= arg.replace("-L", "")
      else if (arg == "-pp")                pretty = true
      else if (arg == "-html")              bisonHtml = true
      else if (arg == "-c")                 bisonCounterexamples = true
      else if (arg.startsWith("-")) {
        println(
          """Usage: scalalr OPTION ... PATH ...
            |Treat (each) PATH as the  path in the filestore to scalalr SOURCE  and generate the
            |scala files corresponding to the %notation it defines.
            |
            |Place the generated files under the directory named by OUTPUTPATH
            |catenated with the %path (if any) declared in the scalalr source.
            |The default OUTPUTPATH is "./generated".
            |
            |OPTIONS:
            |-pp        prettyprint only
            |-log       log the input source parse
            |-html      output grammar report in html form
            |-c         generate detailed conflict report
            |
            |LOGGING OPTIONS
            |-Lsym      inventory the symbols
            |
            |OUTPUTPATH is set by one of
            |-p         OUTPUTPATH
            |-o         OUTPUTPATH
            |--output=OUTPUTPATH
            |--prefix=OUTPUTPATH
            |
            |LITERAL SOURCE (reserved for programmatic testing) a notation may be defined directly in an argument
            |-#         INT     first SOURCE line number
            |-##        INT     first SOURCE column number
            |-s         SOURCE
            |""".stripMargin)
        System.exit(0)
      }
      else if (arg.isEmpty) {}
      else try { processScalaLR(SourceTextCursor(Paths.get(arg))) } catch { case exn: Throwable => exn.printStackTrace() }
    }
  }
}

