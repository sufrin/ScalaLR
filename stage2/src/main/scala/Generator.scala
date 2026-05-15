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

import org.sufrin.utility.SourceTextCursor

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

  @inline def bisonTokenToInt(token: String): Int   = token match {
    case s"T-$num"  => num.toInt
    case "$default" => Int.MinValue
    case "$end"     => 0
    case "error"    => 1
  }
  @inline def numberToBisonToken(num: Int): String  = num match {
    case 0 => "$end"
    case 1 => "error"
    case _ => f"T-${(num)}%03d"
  }

  def warning(what: String): Unit = println(s"WARNING: $what")

  var fieldNumber: Int = 0
  var delayedRules:  List[DelayedRule] = Nil

  case class DelayedRule(theName: Name, fields: List[NamedField], repeatType: Repeat, START: SourceLocation)

  def OptionType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("Option", List(symbolType.asInstanceOf[Type]), START)

  def ListType(symbolType: SymbolType, START: SourceLocation): Type =
    Type("List", List(symbolType.asInstanceOf[Type]), START)


  def expandRepeated(fields: List[NamedField], repeatType: Repeat, START: SourceLocation, END: SourceLocation): Name = {
      fieldNumber += 1
      val theName = Name(s"REP$fieldNumber", false, START)
      delayedRules ::= DelayedRule(theName, fields, repeatType, START)
      theName
  }

  def forceRule(symbolTable: SymbolTables)(delayedRule: DelayedRule): List[Rule] = {
    import delayedRule._
    def hasNoType(field: NamedField): Boolean = symbolTable.symbolType.getOrElse(field.theField, NoType)==NoType
    val searchOrdered = fields.iterator.filterNot(_.isAnonymous) ++ fields.iterator.filterNot(hasNoType) ++ fields.iterator
    repeatType match {
      case MaybeOne =>
        val field = searchOrdered.next()
        val theType = symbolTable.symbolType(field.theField)
        val theFieldName = field.theFieldName match {
          case None => field.theField
          case Some(other) => other
        }
        val lhs = TypedNonterminal(theName, OptionType(theType, START), START)
        val rhs = List(
          Production(Nil, Some(Expression("None")), None, START),
          Production(fields, Some(Expression(s"Some($$$theFieldName)")), None, START)
        )
        List(Rule(lhs, rhs, START))

      case OneOrMore | NoneOrMore =>
        val field = searchOrdered.next()
        val theType = symbolTable.symbolType.getOrElse(field.theField, NoType)
        val theFieldName = field.theFieldName match {
          case None        => field.theField
          case Some(other) => other
        }
        val theListName = Name(theName.forScala++"LIST", false, START)
        val lhs = TypedNonterminal(theListName, ListType(theType, START), START)
        val rhs = List(
          Production(fields.iterator.filterNot(hasNoType).toList,    Some(Expression(s"List($$$theFieldName)")), None, START),
          Production(NamedField(None, theListName, START) :: fields, Some(Expression(s"$$$theFieldName :: $$$theListName")), None, START)
        )
        val revlhs    = TypedNonterminal(theName, ListType(theType, START), START)
        val orNothing: List[Production] = if (repeatType==NoneOrMore) List(Production(Nil, Some(Expression("Nil")), None, START)) else Nil
        val revrhs:    Production = Production(List(NamedField(None, theListName, START)), Some(Expression(s"$$$theListName.reverse")), None, START)
        List(Rule(lhs, rhs, START), Rule(revlhs, revrhs::orNothing, START))
    }
  }

  def expandCode(notation: Notation): Notation = {
    val symbolTables = new SymbolTables(notation)
    notation.copy(theRules = notation.theRules ++ delayedRules.reverse.flatMap(forceRule(symbolTables)))
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
      case ACCEPTED(notation: Notation) => generateCode(expandCode(notation))
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

