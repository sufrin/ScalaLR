

package org.sufrin.scalalr
import org.sufrin.logging._
import org.sufrin.utility._



object Notation {

  case class SyntaxError(message: String) extends Throwable

  def syntaxError[T](message: String): T = {
    println(s"Syntax error: $message")
    throw new SyntaxError(message)
  }


  def isBisonic(c: Char): Boolean = c.isLetterOrDigit || c=='.' || c=='_'

  class Nonterminal(val name: String) extends AnyVal {
    override def toString: String = name
  }

  class Terminal(val name: String) extends AnyVal {
    override def toString: String = name
  }

  class Type(val name: String) extends AnyVal {
    override def toString: String = name
  }

  val mangleDollar = "dol$"
  class Expression(val text: String) extends AnyVal {
    override def toString: String = text
    def mangle: String = text.replace("$", mangleDollar)
  }

  case class Symbol(name: String) extends AnyVal {
    override def toString: String = name
  }


  object Lexical {


    type Value = AnyRef

    trait Token {
      val value: Value
    }

    trait Atom extends Token {
      val value = null
    }

    case class  ID(value: String)   extends Token
    case class  NUM(value: String)  extends Token
    case class  CODE(value: String) extends Token
    case class  TYPE(value: String) extends Token
    case class  COMMENT(value: String) extends Token
    case object ALT                 extends Atom
    case object HASTYPE             extends Atom
    case object IS                  extends Atom
    case object SEMICOLON           extends Atom
    case object BRA                 extends Atom
    case object KET                 extends Atom
    case object LRTABLETYPE         extends Atom
    case object EMPTY               extends Atom

    case object NOTATION            extends Atom
    case object PACKAGE             extends Atom
    case object RULES               extends Atom
    case object SCAN                extends Atom
    case object EXTENDING           extends Atom
    case object PATH                extends Atom
    case object INCLUDE             extends Atom
    case object EOF                 extends Atom

    sealed trait TokenDirective extends Atom
    case object LEFT     extends TokenDirective
    case object RIGHT    extends TokenDirective
    case object NONASSOC extends TokenDirective
    case object TOKEN    extends TokenDirective

    case class LEXICALERROR(message: String) extends Atom

    object Scanner {
      def apply(chars: SourceTextCursor): Scanner = new Scanner(chars)
    }

    class Scanner(chars: SourceTextCursor) extends Iterator[Token] {
      var sl, sc: Int = 0
      def sourceLocation: String = s"${sl}.${sc}"

      @inline def hasChar: Boolean = chars.hasCurrent
      @inline def theChar: Char = chars.current
      @inline def nextChar(): Unit = chars.next()
      @inline def afterNextChar(t: Token): Token = {
        nextChar()
        t
      }


      def eatComment(): Unit = {
        var level = 0
        var go = true
        //println(s"start comment $theChar")
        nextChar() // skip the *
        while (go && hasChar) {
          //print(theChar)
          chars.dropWhile( c=>c!='*')
          // theChar=='*' or !hasChar
          //print(theChar)
          nextChar()
          //println(theChar)
          if (theChar=='/') go=false
        }
        //println("end comment")
        nextChar()
      }

      def hasNext: Boolean = chars.hasCurrent
      def next(): Token = {
        sl = chars.lines
        sc = chars.chars
        val result =
          chars.current match {
            case '[' => afterNextChar(BRA)
            case ']' => afterNextChar(KET)
            case '|' => afterNextChar(ALT)
            case ':' => afterNextChar(HASTYPE)
            case '=' => afterNextChar(IS)
            case '/' =>
              nextChar()
              theChar match {
                case '*' =>
                  eatComment()
                case '/' =>
                  chars.dropWhile( c=>c!='\n')
                case other =>
                  Syntax.Parser.warn(s"Malformed comment sentinel: \"/$other\" at $sourceLocation")
                  chars.dropWhile( c=>c!='\n')
              }
              next()
            case '{' =>
              nextChar(); afterNextChar(CODE(chars.takeNested('{', '}')  .mkString("")))
            case '«' =>
              nextChar(); afterNextChar(CODE(chars.takeNested('«', '»')  .mkString("")))
            case '(' =>
              nextChar(); afterNextChar(TYPE(chars.takeNested('(', ')')  .mkString("")))
            case ';' => afterNextChar(SEMICOLON)
            case '%' =>
              nextChar()
              if (chars.current=='%') { afterNextChar(RULES) }
              else {
                val directive = chars.takeWhile(_.isLetterOrDigit).mkString("")
                directive.toLowerCase match {
                  case "type"         => LRTABLETYPE
                  case "empty"        => EMPTY
                  case "scanner"      => SCAN
                  case "extending"    => EXTENDING
                  case "notation"     => NOTATION
                  case "package"      => PACKAGE
                  case "path"         => PATH
                  case "token"        => TOKEN
                  case "left"         => LEFT
                  case "right"        => RIGHT
                  case "non"          => NONASSOC
                  case "rules"        => RULES
                  case "include"      => INCLUDE
                  case _ => syntaxError(s"Unknown directive %$directive (at $sourceLocation)")
                }
              }
            case '"'  => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='"')  .mkString("\"", "", "\"")))
            case '\'' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='\'') .mkString("\"", "", "\"")))
            case '`' => nextChar(); afterNextChar(ID(chars.takeWhile( c => c!='`') .mkString("\"", "", "\"")))

            case c if c.isLetter =>
              val prefix = chars.takeWhile(isBisonic(_))
              ID((prefix).mkString(""))
            case c if c.isWhitespace =>
               while (hasChar && theChar.isWhitespace) nextChar()
               if (hasChar) next() else EOF
            case other =>
               val r = LEXICALERROR(s"Unrecognised $other (at $sourceLocation)")
               nextChar()
               r
          }
        result
      }
    }
  }

  object Syntax {
    import Lexical._

    import scala.collection.mutable.{ListBuffer => Buffer}

    object Parser extends SourceLoggable {
      level = WARN
      def apply(scanner: Lexical.Scanner): Parser = new Parser(scanner)
    }

    class Parser(scanner: Lexical.Scanner) {
       import Parser._
       val scan = Cursor(scanner)

       locally {
         scan.next()
       }

       def Shift[T](value: T): T = {
         scan.next()
         value
       }

       def Skip(): Unit =
        if (scan.hasCurrent) scan.next() else {
          val message = s"(end of source while expecting a symbol)"
          syntaxError(s"$message at ${scanner.sourceLocation}")
        }

       def Skip(atom: Atom, context: String=s""): Unit =
         if (scan.current==atom) scan.next() else {
           val message = s"$context expecting $atom; found ${scan.current}"
           syntaxError(s"$message at ${scanner.sourceLocation}")
         }

      def Ignore(atom: Atom): Unit =
        if (scan.current==atom) scan.next() else {
        }

      def Expecting[T](convert: PartialFunction[Token, T]): T = {
          val token = scan.current
          val result = convert(token)
          result
      }

      def parseRules(): Seq[Rule] = {
        val rules = Buffer(parseRule())
        while (scan.current==SEMICOLON) {
          Skip()
          if (scan.current.isInstanceOf[ID])
             rules addOne parseRule()
        }
        rules.toSeq
      }



      def parseRule(): Rule = {
        val symbol: TypedNonterminal = parseTypedNonTerminal()
        val _ = Skip(IS)
        val _ = Ignore(ALT)
        val rhss = Buffer(parseProduction())
        while (scan.current==ALT) {
          Skip()
          rhss addOne parseProduction()
        }
        Rule(symbol, rhss.toSeq)
      }

      def parseProduction(): Production = {
        var go = true
        val symbols = Buffer[NamedField]()
        var reduction: Option[Expression] = None

        while (go) {
          Expecting {
            case EMPTY =>
              Skip() // just ignore %empty wherever it appears
            case ID(_) =>
              val namedSymbol = parseNamedField()
              symbols addOne namedSymbol
            case CODE(text) =>
              Skip()
              reduction = Some(new Expression(text))
              go = false
            case SEMICOLON | ALT  =>
              go = false
            case other =>
              syntaxError(s"Production ends with $other ${scanner.sourceLocation}")
          }
        }
        Production(symbols.toSeq, reduction, None)
      }

      def parseNamedField(): NamedField = {
          val name1 = Expecting {
            case ID(name) => Shift(name)
          }
          scan.current match {
            case HASTYPE =>
              Skip()
              Expecting {
                case ID(name2) => Shift(NamedField(Some(name1), name2))
              }
            case _ => NamedField(None, name1)
          }
      }

      def parseInclude(): String = {
        if (scan.current==INCLUDE) {
          Skip()
          Expecting {
            case CODE(scala) => Shift(scala)
            case other       => syntaxError(s"Expecting { ... code ... } after %include (at ${scanner.sourceLocation})")
          }
        } else ""
      }

      def parseLRTableType(): String = {
        if (scan.current==LRTABLETYPE ) {
          Skip()
          Expecting {
            case ID("lr")        => Shift("canonical-lr")
            case ID("canonical") => Shift("canonical-lr")
            case ID("lalr")      => Shift("lalr")
            case ID("ielr")      => Shift("ielr")
            case other           => syntaxError(s"Expecting 'lr' or 'canonical' or 'lalr' or 'ielr' after %tables, found $other (at ${scanner.sourceLocation})")
          }
        } else "lalr"
      }

      def parseNameAfter(symbol: Atom): String = {
        Skip(symbol)
        Expecting {
          case ID(name) => Shift(name)
          case other    => syntaxError(s"Expecting an ID, found $other after $symbol (at ${scanner.sourceLocation})")
        }
      }

      def parsePathAfter(symbol: Atom): String = {
        Skip(symbol)
        Expecting {
          case ID(s"\"$name\"") => Shift(name)
          case ID(name)         => Shift(name)
          case other            => syntaxError(s"Expecting a quoted path or name, found $other after $symbol (at ${scanner.sourceLocation})")
        }
      }

      def parseExending(): String = {
        if (scan.current==EXTENDING) {
          Skip()
          Expecting {
            case ID(name) => Shift(name)
            case other    => syntaxError(s"Expecting an ID, found $other after %extending")
          }
        } else ""
      }

      def parseNotation(): Notation = {
        val _ = Skip(NOTATION)
        val name = Expecting{
          case ID(name) => Shift(name)
        }
        var packageName = ""
        var tablesType = "lalr"
        var scannerName = "Scanner"
        var traitName = "Token"
        var tokensInclude = ""
        var explicitPath = ""
        var go = true
        while (go) {
          finest(s"Paragraph: ${scan.current} (${scanner.sourceLocation})")
          scan.current match {
            case PACKAGE      => packageName = parseNameAfter(PACKAGE)
            case LRTABLETYPE  => tablesType = parseLRTableType()
            case INCLUDE      => tokensInclude = parseInclude()
            case SCAN         => scannerName = parseNameAfter(SCAN)
            case EXTENDING    => traitName = parseExending()
            case PATH         => explicitPath = parsePathAfter(PATH)
            case other        => go = false
          }
        }
        val tokenDefs = parseTokenDefs()

        finest(s"Paragraph: ${scan.current} (${scanner.sourceLocation})")
        Skip(RULES)
        finest(s"Paragraph: ${scan.current} (${scanner.sourceLocation})")
        val rulesInclude = parseInclude()
        val rules = parseRules().toList

        Notation(packageName, name, explicitPath, tablesType, scannerName, new Type(traitName), tokenDefs, rules, tokensInclude, rulesInclude)
      }



      def parseTokenDefs(): Seq[TokenSpec] = {
        val result: Buffer[TokenSpec] = Buffer()
        var go = true
        while (go && scan.hasCurrent) {
          scan.current match {
            case TOKEN    |
                 LEFT     |
                 RIGHT    |
                 NONASSOC  => result addOne parseSpec()
            case RULES =>
              go = false
            case other =>
              syntaxError(s"Expecting %token, %left, %right, %non, %rules; found $other")
          }
        }
        result.toSeq
      }

      def parseSpec(): TokenSpec = {
        val build: Seq[TypedTerminal]=>TokenSpec = scan.current match {
          case TOKEN    => Tokens
          case LEFT     => Left
          case RIGHT    => Right
          case NONASSOC => Nonassoc
        }
        scan.next()
        val tokens = parseTypedTerminals()
        build(tokens)
      }

      def parseTyped(): (String, Type) = {
        val name = Expecting {
          case ID(name) => Shift(name)
          case other => syntaxError(s"Expecting an ID, found $other")
        }
        scan.current match {
          case TYPE(text) =>
            Shift(name, new Type(text))
          case HASTYPE =>
            Skip()
            Expecting {
              case ID(text) =>
                Shift(name, new Type(text))
              case TYPE(text)  =>
                Shift(name, new Type(text))
            }
          case _ =>
            (name, noType)
        }
      }

      def parseTypedNonTerminal(): TypedNonterminal = {
        val(theName, theType) = parseTyped()
        TypedNonterminal(theName, theType)
      }


      def parseTypedTerminals(): Seq[TypedTerminal] = {
        val result: Buffer[TypedTerminal] = Buffer()
        var go = true
        while (go && scan.hasCurrent) scan.current match {
          case ID(_) =>
            val (theName, theType) = parseTyped()
            result addOne TypedTerminal(theName, theType)
          case _ => go = false
        }
        result.toSeq
      }
    }

    case class a(name: String, var code: Int = 0){
      override def toString: String = s"${this.getClass.getSimpleName}.$name($code)"
    }


    case class Rule(lhs: TypedNonterminal, rhs: Seq[Production])

    case class Production(
                           symbols: Seq[NamedField],
                           reduction: Option[Expression],
                           precedence: Option[Terminal]) {
      val code = if (reduction.isDefined) s" {${reduction.get}}" else ""
      val prec = if (precedence.isDefined) s" %prec ${precedence.get}" else ""

      override def toString: String = s"${symbols.map(_.toString).mkString(" ")}$code$prec"
    }

    case class Error(message: String)

    case class Notation
    (thePackageName: String,
     theName: String,
     explicitPath: String, // the destination for all generated files
     tablesType: String,
     theScannerName: String,
     theTokenType: Type,
     theTokens: Seq[TokenSpec],
     theRules: Seq[Rule],
     theTokensInclude: String,
     theRulesInclude: String)

    val noType = new Type("")


    trait Symbol { def theName: String }

    case class TypedTerminal(theName: String, theType: Type=noType) extends Symbol {
      def isTyped: Boolean = !theType.toString.isEmpty
      def theTypeName: String = theType.name
    }

    case class TypedNonterminal(theName: String, theType: Type=noType) extends Symbol {
      override def toString: String =
        if (theType.equals(noType)) theName else s"$theName: ${theType.name}"
    }

    case class NamedField(theName: Option[String], fieldSymbol: String) {
      override def toString: String = if (theName.isDefined) s"${theName.get}: $fieldSymbol" else fieldSymbol
      def isQuoted: Boolean = fieldSymbol.isQuoted
    }

    implicit class StringExtras(s: String) {
      def isQuoted: Boolean = s.matches("\".+\"")
    }

    trait TokenSpec { val terminals: Seq[TypedTerminal] }
    case class Left(terminals:     Seq[TypedTerminal]) extends TokenSpec
    case class Right(terminals:    Seq[TypedTerminal]) extends TokenSpec
    case class Nonassoc(terminals: Seq[TypedTerminal]) extends TokenSpec
    case class Tokens(terminals:   Seq[TypedTerminal]) extends TokenSpec

  }

}
