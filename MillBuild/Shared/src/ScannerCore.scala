package org.sufrin.scalalr

import org.sufrin.utility.SourceTextCursor

/**
 * A `Scanner` is a `Token` `Iterator` that supplies source locations.
 * In general `sourceLocation()` yields the starting source location of the latest `Token` to be
 * returned.
 */
trait Scanner[Token]  extends Iterator[Token] {
  def sourceLocation(): SourceLocation
  def prompt(): Unit
  def defineSymbolTokens(symbolToken: Map[String, Token]): Unit
  def withSymbolTokens(symbolToken: Map[String, Token]): this.type
}

/**
 * A builder for parameterisable lexical scanners. Simplicity is the
 * watchword so as to keep the API simple. It is expected that the
 * following "token-handlers" will all be defined. They are intended
 * to map a sequence of characters discovered by the scanning machinery
 * to a `Token` of the appropriate kind
 *
 * {{{
 *   // Token from a "quoted" text, after processing any "escape" sequences in `body`
 *   def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token
 *
 *   // Token from a text of the form `0x[hexit]+`
 *   def mkHex(source: Seq[Char]):   Token
 *
 *   // Token from a text of the form `[digit]+`
 *   def mkDec(source: Seq[Char]):   Token
 *
 *   // Token from a text of the form `[digit]+.[digit]+(e[digit]+)?`
 *   def mkReal(source: Seq[Char]):  Token
 *
 *   // Token from the text of an identifier [letter][letterordigit]+
 *   def mkIDENTIFIER(source: Seq[Char]):    Token
 *
 *   /** error token from an unrecognised character, or if you really care, an extension point  */
 *   def mkERROR(source: Seq[Char]): Token
 * }}}
 */
abstract class ScannerCore[Token <: Lexeme](chars: SourceTextCursor) extends Scanner[Token] {
  import org.sufrin.utility.CharSequenceMap

  import scala.collection.mutable
  /** Token from a "quoted" text, after processing any "escape" sequences in `body` */
  def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token
  /** Token from a text of the form `0x[hexit]+` */
  def mkHex(source: Seq[Char]):   Token
  /** Token from a text of the form `[digit]+` */
  def mkDec(source: Seq[Char]):   Token
  /** Token from a text of the form `[digit]+.[digit]+(e[digit]+)?` */
  def mkReal(source: Seq[Char]):  Token
  /** Token from the text of an identifier [letter][letterordigit]+ */
  def mkIDENTIFIER(source: Seq[Char]):    Token
  /** error token from an unrecognised character, or if you really care, an extension point  */
  def mkERROR(source: Seq[Char]): Token
  /**
   * None if a newline is just whitespace; else Some(tok) if a newline is to yield tok without reading ahead.
   * This is a straightforward way of interpreting a newline from a terminal exactly when it appears.
   */
  val NEWLINE:                    Option[Token] // == NONE when NL is just whitespace
  val ENDSTREAM:                  Token

  /** Trie mapping from (non-alphabetic) names to the tokens they denote  */
  val tokenMap: CharSequenceMap[Token] = new CharSequenceMap[Token]

  /** Mapping from (alphabetic) names to the tokens they denote */
  val symbolMap: collection.mutable.Map[String,Token] = new mutable.LinkedHashMap[String,Token]

  def defineSymbolTokens(symbolToken: Map[String, Token]): Unit = {
    withSymbolTokens(symbolToken);
    ()
  }

  /** Initialize the token and symbol mappings using the mapping supplied by `ScalaLR` .
   *  All symbols that will be seen as Letter LetterOrDigit* go into the symbol map.
   *  All symbols (~LetterOrDigit)+ go into the token trie map.
   *  Other symbols are ignored: ie there is no provision for hybrids.
   */
  def withSymbolTokens(symbolToken: Map[String, Token]): this.type = {
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall(_.isLetterOrDigit)} symbolMap(symbol) = token
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall { c => ! c.isLetterOrDigit}} tokenMap(symbol) = token
    this
  }

  def prompt(): Unit = { print(chars.promptString); System.out.flush() }

  var lastLocation: SourceLocation = SourceLocation(chars.lines,  chars.chars)
  def sourceLocation(): SourceLocation = lastLocation


  @inline def hasChar: Boolean = chars.hasCurrent
  @inline def theChar: Char = chars.current
  @inline def nextChar(): Unit = chars.next()
  @inline def afterNextChar(t: Token): Token = { nextChar(); t }

  /** Nested comment */
  def eatComment(): Unit = {
    val loc = sourceLocation()
    var level = 1
    var lastChar = ' '
    nextChar()                      // skip the *
    while (hasChar && level!=0) {
      theChar match {
        case '*' if lastChar == '/' => level += 1
        case '/' if lastChar == '*' => level -= 1
        case _                      => lastChar=theChar
      }
      nextChar()
    }
    if (level!=0) System.err.println(s"WARNING: nested (level $level) comment unclosed $loc")
    nextChar()
  }

  def eatWhitespace(): Unit = {
    while (hasChar && theChar.isWhitespace) nextChar()
  }


  def nextNumber(intPart: Seq[Char]) : Token = {
    if (hasChar)
    theChar match {
      case '.' =>
        nextChar()
        val fracPart = chars.takeWhile(c=>c.isDigit).prepended('.')// ddd.ddd
        if (hasChar)
        theChar.toLower match {
          case 'e' =>
            nextChar()
            val expPart =
              if (hasChar) chars.takeWhile(c=>c.isDigit).prepended('e') else(Seq('0'))
            mkReal(intPart ++ fracPart ++ expPart)
          case _ =>
            mkReal(intPart ++ fracPart)
        } else mkReal(intPart ++ fracPart)
      case 'e' =>
        nextChar()
        val expPart = chars.takeWhile(c=>c.isDigit).prepended('e')
        mkReal(intPart ++ expPart)
      case _   =>
        mkDec(intPart)
    } else mkDec(intPart)
  }


  def hasNext: Boolean = chars.hasCurrent

  def next(): Token = if (hasChar) {
    lastLocation = SourceLocation(chars.lines,  chars.chars)
    theChar match {
      case c if c.isLetter =>
        val prefix = chars.takeWhile(_.isLetterOrDigit)
        val string = prefix.mkString("")
        symbolMap.getOrElse(string, mkIDENTIFIER(string))

      case '0' =>
        nextChar()
        theChar.toLower match {
          case 'x' =>
            nextChar()
            mkHex(chars.takeWhile(c=>c.isDigit || ("abcdef" contains c.toLower)))
          case other =>
            nextNumber(chars.takeWhile(c=>c.isDigit).prepended('0'))
        }
      case c if c.isDigit =>
        val intPart = chars.takeWhile(c=>c.isDigit)
        nextNumber(intPart)

      // When a newline appears return NEWLINE (if it's defined) without actually reading ahead.
      // and pretend that the read ahead got a space, so that the subsequent next() skips this space
      case c if c=='\n'     =>
        chars.current = ' '
        if (NEWLINE.isDefined) NEWLINE.get else next()

      case c if c.isWhitespace =>
          while (hasChar && theChar.isWhitespace) nextChar()
          if (hasChar) next() else ENDSTREAM

      case '/' =>
        nextChar()
        theChar match {
          case '*' =>
            eatComment()
            next()
          case '/' =>
            chars.dropWhile( c=>c!='\n')
            next()
          case other =>
            tokenMap.longestPrefixMatch(s"/$other") match {
              case None => mkERROR(s"at: /$other")
              case Some((tok, edges)) =>
                tok
            }
        }
        
      case '«' =>
        nextChar(); afterNextChar(mkString("«", "»", chars.takeNested2('«', '»')))
      case '“' =>
        nextChar(); afterNextChar(mkString("“", "“", chars.takeNested2('“', '“')))
      case '"' =>
        nextChar(); afterNextChar(mkString("\"", "\"", chars.takeWhile( c => c!='"')))
      case '\'' =>
        nextChar(); afterNextChar(mkString("'", "'", chars.takeWhile( c => c!='\'')))

      case c  =>
        tokenMap.longestPrefixMatch(chars) match {
          case None =>
            mkERROR(s"no lexical token begins with the character `$c`")
          case Some((tok, edges)) =>
            tok
        }
    }
  } else ENDSTREAM
}