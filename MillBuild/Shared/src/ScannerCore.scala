package org.sufrin.scalalr

import org.sufrin.utility.SourceTextCursor

/**
 * A `Scanner` is a `Token` `Iterator` that supplies source locations.
 * In general `sourceLocation()` yields the starting source location of the latest `Token` to be
 * returned.
 *
 *
 */
trait Scanner[Token]  extends Iterator[Token] {
  def sourceLocation(): SourceLocation

  def prompt(): Unit
}

/** An extensible scanner is a scanner with additional features that
 * support its parameterization by a straightforward mapping and/or a
 * prefix map.
 *
 * The former is used when the scanner has completely isolated a String from its subject text.
 * The latter is used when the scanner needs incremental access to the representation
 * because it is seeking to isolate the longest prefix of the remaining subject text that
 * has a corresponding token.
 */
trait ExtensibleScanner[Token] extends Scanner[Token] {
  /** If `name` corresponds to a `token: Token` in the `PrefixMap` or in the `Map` . yield `Some(token)` else yield `None` */
  def getToken(name: String): Option[Token]

  def setPrefixMap(name: String, token: Token): Option[Token]
  def setMap(name: String, token: Token): Option[Token]
  def withSymbolTokens(symbolToken: Map[String, Token]): this.type
  def defineSymbolTokens(symbolToken: Map[String, Token]): Unit
}

/**
 * A builder for parameterisable lexical scanners. Simplicity is the
 * watchword so as to keep the API small.
 *
 * It is expected that the following "token-handlers" will all be defined. They are intended
 * to map a sequence of characters that corresponds to a scanned symbol  to the corresponding  `Token`
 *
 * {{{
 *   // Token from a "quoted" text. It is the client's responsibility to process any "escape" sequences in `body`
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
abstract class ScannerCore[Token <: Lexeme](chars: SourceTextCursor) extends ExtensibleScanner[Token] {
  import org.sufrin.utility.CharSequenceMap

  import scala.collection.mutable
  /**
   * Token from a "quoted" text. Client has responsibility for processing any "escape" sequences in `body` ,
   * and we don't define an `escape` character here.
   *
   * But we provide several different pairs of open/close quote in addition to the usual `'` and "` , namely
   * properly nested: `("«", "»")`, and `("“", "“")`
   *
   */
  def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token
  /** Token from a text of the form `0x[hexit]+` */
  def mkHex(source: Seq[Char]):   Token
  /** Token from a text of the form `[digit]+` */
  def mkDec(source: Seq[Char]):   Token
  /** Token from a text of one of the forms:
   *       intpart . fracpart exp [-]? exppart
   *       intpart exp  [-]? exppart
   *  where
   *       intpart  = [digit]+
   *       fracpart = [digit]*
   *       exppart  = [digit]+
   * and
   *       exp      = [Ee]
   */
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

  /** overrideable: so that some lexical context can be kept  */
  def allowNL: Boolean           = true

  /** PrefixMap mapping names to the tokens they denote: accessible incrementally  */
  val tokenPrefixMap: CharSequenceMap[Token] = new CharSequenceMap[Token]

  /** Mapping from names to the tokens they denote */
  val tokenMap: collection.mutable.Map[String,Token] = new mutable.LinkedHashMap[String,Token]

  def defineSymbolTokens(symbolToken: Map[String, Token]): Unit = {
    withSymbolTokens(symbolToken);
    ()
  }

  /** Initialize the token and symbol mappings using the mapping supplied by `ScalaLR` .
   *  All symbols that will be seen as Letter LetterOrDigit* go into the symbol map.
   *  All symbols (~LetterOrDigit)+ go into the token PrefixMap map.
   *  Other symbols are ignored: ie there is no provision for hybrids.
   */
  def withSymbolTokens(symbolToken: Map[String, Token]): this.type = {
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall(_.isLetterOrDigit)} tokenMap(symbol) = token
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall { c => ! c.isLetterOrDigit}} tokenPrefixMap(symbol) = token
    this
  }

  def getToken(name: String): Option[Token] =
    tokenPrefixMap.get(name) match {
      case None    => tokenMap.get(name)
      case defined => defined
    }

  def setPrefixMap(name: String, token: Token): Option[Token] =  {
    val result = tokenPrefixMap.get(name)
    tokenPrefixMap(name) = token
    result
  }

  def setMap(name: String, token: Token): Option[Token]  =  {
    val result = tokenMap.get(name)
    tokenMap(name) = token
    result
  }

  def prompt(): Unit = { print(chars.promptString); System.out.flush() }

  var lastLocation: SourceLocation = SourceLocation(chars.lines,  chars.chars, chars.path)
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
    if (hasChar && (theChar=='.' || theChar=='e')) {
      val dot      = chars.takeIf(c=>c=='.')
      val fracPart = dot ++ chars.takeWhile(c=>c.isDigit)
      val exp = chars.takeIf(c=>c.toLower=='e')
      val neg = chars.takeIf(c=>c=='-')
      val expPart = if (exp.isEmpty) Seq() else exp ++ neg ++ chars.takeWhile(c=>c.isDigit)
      mkReal(intPart ++ fracPart ++ expPart)
    } else  mkDec(intPart)
  }


  def hasNext: Boolean = chars.hasCurrent

  def next(): Token = if (hasChar) {
    lastLocation = SourceLocation(chars.lines,  chars.chars)
    theChar match {
      case c if c.isLetter =>
        val prefix = chars.takeWhile(_.isLetterOrDigit)
        val string = prefix.mkString("")
        tokenMap.getOrElse(string, mkIDENTIFIER(string))

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

      case '\u0000' =>
        nextChar()
        if (hasChar) next() else ENDSTREAM

      // When a newline appears return NEWLINE (if it's defined) without actually reading ahead.
      // and pretend that the read ahead got a nul, so that the subsequent next() skips this nul
      case c if c=='\n' && allowNL   =>
        chars.current = '\u0000'
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
            tokenPrefixMap.longestPrefixMatch(s"/$other") match {
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
        tokenPrefixMap.longestPrefixMatch(chars) match {
          case None =>
            nextChar()
            mkERROR(s"No token starting `$c` can be completed here.")
          case Some((tok, edges)) =>
            tok
        }
    }
  } else ENDSTREAM
}