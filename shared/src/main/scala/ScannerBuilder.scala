
import org.sufrin.scalalr.SourceLocation
import org.sufrin.utility.SourceTextCursor
import org.sufrin.scalalr.Lexeme

/**
 * Builder for lexical scanners.
 */

abstract class ScannerBuilder[Token <: Lexeme](chars: SourceTextCursor) extends Iterator[Token] {
  import org.sufrin.utility.CharSequenceMap
  import scala.collection.mutable

  def NUM(radix: Int, value: Long): Token
  def ID(name: String): Token
  def LEXICALERROR(str: String): Token
  case object ENDSTREAM extends Token { val value = (); val symbol = 0 }

  /** Trie mapping from (non-alphabetic) names to the tokens they denote  */
  val tokenMap: CharSequenceMap[Token] = new CharSequenceMap[Token]

  /** Mapping from (alphabetic) names to the tokens they denote */
  val symbolMap: collection.mutable.Map[String,Token] = new mutable.LinkedHashMap[String,Token]

  /** Initialize the token and symbol mappings using the mapping supplied by `ScalaLR` */
  def withSymbolTokens(symbolToken: Map[String, Token]): this.type = {
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall(_.isLetterOrDigit)} symbolMap(symbol) = token
    for { (symbol, token) <- symbolToken if symbol.nonEmpty && symbol.forall { c => ! c.isLetterOrDigit}} tokenMap(symbol) = token

    {
      import org.sufrin.utility.PrettyPrint._
      tokenMap.prettyPrint()
    }
    this
  }


  def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
  @inline def hasChar: Boolean = chars.hasCurrent
  @inline def theChar: Char = chars.current
  @inline def nextChar(): Unit = chars.next()
  @inline def afterNextChar(t: Token): Token = {
    nextChar()
    t
  }

  def hex(chars: Seq[Char]): Long  = chars.foldLeft(0L) { (acc, c) => acc * 16 + Character.digit(c, 16) }
  def dec(chars: Seq[Char]): Long  = chars.foldLeft(0L) { (acc, c) => acc * 10 + Character.digit(c, 10) }

  def eatComment(): Unit = {
    var level = 0
    var go = true
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
    nextChar()
    eatWhitespace()
  }

  def eatWhitespace(): Unit = {
    while (hasChar && theChar.isWhitespace) nextChar()
  }


  def hasNext: Boolean = chars.hasCurrent
  def next(): Token = if (hasChar) {
    theChar match {
      case c if c.isLetter =>
        val prefix = chars.takeWhile(_.isLetterOrDigit)
        val string = prefix.mkString("")
        symbolMap.getOrElse(string, ID(string))

      case '0' =>
        nextChar()
        theChar.toLower match {
          case 'x' =>
            nextChar()
            val prefix = chars.takeWhile(c=>c.isDigit || ("abcdef" contains c.toLower))
            NUM(16, hex(prefix))
          case other =>
            val prefix = chars.takeWhile(c=>c.isDigit)
            NUM(10, dec(prefix))
        }

      case c if c.isDigit =>
        val prefix = chars.takeWhile(c=>c.isDigit)
        NUM(10, dec(prefix))

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
            eatWhitespace()
            next()
          case other =>
            tokenMap.longestPrefixMatch(s"/$other") match {
              case None => LEXICALERROR(s"at: /$other")
              case Some((tok, edges)) =>
                tok
            }
        }
      case c  =>
        tokenMap.longestPrefixMatch(chars) match {
          case None => LEXICALERROR(s"at: $c")
          case Some((tok, edges)) =>
            tok
        }
    }
  } else ENDSTREAM
}