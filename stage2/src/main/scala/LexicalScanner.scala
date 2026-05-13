
package org.sufrin.scalalr
package stage2

import org.sufrin.utility.SourceTextCursor
import org.sufrin.scalalr.SourceLocation

object LexicalScanner {
  import scalalr.stage2.Scanner._


  type Token = scalalr.stage2.Scanner.Token

  def apply(chars: SourceTextCursor): LexicalScanner = new LexicalScanner(chars)


  class LexicalScanner(chars: SourceTextCursor) extends Iterator[Token] {
    def sourceLocation(): SourceLocation = SourceLocation(chars.lines,  chars.chars)
    @inline def hasChar: Boolean = chars.hasCurrent
    @inline def theChar: Char = chars.current
    @inline def nextChar(): Unit = chars.next()
    @inline def afterNextChar(t: Token): Token = { nextChar(); t }
    def makeID(unQuoted: String, isQuoted: Boolean): ID =
        ID(AST.Name(unQuoted, isQuoted, sourceLocation()))

    var enableSEPARATOR = false

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
      eatWhitespace()
    }

    def eatWhitespace(): Unit = {
      while (hasChar && theChar.isWhitespace) nextChar()
    }

    def isBisonic(c: Char): Boolean = c.isLetterOrDigit || c=='.' || c=='_'

    def hasNext: Boolean = chars.hasCurrent
    def next(): Token = {
      val s = nnext()
      //println(s)
      s
    }
    def nnext(): Token = if (hasChar) {
      chars.current match {
        case '(' => afterNextChar(`(`)
        case ')' => afterNextChar(`)`)
        case '[' => afterNextChar(`[`)
        case ']' => afterNextChar(`]`)
        case '|' => afterNextChar(`|`)
        case '=' => afterNextChar(`=`)
        case ',' => afterNextChar(`,`)
        case ':' => afterNextChar(`:`)
        case '%' =>
          nextChar()
          val directive = chars.takeWhile(_.isLetterOrDigit).mkString("")
          directive.toLowerCase match {
            case "type"         => `%type`
            case "empty"        => `%empty`
            case "notation"     => `%notation`
            case "package"      => `%package`
            case "token"        => `%token`
            case "left"         => `%left`
            case "right"        => `%right`
            case "non"          => `%non`
            case "include"      => `%include`
            case "path"         => `%path`
            case "dialect"      => `%dialect`
            case "scalalr"      => `%scalalr`
            case "tables"       => `%tables`
            case "signature"    => `%signature`
            case "prec"         => `%prec`
            case "rules"        =>
              enableSEPARATOR = true
              eatWhitespace()
              `%rules`
            case _ => LEXICALERROR(s"Unknown directive %$directive (at ${sourceLocation()})")
          }
        case '/' =>
          nextChar()
          theChar match {
            case '*' =>
              eatComment()
            case '/' =>
              chars.dropWhile( c=>c!='\n')
              eatWhitespace()
            case other =>
              //Syntax.Parser.warn(s"Malformed comment sentinel: \"/$other\" at $sourceLocation")
              chars.dropWhile( c=>c!='\n')
          }
          nnext()
        case '{' => // } to balance the %include
          nextChar(); afterNextChar(CODE(chars.takeNested('{', '}')  .mkString("")))
        case '«' => // » to balance the %include
          nextChar(); afterNextChar(CODE(chars.takeNested('«', '»')  .mkString("")))

        case '"'  => nextChar(); afterNextChar(makeID(chars.takeWhile( c => c!='"').mkString(""), true))
        case '\'' => nextChar(); afterNextChar(makeID(chars.takeWhile( c => c!='\'').mkString(""), true))
        case '`' => nextChar(); afterNextChar(makeID(chars.takeWhile( c => c!='`').mkString(""), true))

        case c if c.isLetter =>
          val prefix = chars.takeWhile(isBisonic)
          makeID((prefix).mkString(""), false)

        case ';' =>
          nextChar()
          eatWhitespace()
          SEPARATOR

        case c if c.isWhitespace =>
          var vertical: Int =  0
          while (hasChar && theChar.isWhitespace) {
            if (theChar=='\n') vertical += 1
            nextChar()
          }
          if (enableSEPARATOR && vertical>1) SEPARATOR
          else
            if (hasChar) nnext() else $end
        case other =>
          LEXICALERROR(s"Unrecognised $other (at ${sourceLocation()}")
      }
    } else $end
  }
}
