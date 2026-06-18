package org.sufrin.scalalr

import org.sufrin.utility.SourceTextCursor

object SimpleScannerCore {
  case class UNDEFINED(why: String) extends java.lang.Error
}

/**
 * A simple-to-specify lexical scanner core for little languages that is intended
 * to be straightforward to define within the  language's notation's
 * (first) `%include { ... }`, and thereby to be included in
 * its ScalaLR-generated `Scanner` object. It may also be used
 * from elsewhere.
 *
 * It is expected that one or more of the following handlers for
 * values yielded by the underlying `ScannerCore` will
 * be overridden by the names of "value carrying" tokens
 * in a ScalaLR-generated `Scanner` object; and that
 * `TOKENMAP: Map[String, Token]` will be defined as
 * `TokenMap` from the ScalaLR-generated `Scanner` object.
 *
 * {{{
 * val STRING:   String => Token  = UNDEFINED(s"No ScannerAdapter handler for STRING")(_)
 * val LONG:     Long   => Token  = UNDEFINED(s"No ScannerAdapter handler for LONG")(_)
 * val IDENTIFIER:     String => Token  = UNDEFINED(s"No ScannerAdapter handler for IDENTIFIER")(_)
 * val DOUBLE:   Double => Token  = UNDEFINED(s"No ScannerAdapter handler for DOUBLE")(_)
 * def TOKENMAP: Map[String, Token]
 * }}}
 *
 * `NEWLINE`  should be overridden as `Some(token)` if line endings are to be treated as significant.
 *
 * `UNDEFINED` can be overridden: useful if the scanner shouldn't "bale" on a lexical error.
 *
 * @param chars - source of the input text
 * @tparam Token - type of the returned tokens
 */
abstract class SimpleScannerCore[Token <: Lexeme](chars: SourceTextCursor) extends ScannerCore[Token](chars) {
  val STRING:     String => Token  = UNDEFINED(s"No SimpleScannerCore handler for STRING")(_)
  val LONG:       Long   => Token  = UNDEFINED(s"No SimpleScannerCore handler for LONG")(_)
  val IDENTIFIER: String => Token  = UNDEFINED(s"No SimpleScannerCore handler for IDENTIFIER")(_)
  val DOUBLE:     Double => Token  = UNDEFINED(s"No SimpleScannerCore handler for DOUBLE")(_)
  def TOKENMAP: Map[String, Token]

  def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
  def mkHex(source: Seq[Char]):        Token  = LONG(source.foldLeft(0L) { (acc, c) => acc * 16 + Character.digit(c, 16) })
  def mkDec(source: Seq[Char]):        Token  = LONG(source.mkString.toLong)
  def mkReal(source: Seq[Char]):       Token  = DOUBLE(source.mkString.toDouble)
  def mkIDENTIFIER(source: Seq[Char]): Token  = IDENTIFIER(source.mkString)
  def mkERROR(source: Seq[Char]):      Token  = UNDEFINED(s"No SimpleScannerCore handler for mkERROR")(source)
  val NEWLINE:                  Option[Token] = None

  /**
   *  Default handler for all lexical strings separated by the superclass:
   *  prints the `message`, then throws  `SimpleScannerCore.UNDEFINED(message)`.
   */
  def UNDEFINED(message: String)(value: Any):  Token  = {
    val msg = s"$message: ${value.toString}"
    println(msg)
    throw SimpleScannerCore.UNDEFINED(msg)
  }

  // Build the tokenMapping trie left unpopulated by ScannerCore
  locally {
    defineSymbolTokens(TOKENMAP)
  }
}



