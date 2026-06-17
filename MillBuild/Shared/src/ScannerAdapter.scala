package org.sufrin.scalalr

import org.sufrin.utility.SourceTextCursor

object ScannerAdapter {
  case class Failure(why: String) extends Error
}

object SimpleScanner {
  case class Failure(why: String) extends Error
}

abstract class ScannerAdapter[Token <: Lexeme](chars: SourceTextCursor) extends ScannerBuilder[Token](chars) {
  def STRING(text: String):     Token  = UNDEFINED(s"No ScannerAdapter handler for STRING($text)")
  def LONG(value: Long):        Token  = UNDEFINED(s"No ScannerAdapter handler for LONG($value)")
  def ID(text: String):         Token  = UNDEFINED(s"No ScannerAdapter handler for ID($text)")
  def DOUBLE(double: Double):   Token  = UNDEFINED(s"No ScannerAdapter handler for DOUBLE($double)")

  def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
  def mkHex(source: Seq[Char]):   Token  = LONG(source.foldLeft(0L) { (acc, c) => acc * 16 + Character.digit(c, 16) })
  def mkDec(source: Seq[Char]):   Token  = LONG(source.mkString.toLong)
  def mkReal(source: Seq[Char]):  Token  = DOUBLE(source.mkString.toDouble)
  def mkID(source: Seq[Char]):    Token  = ID(source.mkString)
  def mkERROR(source: Seq[Char]): Token  = UNDEFINED(s"No ScannerAdapter handler for mkERROR($source)")
  val NEWLINE: Option[Token]             = None
  val ENDSTREAM:                  Token
  def UNDEFINED(reason: String):  Token  = { println(reason); throw ScannerAdapter.Failure(reason) }
}

/**
 * A simple-to-specify Lexer core for a little languages
 * @param chars
 * @tparam Token
 */
abstract class SimpleScanner[Token <: Lexeme](chars: SourceTextCursor) extends ScannerBuilder[Token](chars) {
  val STRING: String => Token  = UNDEFINED(s"No ScannerAdapter handler for STRING")(_)
  val LONG:   Long   => Token  = UNDEFINED(s"No ScannerAdapter handler for LONG")(_)
  val NAME:   String => Token  = UNDEFINED(s"No ScannerAdapter handler for NAME")(_)
  val DOUBLE: Double => Token  = UNDEFINED(s"No ScannerAdapter handler for DOUBLE")(_)

  def mkString(openQuote: String, closeQuote: String, body: Seq[Char]): Token = STRING(body.mkString)
  def mkHex(source: Seq[Char]):   Token  = LONG(source.foldLeft(0L) { (acc, c) => acc * 16 + Character.digit(c, 16) })
  def mkDec(source: Seq[Char]):   Token  = LONG(source.mkString.toLong)
  def mkReal(source: Seq[Char]):  Token  = DOUBLE(source.mkString.toDouble)
  def mkID(source: Seq[Char]):    Token  = NAME(source.mkString)
  def mkERROR(source: Seq[Char]): Token  = UNDEFINED(s"No ScannerAdapter handler for mkERROR")(source)
  val NEWLINE: Option[Token]             = None
  val ENDSTREAM:                  Token
  def UNDEFINED(message: String)(value: Any):  Token  = {
    val msg = s"$message: ${value.toString}"
    println(msg)
    throw SimpleScanner.Failure(msg)
  }
}



