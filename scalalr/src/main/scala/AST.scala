package org.sufrin.scalalr

object AST {

  val mangleDollar = "dol$"
  class Expression(val text: String) extends AnyVal {
    override def toString: String = text
    def mangle: String = text.replace("$", mangleDollar)
  }

  class Terminal(val name: String) extends AnyVal {
    override def toString: String = name
  }

  trait SymbolType
  case class Type(name: String, parameters: Seq[Type]) extends SymbolType {
    override val toString: String = if (parameters.isEmpty) s"$name" else parameters.map(_.toString).mkString(s"$name[", ",", "]")

  }

  case object Untyped extends SymbolType

  case class Rule(lhs: TypedNonterminal, rhs: Seq[Production])

  case class Production(symbols: Seq[NamedField], reduction: Option[Expression], precedence: Option[Terminal]) {
    val code = if (reduction.isDefined) s" {${reduction.get}}" else ""
    val prec = if (precedence.isDefined) s" %prec ${precedence.get}" else ""

    override def toString: String = s"${symbols.map(_.toString).mkString(" ")}$code$prec"
  }

  case class Error(message: String)

  case class Notation
  (thePackage: String,
   theName: String,
   theExplicitPath: String,
   tablesType: String,
   theScannerName: String,
   theTokenType: Type,
   theTokens: Seq[TokenSpec],
   theRules: Seq[Rule],
   theTokensInclude: String,
   theRulesInclude: String)


  trait Symbol { def theName: String }

  case class TypedTerminal(theName: String, theType: SymbolType=Untyped) extends Symbol {
    def isTyped: Boolean = theType!=Untyped
    def theTypeName: String = theType.toString
  }

  case class TypedNonterminal(theName: String, theType: SymbolType=Untyped) extends Symbol {
    override def toString: String =
      if (theType==Untyped) theName else s"$theName: ${theType.toString}"
  }

  case class NamedField(theName: Option[String], fieldSymbol: String) {
    override def toString: String = if (theName.isDefined) s"${theName.get}: $fieldSymbol" else fieldSymbol
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