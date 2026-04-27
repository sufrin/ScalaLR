package org.sufrin.scalalr

object AST {

  import org.sufrin.scalalr.bootstrap.Syntax.{Parser => Boot}
  import org.sufrin.scalalr.bootstrap.{Syntax => BootSyntax}

  val mangleDollar = "dol$"

  class Expression(val text: String)  {
    override def toString: String = text
    def mangle: String = text.replace("$", mangleDollar)
    def toBootstrapNotation: BootSyntax.Expression = new BootSyntax.Expression(text)
  }


  class Terminal(val name: String) extends AnyVal {
    override def toString: String = name
    def toBootstrapNotation: BootSyntax.Terminal = new BootSyntax.Terminal(name) // TODO: used only in %prec directives?

  }

  trait SymbolType { def toBootstrapNotation: BootSyntax.Type }

  case class Type(name: String, parameters: Seq[Type], location: SourceLocation) extends SymbolType {
    override val toString: String = if (parameters.isEmpty) s"$name" else parameters.map(_.toString).mkString(s"$name[", ",", "]")
    def toBootstrapNotation: BootSyntax.Type = new BootSyntax.Type(toString)

  }

  case object Untyped extends SymbolType {
    def toBootstrapNotation: BootSyntax.Type = Boot.noType
  }


  case class Rule(lhs: TypedNonterminal, rhs: Seq[Production], location: SourceLocation) {
    def toBootstrapNotation: Boot.Rule = Boot.Rule(lhs.toBootstrapNotation, rhs.map(_.toBootstrapNotation))
  }

  case class Production(symbols:    Seq[NamedField],
                        reduction:  Option[Expression],
                        precedence: Option[Terminal],
                        location:   SourceLocation) {
    val code = if (reduction.isDefined) s" ${reduction.get}" else ""
    val prec = if (precedence.isDefined) s" %prec ${precedence.get}" else ""

    override def toString: String = s"${symbols.map(_.toString).mkString(" ")}$code$prec"

    def toBootstrapNotation: Boot.Production =
      Boot.Production(
        symbols.map(_.toBootstrapNotation),
        Some(new BootSyntax.Expression(code)),// reduction.map(_.toBootstrapNotation), // TODO: WHAT HAPPENNS WHEN CODE IS EMPTY
        precedence.map(_.toBootstrapNotation))
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
   theRulesInclude: String,
   theDialects: (String, String)
  )


  trait Symbol { def theName: String }

  case class TypedTerminal(theName: String, theType: SymbolType=Untyped, location: SourceLocation) extends Symbol {
    def isTyped: Boolean = theType!=Untyped
    def theTypeName: String = theType.toString
    def toBootstrapNotation: Boot.TypedTerminal =
      if (isTyped) Boot.TypedTerminal(theName, new BootSyntax.Type(theTypeName)) else Boot.TypedTerminal(theName)
  }

  case class TypedNonterminal(theName: String, theType: SymbolType=Untyped, location: SourceLocation) extends Symbol {
    override def toString: String =
      if (theType==Untyped) theName else s"$theName: ${theType.toString}"

    def toBootstrapNotation: Boot.TypedNonterminal =
      Boot.TypedNonterminal(theName, theType.toBootstrapNotation)
  }

  case class NamedField(theName: Option[String], fieldSymbol: String, location: SourceLocation) {
    override def toString: String = if (theName.isDefined) s"${theName.get}: $fieldSymbol" else fieldSymbol
    def toBootstrapNotation: Boot.NamedField =
      Boot.NamedField(theName, fieldSymbol)
  }

  implicit class StringExtras(s: String) {
    def isQuoted: Boolean = s.matches("\".+\"")
  }

  trait TokenSpec {
    val terminals: Seq[TypedTerminal]
    def toBootstrapNotation: Boot.TokenSpec
  }
  
  case class Left(terminals:     Seq[TypedTerminal]) extends TokenSpec {
    def toBootstrapNotation = Boot.Left(terminals.map(_.toBootstrapNotation))
  }
  
  case class Right(terminals:    Seq[TypedTerminal]) extends TokenSpec  {
    def toBootstrapNotation = Boot.Right(terminals.map(_.toBootstrapNotation))
  }
  
  case class Nonassoc(terminals: Seq[TypedTerminal]) extends TokenSpec {
    def toBootstrapNotation = Boot.Nonassoc(terminals.map(_.toBootstrapNotation))
  }
  
  case class Tokens(terminals:   Seq[TypedTerminal]) extends TokenSpec {
    def toBootstrapNotation = Boot.Tokens(terminals.map(_.toBootstrapNotation))
  }


}