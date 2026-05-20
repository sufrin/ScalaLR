package org.sufrin.scalalr
package stage2

object AST {

  import org.sufrin.scalalr._


  val mangleDollar = "dol$"

  def mangle(name: String): String = mangleDollar++name
  def mangle(name: Name): String   = mangleDollar++name.withoutQuotes // Design decision

  case class Expression(val text: String)  {
    override def toString: String = text.trim
    def mangle: String = text.trim.replace("$", mangleDollar)
  }

  class Terminal(val name: String)  {
    override def toString: String = name
  }

  trait SymbolType {
    def sourceTypeName: String
    def scalaTypeName: String
    def scalaParameterTypeName: String
    def sourceParameterTypeName: String
  }

  case class Type(name: String, parameters: Seq[SymbolType], location: SourceLocation)  extends SymbolType {
    override val toString: String = if (parameters.isEmpty) s"$name$location" else parameters.map(_.toString).mkString(s"$name[", ",", s"]$location")
    def scalaTypeName: String = if (parameters.isEmpty) name else  parameters.map(_.scalaParameterTypeName).mkString(s"$name[", ",", "]")
    def scalaParameterTypeName: String = s"$scalaTypeName @unchecked"
    def sourceTypeName: String = if (parameters.isEmpty) name else  parameters.map(_.sourceParameterTypeName).mkString(s"$name[", ",", "]")
    def sourceParameterTypeName: String = s"$sourceTypeName"
  }

  case object NoType extends SymbolType {
    def isUntyped: Boolean = false
    def scalaTypeName: String = "_"
    def scalaParameterTypeName: String = "_"
    def sourceTypeName: String = "NoType"
    def sourceParameterTypeName: String = "_"
  }


  case class Rule(lhs: TypedNonterminal, rhs: Seq[Production], location: SourceLocation) {
    override def toString(): String = s"$lhs = $rhs"
  }

  case class Production(symbols:    Seq[NamedField],
                        reduction:  Option[Expression],
                        precedence: Option[Name],
                        location:   SourceLocation) {
    val code = if (reduction.isDefined) s" { ${reduction.get} } " else ""
    val prec = if (precedence.isDefined) s" %prec ${precedence.get}" else ""

    override def toString: String = s"${symbols.map(_.toString).mkString(" ")}$code$prec"

  }

  case class Error(message: String)

  case class Notation
  (thePackage:      String  = "",
   theName:         String  = "",
   theExplicitPath: String  = "components",
   tablesType:      String  = "lalr",
   theScannerName:  String  = "Scanner",
   theTokenType:    Type    = Type(name="Token", parameters = Nil, location = SourceLocation(-1, -1)),
   theTokens:       List[TokenSpec]  = Nil,
   theRules:        List[Rule]       = Nil,
   theTokensInclude:  String         = "",
   theRulesInclude:   String         = "",
   theSignature:      String         = ""
  ) {
      lazy val declaredTokens:       Seq[TokenSpec]        = theTokens.reverse
      lazy val declaredTerminals:    Seq[TypedTerminal]    = declaredTokens.flatMap(_.terminals)
      lazy val declaredNonterminals: Seq[TypedNonterminal] = for { rule <- theRules } yield rule.lhs
  }


  trait Symbol {
    def theName:        Name
  }

  case class Name(unQuoted: String, isQuoted: Boolean, location: SourceLocation = SourceLocation(-1, -1)) {
    override val toString: String = if (isQuoted) s"`$unQuoted`" else unQuoted
    lazy val toFullString: String = s"$toString@${location.line}.${location.col}"
    val forScala: String = toString
    def asPath: String = unQuoted.replace('/', '.').replace('.', '/')
    def warnQuoted: Name = {
      if (isQuoted) println(s"WARNING: Quoted symbol $this at ${location.line}.${location.col} where a plain name is required")
      this
    }

    def withoutQuotes: String = {
      if (isQuoted) println(s"WARNING: Quoted symbol $this at ${location.line}.${location.col} where a plain name is required")
      unQuoted
    }

    override def equals(other: Any): Boolean = other match {
      case that: Name =>
        this.unQuoted == that.unQuoted && this.isQuoted == that.isQuoted  // ignore location
      case _ => false
    }

    override def hashCode(): Int =
      unQuoted.hashCode
  }

  case class TypedTerminal(theName: Name, theType: SymbolType=NoType, location: SourceLocation) extends Symbol {
    def isTyped: Boolean    = theType!=NoType
    def theTypeName: String = theType.toString
    def theScalaTypeName: String = theType.scalaTypeName
    def sourceTypeName: String = theTypeName
  }

  case class TypedNonterminal(theName: Name, theType: SymbolType=NoType, location: SourceLocation) extends Symbol {
    override def toString: String =
      if (theType==NoType) theName.toString else s"$theName: ${theType.sourceTypeName}"
  }

  case class NamedField(theFieldName: Option[Name], theField: Name, location: SourceLocation) {
    override def toString: String = if (theFieldName.isDefined) s"${theFieldName.get}: $theField" else theField.toString
    def isAnonymous: Boolean = !theFieldName.isDefined
    def sameField(that: NamedField): Boolean = this.theField.equals(that.theField)
    def sameFieldType(that: NamedField): Boolean = this.theField.equals(that.theField)
  }

  implicit class StringExtras(s: String) {
    def isQuoted: Boolean = s.matches("\".+\"")
  }

  trait TokenSpec {
    val terminals: Seq[TypedTerminal]
  }
  
  case class Left(terminals:     Seq[TypedTerminal]) extends TokenSpec {
  }
  
  case class Right(terminals:    Seq[TypedTerminal]) extends TokenSpec  {
  }
  
  case class Nonassoc(terminals: Seq[TypedTerminal]) extends TokenSpec {
  }
  
  case class Tokens(terminals:   Seq[TypedTerminal]) extends TokenSpec {
  }

  case class Prec(terminals:   Seq[TypedTerminal]) extends TokenSpec {
  }

  trait Repeat
  case object MaybeOne   extends Repeat { override val toString: String = "?" }
  case object OneOrMore  extends Repeat { override val toString: String = "+" }
  case object NoneOrMore extends Repeat { override val toString: String = "*" }

}