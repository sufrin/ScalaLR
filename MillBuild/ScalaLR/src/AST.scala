package org.sufrin.scalalr
package stage2

object AST {

  import org.sufrin.scalalr._


  val mangleDollar = "dol$"

  def mangle(name: String): String = mangleDollar++name
  def mangle(name: Name): String   = mangleDollar++name.withoutQuotes // Design decision


  class Terminal(val name: String)  {
    override def toString: String = name
  }

  trait SymbolType {
    def sourceTypeName: String
    def scalaTypeName: String
    def scalaParameterTypeName: String
    def sourceParameterTypeName: String
    def isNoType: Boolean = false
  }

  case class Type(name: String, parameters: Seq[SymbolType], location: SourceLocation)  extends SymbolType {
    override val toString: String = if (parameters.isEmpty) s"$name$location" else parameters.map(_.toString).mkString(s"$name[", ",", s"]$location")
    def scalaTypeName: String = if (parameters.isEmpty) name else  parameters.map(_.scalaParameterTypeName).mkString(s"$name[", ",", "]")
    def scalaParameterTypeName: String = s"$scalaTypeName @unchecked"
    def sourceTypeName: String = if (parameters.isEmpty) name else  parameters.map(_.sourceParameterTypeName).mkString(s"$name[", ",", "]")
    def sourceParameterTypeName: String = s"$scalaTypeName"
  }

  case object NoType extends SymbolType {
    def isUntyped: Boolean = false
    def scalaTypeName: String = "Unit"
    def scalaParameterTypeName: String = "Unit @unchecked"
    def sourceTypeName: String = "Unit"
    def sourceParameterTypeName: String = "Unit"
    override def isNoType: Boolean = true

  }

  case class TypeVariable(forName: Name) extends SymbolType {
    def sourceTypeName: String = delegated.sourceTypeName
    def scalaTypeName: String = delegated.scalaTypeName
    def scalaParameterTypeName: String = delegated.scalaParameterTypeName
    def sourceParameterTypeName: String = delegated.scalaParameterTypeName
    def delegated: SymbolType = delegate.getOrElse(NoType)
    var delegate: Option[SymbolType] = None
  }


  case class Rule(lhs: TypedNonterminal, rhs: Seq[Production], location: SourceLocation) {
    override def toString(): String = s"$lhs = $rhs"
  }

  case class Production(symbols:    Seq[NamedField],
                        reduction:  Option[Expression],
                        precedence: Option[Name],
                        location:   SourceLocation) {
    val code = // if (reduction.isDefined) s" { ${reduction.get} } " else ""
      reduction match {
        case None => ""
        case Some(CodeExpression(text))      => s" { ${text} } "
        case Some(ScalaExpression(scala, _)) => s" => ${scala.forScala} "
      }
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
   inferEnabled:    List[String]     = Nil,
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
    val forScala: String = toString.replace("\\","\\\\")
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
        this.toString == that.toString // this.unQuoted == that.unQuoted && this.isQuoted == that.isQuoted  // ignore location
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
    override def equals(other: Any): Boolean = other match {
      case that: TypedTerminal =>
        this.theName equals that.theName
      case _ => false
    }
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

  /**
   * Declarations relating to types/associativity terminal symbols
   * {{{
   *     data TokenSpec = Left(Seq[TypedTerminal])
   *                    | Right(Seq[TypedTerminal])
   *                    | Nonassoc(Seq[TypedTerminal])
   *                    | Tokens(Seq[TypedTerminal])
   *                    | Precedence(Seq[TypedTerminal])
   * }}}
   * */
  sealed trait TokenSpec {
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

  case class Precedence(terminals:   Seq[TypedTerminal]) extends TokenSpec {
  }

  /**
   * Repeat factor for a repeated phrase
   *
   * {{{
   *   data Repeat =
   *      MaybeOne | OneOrMore | Ellipsis | NoneOrMore
   *         ?          +           ...         *
   *    | RightOneOrMore | RightNoneOrMore
   *         +..               *..
   *
   * }}}
   */
  sealed trait Repeat
  case object MaybeOne          extends Repeat { override val toString: String = "?" }
  case object OneOrMore         extends Repeat { override val toString: String = "+" }
  case object Ellipsis          extends Repeat { override val toString: String = s"..." } // left-recursive implementation with  separator as optional terminator
  case object NoneOrMore        extends Repeat { override val toString: String = "*" }
  case object RightNoneOrMore   extends Repeat { override val toString: String = "*.." }  // right-recursive implementation
  case object RightOneOrMore    extends Repeat { override val toString: String = "+.." }  // right-recursive implementation


  /**
   * The result of a production
   */
  sealed trait Expression {
    def text: String
    override def toString: String = text.trim
    def mangle: String = text.trim.replace("$", mangleDollar)
  }

  /** A code literal represneted as text */
  case class CodeExpression(val text: String)  extends Expression {}

  /** A code literal represented as a simple scala expression */
  case class ScalaExpression(scala: Scala, START: SourceLocation)  extends Expression {
    val text: String = scala.forScala
  }

  /**
   * Representation of a (stuctured and checkable) Scala expression
   */
  sealed trait Scala {
    /** Text destined for Scala compilation */
    val forScala:  String
    /** free variables of the expression */
    def free:      List[Name]
    /** '$'-decorated variables of the expression */
    def decorated: List[Name]
    /** Could this denote a constructor */
    def isConst: Boolean = false
    val START: SourceLocation = SourceLocation(0,0)
  }

  case class Id(name: Name, override val START: SourceLocation) extends Scala {
    val forScala = if (name.isQuoted) s""""${name.unQuoted}"""" else name.unQuoted
    def free: List[Name] = List(name)
    def decorated: List[Name] = Nil
    override def isConst: Boolean = true
  }

  /** A `$`-decorated scala expression */
  case class Dollar(scala: Scala) extends Scala {
    val forScala = s"$$${scala.forScala}"
    def free: List[Name] = scala.free
    override def decorated: List[Name] = scala.free
  }

  case class Num(forScala: String, override val START: SourceLocation) extends Scala {
    def free: List[Name] = Nil
    def decorated: List[Name] = Nil
  }

  case class ScalaString(string: String, override val START: SourceLocation) extends Scala {
    val forScala = s""""$string""""
    def free: List[Name] = Nil
    def decorated: List[Name] = Nil
  }

  /** an n-tuple */
  case class Bra(scalas: List[Scala]) extends Scala {
    val forScala = s"(${scalas.map(_.forScala).mkString(",")})"
    def free: List[Name] = scalas.flatMap(_.free)
    def decorated: List[Name] = scalas.flatMap(_.decorated)
  }

  /** `obj.feature` with the `obj` as a source of free/decorated variables*/
  case class Dot(obj: Scala, feature: Scala) extends Scala {
    val forScala: String      = s"${obj.forScala}.${feature.forScala}"
    def free: List[Name]      = obj.free

    def decorated: List[Name] = obj.decorated

  }

  case class Infix(op: String, l: Scala, r: Scala) extends Scala {
    val forScala: String = s"${l.forScala}$op${r.forScala}"
    def free: List[Name] = l.free ++ r.free
    def decorated: List[Name] = l.decorated++r.decorated

  }

  case class Apply(path: Scala, args: Seq[Scala]) extends Scala {
    val forScala: String = s"${path.forScala}(${args.map(_.forScala).mkString(", ")})"

    def free: List[Name] = {
      path match {
        case Id(name, _) => (if (name.unQuoted(0).isUpper) Nil else path.free) ++ args.flatMap(_.free)
        case _ => path.free ++ args.flatMap(_.free)
      }
    }
    def decorated: List[Name] = path.decorated ++ args.flatMap(_.decorated)
  }

  /** Synthetic only: a call to path with parameter values passed by equals */
  case class ApplyNamed(path: Scala, args: Seq[Name]) extends Scala {
    def toEquation(name: Name): String = s"${name.forScala} = $$${name.forScala}"
    val forScala: String = s"${path.forScala}(${args.map(toEquation).mkString(", ")})"

    def free: List[Name]      = args.toList
    def decorated: List[Name] = args.toList
  }

  /** obj.feature(args...) with `obj` and `args` as sources of free/decorated variables */
  case class MethodApply(obj: Scala, feature: Scala, args: Seq[Scala]) extends Scala {
      val forScala: String      = s"${obj.forScala}.${feature.forScala}(${args.map(_.forScala).mkString(", ")})"
      def free: List[Name]      =  obj.free ++ args.flatMap(_.free)
      def decorated: List[Name] = obj.decorated ++ args.flatMap(_.decorated)

  }


}