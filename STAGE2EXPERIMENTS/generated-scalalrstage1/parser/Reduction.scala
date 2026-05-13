
package scalalr.stage2
object Reduction {

 import org.sufrin.scalalr.stage2.AST._
 import org.sufrin.scalalr.stage2.Generator._
 import scalalr.stage2.Scanner
 import org.sufrin.utility.SourceTextCursor
 import org.sufrin.scalalr.SourceLocation
 import org.sufrin.utility.PrettyPrint._

 def makeTupleType(types: Seq[Type], location: SourceLocation): Type =
     types.size match {
       case 1 => types(0)
       case n => Type(s"Tuple$n", types, location)
     }

 def mkTableType(tableTypeName: String): String =
     tableTypeName match {
        case "lr"          => "canonical-lr"
        case "canonical"   => "canonical-lr"
        case "ielr"        => "ielr"
        case "lalr"        => "lalr"
        case _      => println(s"Warning: wrong %tables type $tableTypeName; canonical assumed");  "canonical-lr"
     }

 implicit class StringOps(val string: String) extends AnyVal {
          def unQuoted: String = string  match {
              case s"\"$unquoted\"" => unquoted
              case unquoted => unquoted
          }

          def asPath: String = string.replace('/', '.').replace('.', '/') match {
              case s"\"$unquoted\"" => unquoted
              case unquoted => unquoted
          }
 }

 implicit class NotationUtilities(val p: Notation) extends AnyVal {
        def withTokenDeclaration(wrap: List[TypedTerminal] => TokenSpec)(terminals: List[TypedTerminal]): Notation =
                p.copy(theTokens = wrap(terminals) :: p.theTokens)

        def withSignature(signature: String): Notation =
                 p.copy(theSignature = s"${p.theSignature} $signature")
 }

def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* Notation: Notation = Prefix "%rules" INCLUDE Rules OPTNL {  $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) } */
 case 1 => 
  { case List(dol$Prefix: Notation, _, dol$INCLUDE: String, dol$Rules: List[Rule @unchecked], dol$OPTNL: Unit) => 
          dol$Prefix.copy(theRules = dol$Rules.reverse, theRulesInclude = dol$INCLUDE) 
  }
 /* Prefix: Notation =  {  Notation() } */
 case 2 => 
  { case List() =>    Notation()  } 
 /* Prefix: Notation = p: Prefix "%notation" ID {  $p.copy(theName=$ID.toString) } */
 case 3 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.copy(theName=dol$ID.toString) 
  }
 /* Prefix: Notation = p: Prefix "%package" ID {  $p.copy(thePackage=$ID.toString) } */
 case 4 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.copy(thePackage=dol$ID.toString) 
  }
 /* Prefix: Notation = p: Prefix "%path" ID {  $p.copy(theExplicitPath=$ID.asPath) } */
 case 5 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.copy(theExplicitPath=dol$ID.asPath) 
  }
 /* Prefix: Notation = p: Prefix "%tables" ID {  $p.copy(tablesType=mkTableType($ID.unQuoted)) } */
 case 6 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.copy(tablesType=mkTableType(dol$ID.unQuoted)) 
  }
 /* Prefix: Notation = p: Prefix "%include" CODE {  $p.copy(theTokensInclude=$CODE) } */
 case 7 => 
  { case List(dol$p: Notation, _, dol$CODE: String) => 
          dol$p.copy(theTokensInclude=dol$CODE) 
  }
 /* Prefix: Notation = p: Prefix "%token" TypedTerminals {  $p.withTokenDeclaration(Tokens)($TypedTerminals) } */
 case 8 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
          dol$p.withTokenDeclaration(Tokens)(dol$TypedTerminals) 
  }
 /* Prefix: Notation = p: Prefix "%left" TypedTerminals {  $p.withTokenDeclaration(Left)($TypedTerminals) } */
 case 9 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
          dol$p.withTokenDeclaration(Left)(dol$TypedTerminals) 
  }
 /* Prefix: Notation = p: Prefix "%right" TypedTerminals {  $p.withTokenDeclaration(Right)($TypedTerminals) } */
 case 10 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
          dol$p.withTokenDeclaration(Right)(dol$TypedTerminals) 
  }
 /* Prefix: Notation = p: Prefix "%non" TypedTerminals {  $p.withTokenDeclaration(Nonassoc)($TypedTerminals) } */
 case 11 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
          dol$p.withTokenDeclaration(Nonassoc)(dol$TypedTerminals) 
  }
 /* Prefix: Notation = p: Prefix "%dialect" ID {  $p.withSignature($ID.unQuoted) } */
 case 12 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.withSignature(dol$ID.unQuoted) 
  }
 /* Prefix: Notation = p: Prefix "%scalalr" ID {  $p.withSignature($ID.unQuoted) } */
 case 13 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.withSignature(dol$ID.unQuoted) 
  }
 /* Prefix: Notation = p: Prefix "%signature" ID {  $p.withSignature($ID.unQuoted) } */
 case 14 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          dol$p.withSignature(dol$ID.unQuoted) 
  }
 /* INCLUDE: String = "%include" CODE SEPARATOR {  $CODE } */
 case 15 => 
  { case List(_, dol$CODE: String, _) =>    dol$CODE  } 
 /* INCLUDE: String =  { ""} */
 case 16 => 
  { case List() =>   "" } 
 /* OPTNL: Unit =  { ()} */
 case 17 => 
  { case List() =>   () } 
 /* OPTNL: Unit = SEPARATOR { ()} */
 case 18 => 
  { case List(_) =>   () } 
 /* TypedTerminals: List[TypedTerminal] =  {  Nil } */
 case 19 => 
  { case List() =>    Nil  } 
 /* TypedTerminals: List[TypedTerminal] = TypedTerminal TypedTerminals {  $TypedTerminal :: $TypedTerminals } */
 case 20 => 
  { case List(dol$TypedTerminal: TypedTerminal, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
          dol$TypedTerminal :: dol$TypedTerminals 
  }
 /* TypedTerminal: TypedTerminal = ID ":" Type {   TypedTerminal($ID, $Type, $START)   } */
 case 21 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type) => 
           TypedTerminal(dol$ID, dol$Type, dol$START)   
  }
 /* TypedTerminal: TypedTerminal = ID "(" Type ")" {   TypedTerminal($ID, $Type, $START) } */
 case 22 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type, _) => 
           TypedTerminal(dol$ID, dol$Type, dol$START) 
  }
 /* TypedTerminal: TypedTerminal = ID {   TypedTerminal($ID, NoType, $START) } */
 case 23 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
           TypedTerminal(dol$ID, NoType, dol$START) 
  }
 /* Rules: List[Rule] = Rule {  List($Rule) } */
 case 24 => 
  { case List(dol$Rule: Rule) =>    List(dol$Rule)  } 
 /* Rules: List[Rule] = Rules SEPARATOR Rule {  $Rule :: $Rules } */
 case 25 => 
  { case List(dol$Rules: List[Rule @unchecked], _, dol$Rule: Rule) => 
          dol$Rule :: dol$Rules 
  }
 /* Rule: Rule = LHS "=" OptBar RHS {  Rule($LHS, $RHS, $START) } */
 case 26 => 
  { case List(dol$LHS: TypedNonterminal, _, dol$OptBar: Unit, dol$RHS: List[Production @unchecked]) => 
          Rule(dol$LHS, dol$RHS, dol$START) 
  }
 /* OptBar: Unit = "|" { ()} */
 case 27 => 
  { case List(_) =>   () } 
 /* OptBar: Unit =  { ()} */
 case 28 => 
  { case List() =>   () } 
 /* LHS: TypedNonterminal = ID ":" Type {   TypedNonterminal($ID.warnQuoted, $Type, $START) } */
 case 29 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type) => 
           TypedNonterminal(dol$ID.warnQuoted, dol$Type, dol$START) 
  }
 /* LHS: TypedNonterminal = ID {   TypedNonterminal($ID.warnQuoted, NoType, $START) } */
 case 30 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
           TypedNonterminal(dol$ID.warnQuoted, NoType, dol$START) 
  }
 /* RHS: List[Production] = Production {  List($Production)   } */
 case 31 => 
  { case List(dol$Production: Production) => 
          List(dol$Production)   
  }
 /* RHS: List[Production] = Production "|" RHS {  $Production :: $RHS } */
 case 32 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
          dol$Production :: dol$RHS 
  }
 /* Production: Production = NamedFields Action Precedence {  Production($NamedFields, $Action, $Precedence, $START) } */
 case 33 => 
  { case List(dol$NamedFields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Name @unchecked]) => 
          Production(dol$NamedFields, dol$Action, dol$Precedence, dol$START) 
  }
 /* NamedFields: List[NamedField] = "%empty" {  Nil } */
 case 34 => 
  { case List(_) =>    Nil  } 
 /* NamedFields: List[NamedField] = NamedField {  List($NamedField) } */
 case 35 => 
  { case List(dol$NamedField: NamedField) => 
          List(dol$NamedField) 
  }
 /* NamedFields: List[NamedField] = NamedField NamedFields {  $NamedField :: $NamedFields } */
 case 36 => 
  { case List(dol$NamedField: NamedField, dol$NamedFields: List[NamedField @unchecked]) => 
          dol$NamedField :: dol$NamedFields 
  }
 /* NamedField: NamedField = ID {  NamedField(theFieldName = None, theField = $ID, $START) } */
 case 37 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          NamedField(theFieldName = None, theField = dol$ID, dol$START) 
  }
 /* NamedField: NamedField = theFieldName: ID ":" theName: ID {  NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) } */
 case 38 => 
  { case List(dol$theFieldName: org.sufrin.scalalr.stage2.AST.Name, _, dol$theName: org.sufrin.scalalr.stage2.AST.Name) => 
          NamedField(theFieldName = Some(dol$theFieldName.warnQuoted), dol$theName, dol$START) 
  }
 /* Action: Option[Expression] =  {  None } */
 case 39 => 
  { case List() =>    None  } 
 /* Action: Option[Expression] = CODE {  Some(Expression($CODE)) } */
 case 40 => 
  { case List(dol$CODE: String) => 
          Some(Expression(dol$CODE)) 
  }
 /* Precedence: Option[Name] =  {  None } */
 case 41 => 
  { case List() =>    None  } 
 /* Precedence: Option[Name] = "%prec" ID {  Some($ID)} */
 case 42 => 
  { case List(_, dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>    Some(dol$ID) } 
 /* Type: Type = ID {  Type($ID.withoutQuotes, Nil, $START) } */
 case 43 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
          Type(dol$ID.withoutQuotes, Nil, dol$START) 
  }
 /* Type: Type = ID "[" Types "]" {  Type($ID.withoutQuotes, $Types, $START) } */
 case 44 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Types: List[Type @unchecked], _) => 
          Type(dol$ID.withoutQuotes, dol$Types, dol$START) 
  }
 /* Type: Type = "(" Types ")" {  makeTupleType($Types, $START) } */
 case 45 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
          makeTupleType(dol$Types, dol$START) 
  }
 /* Type: Type = "(" ")" {  Type("Unit", Nil, $START) } */
 case 46 => 
  { case List(_, _) => 
          Type("Unit", Nil, dol$START) 
  }
 /* Types: List[Type] = Type {  List($Type) } */
 case 47 => 
  { case List(dol$Type: Type) =>    List(dol$Type)  } 
 /* Types: List[Type] = Type "," Types {  $Type :: $Types } */
 case 48 => 
  { case List(dol$Type: Type, _, dol$Types: List[Type @unchecked]) => 
          dol$Type :: dol$Types 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""Notation: Notation = Prefix "%rules" INCLUDE Rules OPTNL {  $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""Prefix: Notation =  {  Notation() }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%notation" ID {  $p.copy(theName=$ID.toString) }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%package" ID {  $p.copy(thePackage=$ID.toString) }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%path" ID {  $p.copy(theExplicitPath=$ID.asPath) }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%tables" ID {  $p.copy(tablesType=mkTableType($ID.unQuoted)) }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%include" CODE {  $p.copy(theTokensInclude=$CODE) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%token" TypedTerminals {  $p.withTokenDeclaration(Tokens)($TypedTerminals) }""", 8, trees$trees ) }
 case 9 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%left" TypedTerminals {  $p.withTokenDeclaration(Left)($TypedTerminals) }""", 9, trees$trees ) }
 case 10 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%right" TypedTerminals {  $p.withTokenDeclaration(Right)($TypedTerminals) }""", 10, trees$trees ) }
 case 11 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%non" TypedTerminals {  $p.withTokenDeclaration(Nonassoc)($TypedTerminals) }""", 11, trees$trees ) }
 case 12 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%dialect" ID {  $p.withSignature($ID.unQuoted) }""", 12, trees$trees ) }
 case 13 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%scalalr" ID {  $p.withSignature($ID.unQuoted) }""", 13, trees$trees ) }
 case 14 => 
  { case trees$trees => PARSETREE("""Prefix: Notation = p: Prefix "%signature" ID {  $p.withSignature($ID.unQuoted) }""", 14, trees$trees ) }
 case 15 => 
  { case trees$trees => PARSETREE("""INCLUDE: String = "%include" CODE SEPARATOR {  $CODE }""", 15, trees$trees ) }
 case 16 => 
  { case trees$trees => PARSETREE("""INCLUDE: String =  { ""}""", 16, trees$trees ) }
 case 17 => 
  { case trees$trees => PARSETREE("""OPTNL: Unit =  { ()}""", 17, trees$trees ) }
 case 18 => 
  { case trees$trees => PARSETREE("""OPTNL: Unit = SEPARATOR { ()}""", 18, trees$trees ) }
 case 19 => 
  { case trees$trees => PARSETREE("""TypedTerminals: List[TypedTerminal] =  {  Nil }""", 19, trees$trees ) }
 case 20 => 
  { case trees$trees => PARSETREE("""TypedTerminals: List[TypedTerminal] = TypedTerminal TypedTerminals {  $TypedTerminal :: $TypedTerminals }""", 20, trees$trees ) }
 case 21 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID ":" Type {   TypedTerminal($ID, $Type, $START)   }""", 21, trees$trees ) }
 case 22 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID "(" Type ")" {   TypedTerminal($ID, $Type, $START) }""", 22, trees$trees ) }
 case 23 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID {   TypedTerminal($ID, NoType, $START) }""", 23, trees$trees ) }
 case 24 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rule {  List($Rule) }""", 24, trees$trees ) }
 case 25 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rules SEPARATOR Rule {  $Rule :: $Rules }""", 25, trees$trees ) }
 case 26 => 
  { case trees$trees => PARSETREE("""Rule: Rule = LHS "=" OptBar RHS {  Rule($LHS, $RHS, $START) }""", 26, trees$trees ) }
 case 27 => 
  { case trees$trees => PARSETREE("""OptBar: Unit = "|" { ()}""", 27, trees$trees ) }
 case 28 => 
  { case trees$trees => PARSETREE("""OptBar: Unit =  { ()}""", 28, trees$trees ) }
 case 29 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID ":" Type {   TypedNonterminal($ID.warnQuoted, $Type, $START) }""", 29, trees$trees ) }
 case 30 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID {   TypedNonterminal($ID.warnQuoted, NoType, $START) }""", 30, trees$trees ) }
 case 31 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production {  List($Production)   }""", 31, trees$trees ) }
 case 32 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production "|" RHS {  $Production :: $RHS }""", 32, trees$trees ) }
 case 33 => 
  { case trees$trees => PARSETREE("""Production: Production = NamedFields Action Precedence {  Production($NamedFields, $Action, $Precedence, $START) }""", 33, trees$trees ) }
 case 34 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = "%empty" {  Nil }""", 34, trees$trees ) }
 case 35 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField {  List($NamedField) }""", 35, trees$trees ) }
 case 36 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField NamedFields {  $NamedField :: $NamedFields }""", 36, trees$trees ) }
 case 37 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = ID {  NamedField(theFieldName = None, theField = $ID, $START) }""", 37, trees$trees ) }
 case 38 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = theFieldName: ID ":" theName: ID {  NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) }""", 38, trees$trees ) }
 case 39 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] =  {  None }""", 39, trees$trees ) }
 case 40 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] = CODE {  Some(Expression($CODE)) }""", 40, trees$trees ) }
 case 41 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Name] =  {  None }""", 41, trees$trees ) }
 case 42 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Name] = "%prec" ID {  Some($ID)}""", 42, trees$trees ) }
 case 43 => 
  { case trees$trees => PARSETREE("""Type: Type = ID {  Type($ID.withoutQuotes, Nil, $START) }""", 43, trees$trees ) }
 case 44 => 
  { case trees$trees => PARSETREE("""Type: Type = ID "[" Types "]" {  Type($ID.withoutQuotes, $Types, $START) }""", 44, trees$trees ) }
 case 45 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" Types ")" {  makeTupleType($Types, $START) }""", 45, trees$trees ) }
 case 46 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" ")" {  Type("Unit", Nil, $START) }""", 46, trees$trees ) }
 case 47 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type {  List($Type) }""", 47, trees$trees ) }
 case 48 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type "," Types {  $Type :: $Types }""", 48, trees$trees ) }
 }

}
