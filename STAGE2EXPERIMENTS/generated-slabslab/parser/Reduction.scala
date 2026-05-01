
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

 implicit class ID2Path(val string: String) extends AnyVal {
          def toPathString: String = string.replace('/', '.').replace('.', '/') match {
            case s"\"$unquoted\"" => unquoted
            case unquoted => unquoted
          }
        }


def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 /* command: Unit = Notation {  translate($Notation) } */
 case 1 => 
  { case List(dol$Notation: Notation) => 
          translate(dol$Notation) 
  }
 /* Notation: Notation = p: Prologue RULES INCLUDE Rules {  $p.copy(theRules = $Rules, theRulesInclude = $INCLUDE) } */
 case 2 => 
  { case List(dol$p: Notation, dol$RULES: Unit, dol$INCLUDE: String, dol$Rules: List[Rule @unchecked]) => 
          dol$p.copy(theRules = dol$Rules, theRulesInclude = dol$INCLUDE) 
  }
 /* Prologue: Notation =  {  Notation() } */
 case 3 => 
  { case List() =>    Notation()  } 
 /* Prologue: Notation = p: Prologue "%notation" ID {  $p.copy(theName=$ID) } */
 case 4 => 
  { case List(dol$p: Notation, _, dol$ID: String) => 
          dol$p.copy(theName=dol$ID) 
  }
 /* Prologue: Notation = p: Prologue "%package" ID {  $p.copy(thePackage=$ID) } */
 case 5 => 
  { case List(dol$p: Notation, _, dol$ID: String) => 
          dol$p.copy(thePackage=dol$ID) 
  }
 /* Prologue: Notation = p: Prologue "%path" ID {  $p.copy(theExplicitPath=$ID.toPathString) } */
 case 6 => 
  { case List(dol$p: Notation, _, dol$ID: String) => 
          dol$p.copy(theExplicitPath=dol$ID.toPathString) 
  }
 /* Prologue: Notation = p: Prologue "%tables" ID {  $p.copy(tablesType=mkTableType($ID)) } */
 case 7 => 
  { case List(dol$p: Notation, _, dol$ID: String) => 
          dol$p.copy(tablesType=mkTableType(dol$ID)) 
  }
 /* Prologue: Notation = p: Prologue "%include" CODE {  $p.copy(theTokensInclude=$CODE) } */
 case 8 => 
  { case List(dol$p: Notation, _, dol$CODE: String) => 
          dol$p.copy(theTokensInclude=dol$CODE) 
  }
 /* RULES: Unit = "%rules" {  println("IMPLICIT RULE ENDINGS ENABLED"); Scanner.enableNL=true } */
 case 9 => 
  { case List(_) => 
          println("IMPLICIT RULE ENDINGS ENABLED"); Scanner.enableNL=true 
  }
 /* INCLUDE: String = "%include" CODE NL {  $CODE } */
 case 10 => 
  { case List(_, dol$CODE: String, _) =>    dol$CODE  } 
 /* INCLUDE: String =  { ""} */
 case 11 => 
  { case List() =>   "" } 
 /* Rules: List[Rule] = Rule {  List($Rule) } */
 case 12 => 
  { case List(dol$Rule: Rule) =>    List(dol$Rule)  } 
 /* Rules: List[Rule] = Rules NL Rule {  $Rule :: $Rules } */
 case 13 => 
  { case List(dol$Rules: List[Rule @unchecked], _, dol$Rule: Rule) => 
          dol$Rule :: dol$Rules 
  }
 /* Rule: Rule = LHS "=" OptBar RHS {  Rule($LHS, $RHS, $START) } */
 case 14 => 
  { case List(dol$LHS: TypedNonterminal, _, dol$OptBar: Unit, dol$RHS: List[Production @unchecked]) => 
          Rule(dol$LHS, dol$RHS, dol$START) 
  }
 /* OptBar: Unit = "|" { ()} */
 case 15 => 
  { case List(_) =>   () } 
 /* OptBar: Unit =  { ()} */
 case 16 => 
  { case List() =>   () } 
 /* LHS: TypedNonterminal = ID ":" Type {   TypedNonterminal($ID, $Type, $START) } */
 case 17 => 
  { case List(dol$ID: String, _, dol$Type: Type) => 
           TypedNonterminal(dol$ID, dol$Type, dol$START) 
  }
 /* LHS: TypedNonterminal = ID {   TypedNonterminal($ID, Untyped, $START) } */
 case 18 => 
  { case List(dol$ID: String) => 
           TypedNonterminal(dol$ID, Untyped, dol$START) 
  }
 /* RHS: List[Production] = Production {  List($Production)   } */
 case 19 => 
  { case List(dol$Production: Production) => 
          List(dol$Production)   
  }
 /* RHS: List[Production] = Production "|" RHS {  $Production :: $RHS } */
 case 20 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
          dol$Production :: dol$RHS 
  }
 /* Production: Production = NamedFields Action Precedence {  Production($NamedFields, $Action, $Precedence, $START) } */
 case 21 => 
  { case List(dol$NamedFields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Terminal @unchecked]) => 
          Production(dol$NamedFields, dol$Action, dol$Precedence, dol$START) 
  }
 /* NamedFields: List[NamedField] = "%empty" {  Nil } */
 case 22 => 
  { case List(_) =>    Nil  } 
 /* NamedFields: List[NamedField] = NamedField {  List($NamedField) } */
 case 23 => 
  { case List(dol$NamedField: NamedField) => 
          List(dol$NamedField) 
  }
 /* NamedFields: List[NamedField] = NamedField NamedFields {  $NamedField :: $NamedFields } */
 case 24 => 
  { case List(dol$NamedField: NamedField, dol$NamedFields: List[NamedField @unchecked]) => 
          dol$NamedField :: dol$NamedFields 
  }
 /* NamedField: NamedField = ID {  NamedField(theName = None, fieldSymbol = $ID, $START) } */
 case 25 => 
  { case List(dol$ID: String) => 
          NamedField(theName = None, fieldSymbol = dol$ID, dol$START) 
  }
 /* NamedField: NamedField = theName: ID ":" fieldSymbol: ID {  NamedField(Some($theName), $fieldSymbol, $START) } */
 case 26 => 
  { case List(dol$theName: String, _, dol$fieldSymbol: String) => 
          NamedField(Some(dol$theName), dol$fieldSymbol, dol$START) 
  }
 /* Action: Option[Expression] =  {  None } */
 case 27 => 
  { case List() =>    None  } 
 /* Action: Option[Expression] = CODE {  Some($CODE) } */
 case 28 => 
  { case List(dol$CODE: String) =>    Some(dol$CODE)  } 
 /* Precedence: Option[Terminal] =  {  None } */
 case 29 => 
  { case List() =>    None  } 
 /* Precedence: Option[Terminal] = "%prec" ID {  Some(new Terminal($ID)) } */
 case 30 => 
  { case List(_, dol$ID: String) => 
          Some(new Terminal(dol$ID)) 
  }
 /* Type: Type = ID {  Type($ID, Nil, $START) } */
 case 31 => 
  { case List(dol$ID: String) => 
          Type(dol$ID, Nil, dol$START) 
  }
 /* Type: Type = ID "[" Types "]" {  Type($ID, $Types, $START) } */
 case 32 => 
  { case List(dol$ID: String, _, dol$Types: List[Type @unchecked], _) => 
          Type(dol$ID, dol$Types, dol$START) 
  }
 /* Type: Type = "(" Types ")" {  makeTupleType($Types, $START) } */
 case 33 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
          makeTupleType(dol$Types, dol$START) 
  }
 /* Type: Type = "(" ")" {  Type("Unit", Nil, $START) } */
 case 34 => 
  { case List(_, _) => 
          Type("Unit", Nil, dol$START) 
  }
 /* Types: List[Type] = Type {  List($Type) } */
 case 35 => 
  { case List(dol$Type: Type) =>    List(dol$Type)  } 
 /* Types: List[Type] = Type "," Types {  $Type :: $Types } */
 case 36 => 
  { case List(dol$Type: Type, _, dol$Types: List[Type @unchecked]) => 
          dol$Type :: dol$Types 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""command: Unit = Notation {  translate($Notation) }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""Notation: Notation = p: Prologue RULES INCLUDE Rules {  $p.copy(theRules = $Rules, theRulesInclude = $INCLUDE) }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""Prologue: Notation =  {  Notation() }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""Prologue: Notation = p: Prologue "%notation" ID {  $p.copy(theName=$ID) }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""Prologue: Notation = p: Prologue "%package" ID {  $p.copy(thePackage=$ID) }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""Prologue: Notation = p: Prologue "%path" ID {  $p.copy(theExplicitPath=$ID.toPathString) }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""Prologue: Notation = p: Prologue "%tables" ID {  $p.copy(tablesType=mkTableType($ID)) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""Prologue: Notation = p: Prologue "%include" CODE {  $p.copy(theTokensInclude=$CODE) }""", 8, trees$trees ) }
 case 9 => 
  { case trees$trees => PARSETREE("""RULES: Unit = "%rules" {  println("IMPLICIT RULE ENDINGS ENABLED"); Scanner.enableNL=true }""", 9, trees$trees ) }
 case 10 => 
  { case trees$trees => PARSETREE("""INCLUDE: String = "%include" CODE NL {  $CODE }""", 10, trees$trees ) }
 case 11 => 
  { case trees$trees => PARSETREE("""INCLUDE: String =  { ""}""", 11, trees$trees ) }
 case 12 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rule {  List($Rule) }""", 12, trees$trees ) }
 case 13 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rules NL Rule {  $Rule :: $Rules }""", 13, trees$trees ) }
 case 14 => 
  { case trees$trees => PARSETREE("""Rule: Rule = LHS "=" OptBar RHS {  Rule($LHS, $RHS, $START) }""", 14, trees$trees ) }
 case 15 => 
  { case trees$trees => PARSETREE("""OptBar: Unit = "|" { ()}""", 15, trees$trees ) }
 case 16 => 
  { case trees$trees => PARSETREE("""OptBar: Unit =  { ()}""", 16, trees$trees ) }
 case 17 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID ":" Type {   TypedNonterminal($ID, $Type, $START) }""", 17, trees$trees ) }
 case 18 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID {   TypedNonterminal($ID, Untyped, $START) }""", 18, trees$trees ) }
 case 19 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production {  List($Production)   }""", 19, trees$trees ) }
 case 20 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production "|" RHS {  $Production :: $RHS }""", 20, trees$trees ) }
 case 21 => 
  { case trees$trees => PARSETREE("""Production: Production = NamedFields Action Precedence {  Production($NamedFields, $Action, $Precedence, $START) }""", 21, trees$trees ) }
 case 22 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = "%empty" {  Nil }""", 22, trees$trees ) }
 case 23 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField {  List($NamedField) }""", 23, trees$trees ) }
 case 24 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField NamedFields {  $NamedField :: $NamedFields }""", 24, trees$trees ) }
 case 25 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = ID {  NamedField(theName = None, fieldSymbol = $ID, $START) }""", 25, trees$trees ) }
 case 26 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = theName: ID ":" fieldSymbol: ID {  NamedField(Some($theName), $fieldSymbol, $START) }""", 26, trees$trees ) }
 case 27 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] =  {  None }""", 27, trees$trees ) }
 case 28 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] = CODE {  Some($CODE) }""", 28, trees$trees ) }
 case 29 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Terminal] =  {  None }""", 29, trees$trees ) }
 case 30 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Terminal] = "%prec" ID {  Some(new Terminal($ID)) }""", 30, trees$trees ) }
 case 31 => 
  { case trees$trees => PARSETREE("""Type: Type = ID {  Type($ID, Nil, $START) }""", 31, trees$trees ) }
 case 32 => 
  { case trees$trees => PARSETREE("""Type: Type = ID "[" Types "]" {  Type($ID, $Types, $START) }""", 32, trees$trees ) }
 case 33 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" Types ")" {  makeTupleType($Types, $START) }""", 33, trees$trees ) }
 case 34 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" ")" {  Type("Unit", Nil, $START) }""", 34, trees$trees ) }
 case 35 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type {  List($Type) }""", 35, trees$trees ) }
 case 36 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type "," Types {  $Type :: $Types }""", 36, trees$trees ) }
 }

}
