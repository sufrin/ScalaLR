
package scalalr.parser.ScalaLR
object Reduction {

 import org.sufrin.scalalr.AST._
 import org.sufrin.scalalr.SourceLocation
 import org.sufrin.scalalr.translateFLAB._
 import org.sufrin.utility.PrettyPrint._
 import org.sufrin.utility.SourceTextCursor

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


def reduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 // command: Unit = Notation { translate($Notation) }
 case 1 => 
  { case List(dol$Notation: Notation) => 
         translate(dol$Notation) 
  }
 // Notation: Notation = "%notation" theName: ID "%package" thePackage: ID "%path" thePath: ID Tables tokensInclude: OptInclude Tokens "%rules" rulesInclude: OptInclude Rules OptSemicolon { Notation($thePackage, $theName, $thePath, $Tables, "Scanner", Type("Token", Nil, $START), $Tokens, $Rules, $tokensInclude, $rulesInclude) }
 case 2 => 
  { case List(_, dol$theName: String, _, dol$thePackage: String, _, dol$thePath: String, dol$Tables: String, dol$tokensInclude: String, dol$Tokens: List[TokenSpec @unchecked], _, dol$rulesInclude: String, dol$Rules: List[Rule @unchecked], dol$OptSemicolon: Unit) => 
         Notation(dol$thePackage, dol$theName, dol$thePath, dol$Tables, "Scanner", Type("Token", Nil, dol$START), dol$Tokens, dol$Rules, dol$tokensInclude, dol$rulesInclude) 
  }
 // OptInclude: String = "%empty" { "" }
 case 3 => 
  { case List(_) =>   ""  } 
 // OptInclude: String = "%include" CODE { $CODE }
 case 4 => 
  { case List(_, dol$CODE: String) =>   dol$CODE  } 
 // Tokens: List[TokenSpec] =  { Nil }
 case 5 => 
  { case List() =>   Nil  } 
 // Tokens: List[TokenSpec] = TokenSpec Tokens { $TokenSpec :: $Tokens }
 case 6 => 
  { case List(dol$TokenSpec: TokenSpec, dol$Tokens: List[TokenSpec @unchecked]) => 
         dol$TokenSpec :: dol$Tokens 
  }
 // TokenSpec: TokenSpec = "%left" TypedTerminals { Left($TypedTerminals) }
 case 7 => 
  { case List(_, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
         Left(dol$TypedTerminals) 
  }
 // TokenSpec: TokenSpec = "%right" TypedTerminals { Right($TypedTerminals) }
 case 8 => 
  { case List(_, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
         Right(dol$TypedTerminals) 
  }
 // TokenSpec: TokenSpec = "%non" TypedTerminals { Nonassoc($TypedTerminals) }
 case 9 => 
  { case List(_, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
         Nonassoc(dol$TypedTerminals) 
  }
 // TokenSpec: TokenSpec = "%token" TypedTerminals { Tokens($TypedTerminals) }
 case 10 => 
  { case List(_, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
         Tokens(dol$TypedTerminals) 
  }
 // TypedTerminals: List[TypedTerminal] = TypedTerminal { List($TypedTerminal) }
 case 11 => 
  { case List(dol$TypedTerminal: TypedTerminal) => 
         List(dol$TypedTerminal) 
  }
 // TypedTerminals: List[TypedTerminal] = TypedTerminal TypedTerminals { $TypedTerminal :: $TypedTerminals }
 case 12 => 
  { case List(dol$TypedTerminal: TypedTerminal, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
         dol$TypedTerminal :: dol$TypedTerminals 
  }
 // TypedTerminal: TypedTerminal = ID ":" Type {  TypedTerminal($ID, $Type, $START)   }
 case 13 => 
  { case List(dol$ID: String, _, dol$Type: Type) => 
          TypedTerminal(dol$ID, dol$Type, dol$START)   
  }
 // TypedTerminal: TypedTerminal = ID "(" Type ")" {  TypedTerminal($ID, $Type, $START) }
 case 14 => 
  { case List(dol$ID: String, _, dol$Type: Type, _) => 
          TypedTerminal(dol$ID, dol$Type, dol$START) 
  }
 // TypedTerminal: TypedTerminal = ID {  TypedTerminal($ID, Untyped, $START) }
 case 15 => 
  { case List(dol$ID: String) => 
          TypedTerminal(dol$ID, Untyped, dol$START) 
  }
 // Tables: String =  { "lalr" }
 case 16 => 
  { case List() =>   "lalr"  } 
 // Tables: String = "%tables" ID { mkTableType($ID) }
 case 17 => 
  { case List(_, dol$ID: String) => 
         mkTableType(dol$ID) 
  }
 // Rules: List[Rule] = Rule { List($Rule) }
 case 18 => 
  { case List(dol$Rule: Rule) =>   List(dol$Rule)  } 
 // Rules: List[Rule] = Rules ";" Rule { $Rule :: $Rules }
 case 19 => 
  { case List(dol$Rules: List[Rule @unchecked], _, dol$Rule: Rule) => 
         dol$Rule :: dol$Rules 
  }
 // Rule: Rule = LHS "=" RHS { Rule($LHS, $RHS, $START) }
 case 20 => 
  { case List(dol$LHS: TypedNonterminal, _, dol$RHS: List[Production @unchecked]) => 
         Rule(dol$LHS, dol$RHS, dol$START) 
  }
 // OptSemicolon: Unit =  {()}
 case 21 => 
  { case List() =>  () } 
 // OptSemicolon: Unit = ";" {()}
 case 22 => 
  { case List(_) =>  () } 
 // LHS: TypedNonterminal = ID ":" Type {  TypedNonterminal($ID, $Type, $START) }
 case 23 => 
  { case List(dol$ID: String, _, dol$Type: Type) => 
          TypedNonterminal(dol$ID, dol$Type, dol$START) 
  }
 // LHS: TypedNonterminal = ID {  TypedNonterminal($ID, Untyped, $START) }
 case 24 => 
  { case List(dol$ID: String) => 
          TypedNonterminal(dol$ID, Untyped, dol$START) 
  }
 // RHS: List[Production] = Production { List($Production)   }
 case 25 => 
  { case List(dol$Production: Production) => 
         List(dol$Production)   
  }
 // RHS: List[Production] = Production "|" RHS { $Production :: $RHS }
 case 26 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
         dol$Production :: dol$RHS 
  }
 // Production: Production = NamedFields Action Precedence { Production($NamedFields, $Action, $Precedence, $START) }
 case 27 => 
  { case List(dol$NamedFields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Terminal @unchecked]) => 
         Production(dol$NamedFields, dol$Action, dol$Precedence, dol$START) 
  }
 // NamedFields: List[NamedField] = "%empty" { Nil }
 case 28 => 
  { case List(_) =>   Nil  } 
 // NamedFields: List[NamedField] = NamedField { List($NamedField) }
 case 29 => 
  { case List(dol$NamedField: NamedField) => 
         List(dol$NamedField) 
  }
 // NamedFields: List[NamedField] = NamedField NamedFields { $NamedField :: $NamedFields }
 case 30 => 
  { case List(dol$NamedField: NamedField, dol$NamedFields: List[NamedField @unchecked]) => 
         dol$NamedField :: dol$NamedFields 
  }
 // NamedField: NamedField = ID { NamedField(theName = None, fieldSymbol = $ID, $START) }
 case 31 => 
  { case List(dol$ID: String) => 
         NamedField(theName = None, fieldSymbol = dol$ID, dol$START) 
  }
 // NamedField: NamedField = theName: ID ":" fieldSymbol: ID { NamedField(Some($theName), $fieldSymbol, $START) }
 case 32 => 
  { case List(dol$theName: String, _, dol$fieldSymbol: String) => 
         NamedField(Some(dol$theName), dol$fieldSymbol, dol$START) 
  }
 // Action: Option[Expression] =  { None }
 case 33 => 
  { case List() =>   None  } 
 // Action: Option[Expression] = CODE { Some($CODE) }
 case 34 => 
  { case List(dol$CODE: String) =>   Some(dol$CODE)  } 
 // Precedence: Option[Terminal] =  { None }
 case 35 => 
  { case List() =>   None  } 
 // Precedence: Option[Terminal] = "%prec" ID { Some(new Terminal($ID)) }
 case 36 => 
  { case List(_, dol$ID: String) => 
         Some(new Terminal(dol$ID)) 
  }
 // Type: Type = ID { Type($ID, Nil, $START) }
 case 37 => 
  { case List(dol$ID: String) => 
         Type(dol$ID, Nil, dol$START) 
  }
 // Type: Type = ID "[" Types "]" { Type($ID, $Types, $START) }
 case 38 => 
  { case List(dol$ID: String, _, dol$Types: List[Type @unchecked], _) => 
         Type(dol$ID, dol$Types, dol$START) 
  }
 // Type: Type = "(" Types ")" { makeTupleType($Types, $START) }
 case 39 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
         makeTupleType(dol$Types, dol$START) 
  }
 // Type: Type = "(" ")" { Type("Unit", Nil, $START) }
 case 40 => 
  { case List(_, _) => 
         Type("Unit", Nil, dol$START) 
  }
 // Types: List[Type] = Type { List($Type) }
 case 41 => 
  { case List(dol$Type: Type) =>   List(dol$Type)  } 
 // Types: List[Type] = Type "," Types { $Type :: $Types }
 case 42 => 
  { case List(dol$Type: Type, _, dol$Types: List[Type @unchecked]) => 
         dol$Type :: dol$Types 
  }
 }

case class PARSETREE(prod: String, rule: Int, trees:List[Any])
def parsetreereduction(dol$START:  org.sufrin.scalalr.SourceLocation, dol$END:  org.sufrin.scalalr.SourceLocation, n: Int): PartialFunction[List[Any], Any] = n match {
 case 1 => 
  { case trees$trees => PARSETREE("""command: Unit = Notation { translate($Notation) }""", 1, trees$trees ) }
 case 2 => 
  { case trees$trees => PARSETREE("""Notation: Notation = "%notation" theName: ID "%package" thePackage: ID "%path" thePath: ID Tables tokensInclude: OptInclude Tokens "%rules" rulesInclude: OptInclude Rules OptSemicolon { Notation($thePackage, $theName, $thePath, $Tables, "Scanner", Type("Token", Nil, $START), $Tokens, $Rules, $tokensInclude, $rulesInclude) }""", 2, trees$trees ) }
 case 3 => 
  { case trees$trees => PARSETREE("""OptInclude: String = "%empty" { "" }""", 3, trees$trees ) }
 case 4 => 
  { case trees$trees => PARSETREE("""OptInclude: String = "%include" CODE { $CODE }""", 4, trees$trees ) }
 case 5 => 
  { case trees$trees => PARSETREE("""Tokens: List[TokenSpec] =  { Nil }""", 5, trees$trees ) }
 case 6 => 
  { case trees$trees => PARSETREE("""Tokens: List[TokenSpec] = TokenSpec Tokens { $TokenSpec :: $Tokens }""", 6, trees$trees ) }
 case 7 => 
  { case trees$trees => PARSETREE("""TokenSpec: TokenSpec = "%left" TypedTerminals { Left($TypedTerminals) }""", 7, trees$trees ) }
 case 8 => 
  { case trees$trees => PARSETREE("""TokenSpec: TokenSpec = "%right" TypedTerminals { Right($TypedTerminals) }""", 8, trees$trees ) }
 case 9 => 
  { case trees$trees => PARSETREE("""TokenSpec: TokenSpec = "%non" TypedTerminals { Nonassoc($TypedTerminals) }""", 9, trees$trees ) }
 case 10 => 
  { case trees$trees => PARSETREE("""TokenSpec: TokenSpec = "%token" TypedTerminals { Tokens($TypedTerminals) }""", 10, trees$trees ) }
 case 11 => 
  { case trees$trees => PARSETREE("""TypedTerminals: List[TypedTerminal] = TypedTerminal { List($TypedTerminal) }""", 11, trees$trees ) }
 case 12 => 
  { case trees$trees => PARSETREE("""TypedTerminals: List[TypedTerminal] = TypedTerminal TypedTerminals { $TypedTerminal :: $TypedTerminals }""", 12, trees$trees ) }
 case 13 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID ":" Type {  TypedTerminal($ID, $Type, $START)   }""", 13, trees$trees ) }
 case 14 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID "(" Type ")" {  TypedTerminal($ID, $Type, $START) }""", 14, trees$trees ) }
 case 15 => 
  { case trees$trees => PARSETREE("""TypedTerminal: TypedTerminal = ID {  TypedTerminal($ID, Untyped, $START) }""", 15, trees$trees ) }
 case 16 => 
  { case trees$trees => PARSETREE("""Tables: String =  { "lalr" }""", 16, trees$trees ) }
 case 17 => 
  { case trees$trees => PARSETREE("""Tables: String = "%tables" ID { mkTableType($ID) }""", 17, trees$trees ) }
 case 18 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rule { List($Rule) }""", 18, trees$trees ) }
 case 19 => 
  { case trees$trees => PARSETREE("""Rules: List[Rule] = Rules ";" Rule { $Rule :: $Rules }""", 19, trees$trees ) }
 case 20 => 
  { case trees$trees => PARSETREE("""Rule: Rule = LHS "=" RHS { Rule($LHS, $RHS, $START) }""", 20, trees$trees ) }
 case 21 => 
  { case trees$trees => PARSETREE("""OptSemicolon: Unit =  {()}""", 21, trees$trees ) }
 case 22 => 
  { case trees$trees => PARSETREE("""OptSemicolon: Unit = ";" {()}""", 22, trees$trees ) }
 case 23 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID ":" Type {  TypedNonterminal($ID, $Type, $START) }""", 23, trees$trees ) }
 case 24 => 
  { case trees$trees => PARSETREE("""LHS: TypedNonterminal = ID {  TypedNonterminal($ID, Untyped, $START) }""", 24, trees$trees ) }
 case 25 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production { List($Production)   }""", 25, trees$trees ) }
 case 26 => 
  { case trees$trees => PARSETREE("""RHS: List[Production] = Production "|" RHS { $Production :: $RHS }""", 26, trees$trees ) }
 case 27 => 
  { case trees$trees => PARSETREE("""Production: Production = NamedFields Action Precedence { Production($NamedFields, $Action, $Precedence, $START) }""", 27, trees$trees ) }
 case 28 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = "%empty" { Nil }""", 28, trees$trees ) }
 case 29 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField { List($NamedField) }""", 29, trees$trees ) }
 case 30 => 
  { case trees$trees => PARSETREE("""NamedFields: List[NamedField] = NamedField NamedFields { $NamedField :: $NamedFields }""", 30, trees$trees ) }
 case 31 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = ID { NamedField(theName = None, fieldSymbol = $ID, $START) }""", 31, trees$trees ) }
 case 32 => 
  { case trees$trees => PARSETREE("""NamedField: NamedField = theName: ID ":" fieldSymbol: ID { NamedField(Some($theName), $fieldSymbol, $START) }""", 32, trees$trees ) }
 case 33 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] =  { None }""", 33, trees$trees ) }
 case 34 => 
  { case trees$trees => PARSETREE("""Action: Option[Expression] = CODE { Some($CODE) }""", 34, trees$trees ) }
 case 35 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Terminal] =  { None }""", 35, trees$trees ) }
 case 36 => 
  { case trees$trees => PARSETREE("""Precedence: Option[Terminal] = "%prec" ID { Some(new Terminal($ID)) }""", 36, trees$trees ) }
 case 37 => 
  { case trees$trees => PARSETREE("""Type: Type = ID { Type($ID, Nil, $START) }""", 37, trees$trees ) }
 case 38 => 
  { case trees$trees => PARSETREE("""Type: Type = ID "[" Types "]" { Type($ID, $Types, $START) }""", 38, trees$trees ) }
 case 39 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" Types ")" { makeTupleType($Types, $START) }""", 39, trees$trees ) }
 case 40 => 
  { case trees$trees => PARSETREE("""Type: Type = "(" ")" { Type("Unit", Nil, $START) }""", 40, trees$trees ) }
 case 41 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type { List($Type) }""", 41, trees$trees ) }
 case 42 => 
  { case trees$trees => PARSETREE("""Types: List[Type] = Type "," Types { $Type :: $Types }""", 42, trees$trees ) }
 }

}
