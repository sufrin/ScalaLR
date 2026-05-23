

package scalalr.stage2
object Reduction {



 import org.sufrin.scalalr.stage2.AST._
 import org.sufrin.scalalr.stage2.Normalization._
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
 /* Notation: Notation = Prefix `%rules` INCLUDE Rules OPTSEPARATOR { $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) }  */
 case 1 => 
  { case List(dol$Prefix: Notation, _, dol$INCLUDE: String, dol$Rules: List[Rule @unchecked], _) => 
        dol$Prefix.copy(theRules = dol$Rules.reverse, theRulesInclude = dol$INCLUDE)
  }
 /* Prefix: Notation =  { Notation() }  */
 case 2 => 
  { case List() =>  Notation() } 
 /* Prefix: Notation = p: Prefix `%notation` ID { $p.copy(theName=$ID.toString) }  */
 case 3 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(theName=dol$ID.toString)
  }
 /* Prefix: Notation = p: Prefix `%package` ID { $p.copy(thePackage=$ID.toString) }  */
 case 4 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(thePackage=dol$ID.toString)
  }
 /* Prefix: Notation = p: Prefix `%path` ID { $p.copy(theExplicitPath=$ID.asPath) }  */
 case 5 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(theExplicitPath=dol$ID.asPath)
  }
 /* Prefix: Notation = p: Prefix `%tables` ID { $p.copy(tablesType=mkTableType($ID.unQuoted)) }  */
 case 6 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(tablesType=mkTableType(dol$ID.unQuoted))
  }
 /* Prefix: Notation = p: Prefix `%include` CODE { $p.copy(theTokensInclude=$CODE) }  */
 case 7 => 
  { case List(dol$p: Notation, _, dol$CODE: String) => 
        dol$p.copy(theTokensInclude=dol$CODE)
  }
 /* Prefix: Notation = p: Prefix `%token` TypedTerminals { $p.withTokenDeclaration(Tokens)($TypedTerminals) }  */
 case 8 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Tokens)(dol$TypedTerminals)
  }
 /* Prefix: Notation = p: Prefix `%left` TypedTerminals { $p.withTokenDeclaration(Left)($TypedTerminals) }  */
 case 9 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Left)(dol$TypedTerminals)
  }
 /* Prefix: Notation = p: Prefix `%right` TypedTerminals { $p.withTokenDeclaration(Right)($TypedTerminals) }  */
 case 10 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Right)(dol$TypedTerminals)
  }
 /* Prefix: Notation = p: Prefix `%non` TypedTerminals { $p.withTokenDeclaration(Nonassoc)($TypedTerminals) }  */
 case 11 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Nonassoc)(dol$TypedTerminals)
  }
 /* Prefix: Notation = p: Prefix `%prec` TypedTerminals { $p.withTokenDeclaration(Precedence)($TypedTerminals) }  */
 case 12 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Precedence)(dol$TypedTerminals)
  }
 /* Prefix: Notation = p: Prefix `%dialect` ID { $p.withSignature($ID.unQuoted) }  */
 case 13 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* Prefix: Notation = p: Prefix `%scalalr` ID { $p.withSignature($ID.unQuoted) }  */
 case 14 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* Prefix: Notation = p: Prefix `%signature` ID { $p.withSignature($ID.unQuoted) }  */
 case 15 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* INCLUDE: String = `%include` CODE SEPARATOR { $CODE }  */
 case 16 => 
  { case List(_, dol$CODE: String, _) =>  dol$CODE } 
 /* INCLUDE: String =  { "" }  */
 case 17 => 
  { case List() =>  "" } 
 /* OPTSEPARATOR: Unit =  { () }  */
 case 18 => 
  { case List() =>  () } 
 /* OPTSEPARATOR: Unit = SEPARATOR { () }  */
 case 19 => 
  { case List(_) =>  () } 
 /* TypedTerminals: List[TypedTerminal] =  { Nil }  */
 case 20 => 
  { case List() =>  Nil } 
 /* TypedTerminals: List[TypedTerminal] = TypedTerminal TypedTerminals { $TypedTerminal :: $TypedTerminals }  */
 case 21 => 
  { case List(dol$TypedTerminal: TypedTerminal, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$TypedTerminal :: dol$TypedTerminals
  }
 /* TypedTerminal: TypedTerminal = ID `:` Type { TypedTerminal($ID, $Type, $START) }  */
 case 22 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: SymbolType) => 
        TypedTerminal(dol$ID, dol$Type, dol$START)
  }
 /* TypedTerminal: TypedTerminal = ID `(` Type `)` { TypedTerminal($ID, $Type, $START) }  */
 case 23 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: SymbolType, _) => 
        TypedTerminal(dol$ID, dol$Type, dol$START)
  }
 /* TypedTerminal: TypedTerminal = ID { TypedTerminal($ID, NoType, $START) }  */
 case 24 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        TypedTerminal(dol$ID, NoType, dol$START)
  }
 /* Rules: List[Rule] = Rule { List($Rule) }  */
 case 25 => 
  { case List(dol$Rule: Rule) =>  List(dol$Rule) } 
 /* Rules: List[Rule] = Rules SEPARATOR Rule { $Rule :: $Rules }  */
 case 26 => 
  { case List(dol$Rules: List[Rule @unchecked], _, dol$Rule: Rule) => 
        dol$Rule :: dol$Rules
  }
 /* Rule: Rule = LHS S_1 RHS { Rule($LHS, $RHS, $START) }  */
 case 27 => 
  { case List(dol$LHS: TypedNonterminal, dol$S_1: Option[Unit @unchecked], dol$RHS: List[Production @unchecked]) => 
        Rule(dol$LHS, dol$RHS, dol$START)
  }
 /* LHS: TypedNonterminal = ID `:` Type `=` { (TypedNonterminal($ID.warnQuoted, $Type, $START)) }  */
 case 28 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: SymbolType, _) => 
        (TypedNonterminal(dol$ID.warnQuoted, dol$Type, dol$START))
  }
 /* LHS: TypedNonterminal = ID `=` { (TypedNonterminal($ID.warnQuoted, TypeVariable($ID), $START)) }  */
 case 29 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _) => 
        (TypedNonterminal(dol$ID.warnQuoted, TypeVariable(dol$ID), dol$START))
  }
 /* RHS: List[Production] = Production { List($Production) }  */
 case 30 => 
  { case List(dol$Production: Production) =>  List(dol$Production) } 
 /* RHS: List[Production] = Production `|` RHS { $Production :: $RHS }  */
 case 31 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
        dol$Production :: dol$RHS
  }
 /* Production: Production = Fields Action Precedence { Production($Fields, $Action, $Precedence, $START) }  */
 case 32 => 
  { case List(dol$Fields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Name @unchecked]) => 
        Production(dol$Fields, dol$Action, dol$Precedence, dol$START)
  }
 /* Fields: List[NamedField] = `%empty` { Nil }  */
 case 33 => 
  { case List(_) =>  Nil } 
 /* Fields: List[NamedField] = fields: S_2 { $fields }  */
 case 34 => 
  { case List(dol$fields: List[NamedField @unchecked]) =>  dol$fields } 
 /* NamedField: NamedField = FIELD { NamedField(theFieldName = None, theField = $FIELD, $START) }  */
 case 35 => 
  { case List(dol$FIELD: Name) => 
        NamedField(theFieldName = None, theField = dol$FIELD, dol$START)
  }
 /* NamedField: NamedField = theFieldName: ID `:` theName: FIELD { NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) }  */
 case 36 => 
  { case List(dol$theFieldName: org.sufrin.scalalr.stage2.AST.Name, _, dol$theName: Name) => 
        NamedField(theFieldName = Some(dol$theFieldName.warnQuoted), dol$theName, dol$START)
  }
 /* FIELD: Name = ID { $ID }  */
 case 37 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>  dol$ID } 
 /* FIELD: Name = `(` Fields `)` REPEAT { syntheticRuleName($Fields, $REPEAT, $START, $END) }  */
 case 38 => 
  { case List(_, dol$Fields: List[NamedField @unchecked], _, dol$REPEAT: Repeat) => 
        syntheticRuleName(dol$Fields, dol$REPEAT, dol$START, dol$END)
  }
 /* REPEAT: Repeat = `?` { MaybeOne }  */
 case 39 => 
  { case List(_) =>  MaybeOne } 
 /* REPEAT: Repeat = `*` { NoneOrMore }  */
 case 40 => 
  { case List(_) =>  NoneOrMore } 
 /* REPEAT: Repeat = `+` { OneOrMore }  */
 case 41 => 
  { case List(_) =>  OneOrMore } 
 /* Action: Option[Expression] =  { None }  */
 case 42 => 
  { case List() =>  None } 
 /* Action: Option[Expression] = CODE { Some(Expression($CODE)) }  */
 case 43 => 
  { case List(dol$CODE: String) =>  Some(Expression(dol$CODE)) } 
 /* Precedence: Option[Name] = S_3 { $S_3 }  */
 case 44 => 
  { case List(dol$S_3: Option[org.sufrin.scalalr.stage2.AST.Name @unchecked]) =>  dol$S_3 } 
 /* Type: SymbolType = ID { Type($ID.withoutQuotes, Nil, $START) }  */
 case 45 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        Type(dol$ID.withoutQuotes, Nil, dol$START)
  }
 /* Type: SymbolType = ID `[` Types `]` { Type($ID.withoutQuotes, $Types, $START) }  */
 case 46 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Types: List[Type @unchecked], _) => 
        Type(dol$ID.withoutQuotes, dol$Types, dol$START)
  }
 /* Type: SymbolType = `(` Types `)` { makeTupleType($Types, $START) }  */
 case 47 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
        makeTupleType(dol$Types, dol$START)
  }
 /* Type: SymbolType = `(` `)` { Type("Unit", Nil, $START) }  */
 case 48 => 
  { case List(_, _) =>  Type("Unit", Nil, dol$START) } 
 /* Types: List[Type] = S_4 { $S_4 }  */
 case 49 => 
  { case List(dol$S_4: List[SymbolType @unchecked]) =>  dol$S_4 } 
 /* S_1: Option[Unit] =  { None }  */
 case 50 => 
  { case List() =>  None } 
 /* S_1: Option[Unit] = `|` { Some(()) }  */
 case 51 => 
  { case List(_) =>  Some(()) } 
 /* S_2_L: List[NamedField] = NamedField { List($NamedField) }  */
 case 52 => 
  { case List(dol$NamedField: NamedField) =>  List(dol$NamedField) } 
 /* S_2_L: List[NamedField] = S_2_L NamedField { $NamedField :: $S_2_L }  */
 case 53 => 
  { case List(dol$S_2_L: List[NamedField @unchecked], dol$NamedField: NamedField) => 
        dol$NamedField :: dol$S_2_L
  }
 /* S_2: List[NamedField] = S_2_L { $S_2_L.reverse }  */
 case 54 => 
  { case List(dol$S_2_L: List[NamedField @unchecked]) =>  dol$S_2_L.reverse } 
 /* S_3: Option[org.sufrin.scalalr.stage2.AST.Name] =  { None }  */
 case 55 => 
  { case List() =>  None } 
 /* S_3: Option[org.sufrin.scalalr.stage2.AST.Name] = `%prec` ID { Some($ID) }  */
 case 56 => 
  { case List(_, dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>  Some(dol$ID) } 
 /* S_4_L: List[SymbolType] = Type { List($Type) }  */
 case 57 => 
  { case List(dol$Type: SymbolType) =>  List(dol$Type) } 
 /* S_4_L: List[SymbolType] = S_4_L `,` Type { $Type :: $S_4_L }  */
 case 58 => 
  { case List(dol$S_4_L: List[SymbolType @unchecked], _, dol$Type: SymbolType) => 
        dol$Type :: dol$S_4_L
  }
 /* S_4: List[SymbolType] = S_4_L { $S_4_L.reverse }  */
 case 59 => 
  { case List(dol$S_4_L: List[SymbolType @unchecked]) =>  dol$S_4_L.reverse } 

 }

}
