

package scalalr.stage2
object Reduction {



 import org.sufrin.scalalr.SourceLocation
 import org.sufrin.scalalr.stage2.AST._
 import org.sufrin.scalalr.stage2.Normalization._
 import org.sufrin.utility.PrettyPrint._
 import org.sufrin.utility.SourceTextCursor
 import scalalr.stage2.Scanner

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
 /* Notation: Notation = Prefix `%rules` INCLUDE Rules OPTNL { $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) }  */
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
 /* OPTNL: Unit =  { () }  */
 case 18 => 
  { case List() =>  () } 
 /* OPTNL: Unit = SEPARATOR { () }  */
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
 /* Rule: Rule = LHS `=` OptBar RHS { Rule($LHS, $RHS, $START) }  */
 case 27 => 
  { case List(dol$LHS: TypedNonterminal, _, _, dol$RHS: List[Production @unchecked]) => 
        Rule(dol$LHS, dol$RHS, dol$START)
  }
 /* OptBar: Unit = `|` { () }  */
 case 28 => 
  { case List(_) =>  () } 
 /* OptBar: Unit =  { () }  */
 case 29 => 
  { case List() =>  () } 
 /* LHS: TypedNonterminal = ID `:` Type { (TypedNonterminal($ID.warnQuoted, $Type, $START)) }  */
 case 30 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: SymbolType) => 
        (TypedNonterminal(dol$ID.warnQuoted, dol$Type, dol$START))
  }
 /* LHS: TypedNonterminal = ID { (TypedNonterminal($ID.warnQuoted, TypeVariable($ID), $START)) }  */
 case 31 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        (TypedNonterminal(dol$ID.warnQuoted, TypeVariable(dol$ID), dol$START))
  }
 /* RHS: List[Production] = Production { List($Production) }  */
 case 32 => 
  { case List(dol$Production: Production) =>  List(dol$Production) } 
 /* RHS: List[Production] = Production `|` RHS { $Production :: $RHS }  */
 case 33 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
        dol$Production :: dol$RHS
  }
 /* Production: Production = NamedFields Action Precedence { Production($NamedFields, $Action, $Precedence, $START) }  */
 case 34 => 
  { case List(dol$NamedFields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Name @unchecked]) => 
        Production(dol$NamedFields, dol$Action, dol$Precedence, dol$START)
  }
 /* NamedFields: List[NamedField] = `%empty` { Nil }  */
 case 35 => 
  { case List(_) =>  Nil } 
 /* NamedFields: List[NamedField] = NamedField { List($NamedField) }  */
 case 36 => 
  { case List(dol$NamedField: NamedField) =>  List(dol$NamedField) } 
 /* NamedFields: List[NamedField] = NamedField NamedFields { $NamedField :: $NamedFields }  */
 case 37 => 
  { case List(dol$NamedField: NamedField, dol$NamedFields: List[NamedField @unchecked]) => 
        dol$NamedField :: dol$NamedFields
  }
 /* NamedField: NamedField = FIELD { NamedField(theFieldName = None, theField = $FIELD, $START) }  */
 case 38 => 
  { case List(dol$FIELD: Name) => 
        NamedField(theFieldName = None, theField = dol$FIELD, dol$START)
  }
 /* NamedField: NamedField = theFieldName: ID `:` theName: FIELD { NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) }  */
 case 39 => 
  { case List(dol$theFieldName: org.sufrin.scalalr.stage2.AST.Name, _, dol$theName: Name) => 
        NamedField(theFieldName = Some(dol$theFieldName.warnQuoted), dol$theName, dol$START)
  }
 /* FIELD: Name = ID { $ID }  */
 case 40 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>  dol$ID } 
 /* FIELD: Name = `(` NamedFields `)` REPEAT { syntheticRuleName($NamedFields, $REPEAT, $START, $END) }  */
 case 41 => 
  { case List(_, dol$NamedFields: List[NamedField @unchecked], _, dol$REPEAT: Repeat) => 
        syntheticRuleName(dol$NamedFields, dol$REPEAT, dol$START, dol$END)
  }
 /* FIELD: Name = `(` NamedFields `)` `.` `.` `.` { syntheticRuleName($NamedFields, Ellipsis, $START, $END) }  */
 case 42 => 
  { case List(_, dol$NamedFields: List[NamedField @unchecked], _, _, _, _) => 
        syntheticRuleName(dol$NamedFields, Ellipsis, dol$START, dol$END)
  }
 /* REPEAT: Repeat = `?` { MaybeOne }  */
 case 43 => 
  { case List(_) =>  MaybeOne } 
 /* REPEAT: Repeat = `*` { NoneOrMore }  */
 case 44 => 
  { case List(_) =>  NoneOrMore } 
 /* REPEAT: Repeat = `+` { OneOrMore }  */
 case 45 => 
  { case List(_) =>  OneOrMore } 
 /* REPEAT: Repeat = `*` `.` `.` { RightNoneOrMore }  */
 case 46 => 
  { case List(_, _, _) =>  RightNoneOrMore } 
 /* REPEAT: Repeat = `+` `.` `.` { RightOneOrMore }  */
 case 47 => 
  { case List(_, _, _) =>  RightOneOrMore } 
 /* Precedence: Option[Name] =  { None }  */
 case 48 => 
  { case List() =>  None } 
 /* Precedence: Option[Name] = `%prec` ID { Some($ID) }  */
 case 49 => 
  { case List(_, dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>  Some(dol$ID) } 
 /* Type: SymbolType = ID { Type($ID.withoutQuotes, Nil, $START) }  */
 case 50 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        Type(dol$ID.withoutQuotes, Nil, dol$START)
  }
 /* Type: SymbolType = ID `[` Types `]` { Type($ID.withoutQuotes, $Types, $START) }  */
 case 51 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Types: List[Type @unchecked], _) => 
        Type(dol$ID.withoutQuotes, dol$Types, dol$START)
  }
 /* Type: SymbolType = `(` Types `)` { makeTupleType($Types, $START) }  */
 case 52 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
        makeTupleType(dol$Types, dol$START)
  }
 /* Type: SymbolType = `(` `)` { Type("Unit", Nil, $START) }  */
 case 53 => 
  { case List(_, _) =>  Type("Unit", Nil, dol$START) } 
 /* Types: List[Type] = Type { List($Type) }  */
 case 54 => 
  { case List(dol$Type: SymbolType) =>  List(dol$Type) } 
 /* Types: List[Type] = Type `,` Types { $Type :: $Types }  */
 case 55 => 
  { case List(dol$Type: SymbolType, _, dol$Types: List[Type @unchecked]) => 
        dol$Type :: dol$Types
  }
 /* Action: Option[Expression] =  { None }  */
 case 56 => 
  { case List() =>  None } 
 /* Action: Option[Expression] = CODE { Some(CodeExpression($CODE)) }  */
 case 57 => 
  { case List(dol$CODE: String) =>  Some(CodeExpression(dol$CODE)) } 
 /* Action: Option[Expression] = `=>` Scala { Some(ScalaExpression($Scala, $START)) }  */
 case 58 => 
  { case List(_, dol$Scala: Scala) =>  Some(ScalaExpression(dol$Scala, dol$START)) } 
 /* Scala: Scala = ScalaAtom { $ScalaAtom }  */
 case 59 => 
  { case List(dol$ScalaAtom: Scala) =>  dol$ScalaAtom } 
 /* Scala: Scala = fun: ScalaAtom `(` args: Scalas `)` { Apply($fun, $args) }  */
 case 60 => 
  { case List(dol$fun: Scala, _, dol$args: List[Scala @unchecked], _) => 
        Apply(dol$fun, dol$args)
  }
 /* Scala: Scala = lhs: Scala `.` rhs: Scala { Dot($lhs, $rhs) }  */
 case 61 => 
  { case List(dol$lhs: Scala, _, dol$rhs: Scala) =>  Dot(dol$lhs, dol$rhs) } 
 /* Scala: Scala = lhs: Scala `::` rhs: Scala { Infix("::", $lhs, $rhs) }  */
 case 62 => 
  { case List(dol$lhs: Scala, _, dol$rhs: Scala) =>  Infix("::", dol$lhs, dol$rhs) } 
 /* Scala: Scala = lhs: Scala `+` rhs: Scala { Infix("+", $lhs, $rhs) }  */
 case 63 => 
  { case List(dol$lhs: Scala, _, dol$rhs: Scala) =>  Infix("+", dol$lhs, dol$rhs) } 
 /* Scala: Scala = lhs: Scala `-` rhs: Scala { Infix("-", $lhs, $rhs) }  */
 case 64 => 
  { case List(dol$lhs: Scala, _, dol$rhs: Scala) =>  Infix("-", dol$lhs, dol$rhs) } 
 /* Scalas: List[Scala] =  { Nil }  */
 case 65 => 
  { case List() =>  Nil } 
 /* Scalas: List[Scala] = ScalaPlus { $ScalaPlus.reverse }  */
 case 66 => 
  { case List(dol$ScalaPlus: List[Scala @unchecked]) =>  dol$ScalaPlus.reverse } 
 /* ScalaPlus: List[Scala] = Scala { List($Scala) }  */
 case 67 => 
  { case List(dol$Scala: Scala) =>  List(dol$Scala) } 
 /* ScalaPlus: List[Scala] = ScalaPlus `,` Scala { $Scala :: $ScalaPlus }  */
 case 68 => 
  { case List(dol$ScalaPlus: List[Scala @unchecked], _, dol$Scala: Scala) => 
        dol$Scala :: dol$ScalaPlus
  }
 /* ScalaAtom: Scala = ID { Id($ID, $START) }  */
 case 69 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) =>  Id(dol$ID, dol$START) } 
 /* ScalaAtom: Scala = `$` ID { Dollar(Id($ID, $START)) }  */
 case 70 => 
  { case List(_, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        Dollar(Id(dol$ID, dol$START))
  }
 /* ScalaAtom: Scala = NUM { Num($NUM, $START) }  */
 case 71 => 
  { case List(dol$NUM: String) =>  Num(dol$NUM, dol$START) } 
 /* ScalaAtom: Scala = `(` Scala `)` { Bra($Scala) }  */
 case 72 => 
  { case List(_, dol$Scala: Scala, _) =>  Bra(dol$Scala) } 

 }

}
