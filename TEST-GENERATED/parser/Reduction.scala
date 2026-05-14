

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
 /* command: Unit@343.14 = Notation{ translate($Notation) } */
 case 1 => 
  { case List(dol$Notation: Notation) =>  translate(dol$Notation) } 
 /* Notation: Notation@345.19 = Prefix `%rules` INCLUDE Rules OPTSEPARATOR{ $Prefix.copy(theRules = $Rules.reverse, theRulesInclude = $INCLUDE) } */
 case 2 => 
  { case List(dol$Prefix: Notation, _, dol$INCLUDE: String, dol$Rules: List[Rule @unchecked], dol$OPTSEPARATOR: Unit) => 
        dol$Prefix.copy(theRules = dol$Rules.reverse, theRulesInclude = dol$INCLUDE)
  }
 /* Prefix: Notation@355.17 = { Notation() } */
 case 3 => 
  { case List() =>  Notation() } 
 /* Prefix: Notation@355.17 = p: Prefix `%notation` ID{ $p.copy(theName=$ID.toString) } */
 case 4 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(theName=dol$ID.toString)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%package` ID{ $p.copy(thePackage=$ID.toString) } */
 case 5 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(thePackage=dol$ID.toString)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%path` ID{ $p.copy(theExplicitPath=$ID.asPath) } */
 case 6 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(theExplicitPath=dol$ID.asPath)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%tables` ID{ $p.copy(tablesType=mkTableType($ID.unQuoted)) } */
 case 7 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.copy(tablesType=mkTableType(dol$ID.unQuoted))
  }
 /* Prefix: Notation@355.17 = p: Prefix `%include` CODE{ $p.copy(theTokensInclude=$CODE) } */
 case 8 => 
  { case List(dol$p: Notation, _, dol$CODE: String) => 
        dol$p.copy(theTokensInclude=dol$CODE)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%token` TypedTerminals{ $p.withTokenDeclaration(Tokens)($TypedTerminals) } */
 case 9 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Tokens)(dol$TypedTerminals)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%left` TypedTerminals{ $p.withTokenDeclaration(Left)($TypedTerminals) } */
 case 10 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Left)(dol$TypedTerminals)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%right` TypedTerminals{ $p.withTokenDeclaration(Right)($TypedTerminals) } */
 case 11 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Right)(dol$TypedTerminals)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%non` TypedTerminals{ $p.withTokenDeclaration(Nonassoc)($TypedTerminals) } */
 case 12 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Nonassoc)(dol$TypedTerminals)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%prec` TypedTerminals{ $p.withTokenDeclaration(Prec)($TypedTerminals) } */
 case 13 => 
  { case List(dol$p: Notation, _, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$p.withTokenDeclaration(Prec)(dol$TypedTerminals)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%dialect` ID{ $p.withSignature($ID.unQuoted) } */
 case 14 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%scalalr` ID{ $p.withSignature($ID.unQuoted) } */
 case 15 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* Prefix: Notation@355.17 = p: Prefix `%signature` ID{ $p.withSignature($ID.unQuoted) } */
 case 16 => 
  { case List(dol$p: Notation, _, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        dol$p.withSignature(dol$ID.unQuoted)
  }
 /* INCLUDE: String@371.21 = `%include` CODE SEPARATOR{ $CODE } */
 case 17 => 
  { case List(_, dol$CODE: String, _) =>  dol$CODE } 
 /* INCLUDE: String@371.21 = { "" } */
 case 18 => 
  { case List() =>  "" } 
 /* OPTSEPARATOR: Unit@373.19 = { () } */
 case 19 => 
  { case List() =>  () } 
 /* OPTSEPARATOR: Unit@373.19 = SEPARATOR{ () } */
 case 20 => 
  { case List(_) =>  () } 
 /* TypedTerminals: List[TypedTerminal@375.35]@375.21 = { Nil } */
 case 21 => 
  { case List() =>  Nil } 
 /* TypedTerminals: List[TypedTerminal@375.35]@375.21 = TypedTerminal TypedTerminals{ $TypedTerminal :: $TypedTerminals } */
 case 22 => 
  { case List(dol$TypedTerminal: TypedTerminal, dol$TypedTerminals: List[TypedTerminal @unchecked]) => 
        dol$TypedTerminal :: dol$TypedTerminals
  }
 /* TypedTerminal: TypedTerminal@380.29 = ID `:` Type{ TypedTerminal($ID, $Type, $START) } */
 case 23 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type) => 
        TypedTerminal(dol$ID, dol$Type, dol$START)
  }
 /* TypedTerminal: TypedTerminal@380.29 = ID `(` Type `)`{ TypedTerminal($ID, $Type, $START) } */
 case 24 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type, _) => 
        TypedTerminal(dol$ID, dol$Type, dol$START)
  }
 /* TypedTerminal: TypedTerminal@380.29 = ID{ TypedTerminal($ID, NoType, $START) } */
 case 25 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        TypedTerminal(dol$ID, NoType, dol$START)
  }
 /* Rules: List[Rule@385.17]@385.12 = Rule{ List($Rule) } */
 case 26 => 
  { case List(dol$Rule: Rule) =>  List(dol$Rule) } 
 /* Rules: List[Rule@385.17]@385.12 = Rules SEPARATOR Rule{ $Rule :: $Rules } */
 case 27 => 
  { case List(dol$Rules: List[Rule @unchecked], _, dol$Rule: Rule) => 
        dol$Rule :: dol$Rules
  }
 /* Rule: Rule@388.11 = LHS `=` OptBar RHS{ Rule($LHS, $RHS, $START) } */
 case 28 => 
  { case List(dol$LHS: TypedNonterminal, _, dol$OptBar: Unit, dol$RHS: List[Production @unchecked]) => 
        Rule(dol$LHS, dol$RHS, dol$START)
  }
 /* OptBar: Unit@390.13 = `|`{ () } */
 case 29 => 
  { case List(_) =>  () } 
 /* OptBar: Unit@390.13 = { () } */
 case 30 => 
  { case List() =>  () } 
 /* LHS: TypedNonterminal@392.22 = ID `:` Type{ TypedNonterminal($ID.warnQuoted, $Type, $START) } */
 case 31 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Type: Type) => 
        TypedNonterminal(dol$ID.warnQuoted, dol$Type, dol$START)
  }
 /* LHS: TypedNonterminal@392.22 = ID{ TypedNonterminal($ID.warnQuoted, NoType, $START) } */
 case 32 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        TypedNonterminal(dol$ID.warnQuoted, NoType, dol$START)
  }
 /* RHS: List[Production@396.21]@396.10 = Production{ List($Production) } */
 case 33 => 
  { case List(dol$Production: Production) =>  List(dol$Production) } 
 /* RHS: List[Production@396.21]@396.10 = Production `|` RHS{ $Production :: $RHS } */
 case 34 => 
  { case List(dol$Production: Production, _, dol$RHS: List[Production @unchecked]) => 
        dol$Production :: dol$RHS
  }
 /* Production: Production@400.23 = NamedFields Action Precedence{ Production($NamedFields, $Action, $Precedence, $START) } */
 case 35 => 
  { case List(dol$NamedFields: List[NamedField @unchecked], dol$Action: Option[Expression @unchecked], dol$Precedence: Option[Terminal @unchecked]) => 
        Production(dol$NamedFields, dol$Action, dol$Precedence, dol$START)
  }
 /* NamedFields: List[NamedField@402.29]@402.18 = `%empty`{ Nil } */
 case 36 => 
  { case List(_) =>  Nil } 
 /* NamedFields: List[NamedField@402.29]@402.18 = NamedField{ List($NamedField) } */
 case 37 => 
  { case List(dol$NamedField: NamedField) =>  List(dol$NamedField) } 
 /* NamedFields: List[NamedField@402.29]@402.18 = NamedField NamedFields{ $NamedField :: $NamedFields } */
 case 38 => 
  { case List(dol$NamedField: NamedField, dol$NamedFields: List[NamedField @unchecked]) => 
        dol$NamedField :: dol$NamedFields
  }
 /* NamedField: NamedField@408.23 = ID{ NamedField(theFieldName = None, theField = $ID, $START) } */
 case 39 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        NamedField(theFieldName = None, theField = dol$ID, dol$START)
  }
 /* NamedField: NamedField@408.23 = theFieldName: ID `:` theName: ID{ NamedField(theFieldName = Some($theFieldName.warnQuoted), $theName, $START) } */
 case 40 => 
  { case List(dol$theFieldName: org.sufrin.scalalr.stage2.AST.Name, _, dol$theName: org.sufrin.scalalr.stage2.AST.Name) => 
        NamedField(theFieldName = Some(dol$theFieldName.warnQuoted), dol$theName, dol$START)
  }
 /* Action: Option[Expression@412.26]@412.15 = { None } */
 case 41 => 
  { case List() =>  None } 
 /* Action: Option[Expression@412.26]@412.15 = CODE{ Some($CODE) } */
 case 42 => 
  { case List(dol$CODE: String) =>  Some(dol$CODE) } 
 /* Precedence: Option[Terminal@414.29]@414.20 = { None } */
 case 43 => 
  { case List() =>  None } 
 /* Precedence: Option[Terminal@414.29]@414.20 = `%prec` ID{ Some(TypedTerminal($ID, NoType, $START)) } */
 case 44 => 
  { case List(_, dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        Some(TypedTerminal(dol$ID, NoType, dol$START))
  }
 /* Type: Type@416.11 = ID{ Type($ID.withoutQuotes, Nil, $START) } */
 case 45 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name) => 
        Type(dol$ID.withoutQuotes, Nil, dol$START)
  }
 /* Type: Type@416.11 = ID `[` Types `]`{ Type($ID.withoutQuotes, $Types, $START) } */
 case 46 => 
  { case List(dol$ID: org.sufrin.scalalr.stage2.AST.Name, _, dol$Types: List[Type @unchecked], _) => 
        Type(dol$ID.withoutQuotes, dol$Types, dol$START)
  }
 /* Type: Type@416.11 = `(` Types `)`{ makeTupleType($Types, $START) } */
 case 47 => 
  { case List(_, dol$Types: List[Type @unchecked], _) => 
        makeTupleType(dol$Types, dol$START)
  }
 /* Type: Type@416.11 = `(` `)`{ Type("Unit", Nil, $START) } */
 case 48 => 
  { case List(_, _) =>  Type("Unit", Nil, dol$START) } 
 /* Types: List[Type@422.17]@422.12 = Type{ List($Type) } */
 case 49 => 
  { case List(dol$Type: Type) =>  List(dol$Type) } 
 /* Types: List[Type@422.17]@422.12 = Type `,` Types{ $Type :: $Types } */
 case 50 => 
  { case List(dol$Type: Type, _, dol$Types: List[Type @unchecked]) => 
        dol$Type :: dol$Types
  }

 }

}
