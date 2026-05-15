
// notation stage2
%define lr.type lalr
%token ID NUM CODE COMMENT LEXICALERROR TOK-8 TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 SEPARATOR TOK-20 TOK-21 TOK-22 TOK-23 TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32 TOK-33 TOK-34 TOK-35
%token TOK-36 // "?" (appears in the definition of REPEAT)
%token TOK-37 // "*" (appears in the definition of REPEAT)
%token TOK-38 // "+" (appears in the definition of REPEAT)
// Special symbols
// "[" TOK-8
// "]" TOK-9
// ";" TOK-10
// "=" TOK-11
// "|" TOK-12
// ":" TOK-13
// "{" TOK-14
// "}" TOK-15
// "(" TOK-16
// ")" TOK-17
// "," TOK-18
// "%path" TOK-20
// "%type" TOK-21
// "%empty" TOK-22
// "%notation" TOK-23
// "%package" TOK-24
// "%token" TOK-25
// "%left" TOK-26
// "%right" TOK-27
// "%non" TOK-28
// "%rules" TOK-29
// "%include" TOK-30
// "%prec" TOK-31
// "%tables" TOK-32
// "%dialect" TOK-33
// "%scalalr" TOK-34
// "%signature" TOK-35
// "?" TOK-36
// "*" TOK-37
// "+" TOK-38
%%
Notation:  Prefix TOK-29 INCLUDE Rules OPTNL;
Prefix: ;
Prefix:  Prefix TOK-23 ID;
Prefix:  Prefix TOK-24 ID;
Prefix:  Prefix TOK-20 ID;
Prefix:  Prefix TOK-32 ID;
Prefix:  Prefix TOK-30 CODE;
Prefix:  Prefix TOK-25 TypedTerminals;
Prefix:  Prefix TOK-26 TypedTerminals;
Prefix:  Prefix TOK-27 TypedTerminals;
Prefix:  Prefix TOK-28 TypedTerminals;
Prefix:  Prefix TOK-33 ID;
Prefix:  Prefix TOK-34 ID;
Prefix:  Prefix TOK-35 ID;
INCLUDE:  TOK-30 CODE SEPARATOR;
INCLUDE: ;
OPTNL: ;
OPTNL:  SEPARATOR;
TypedTerminals: ;
TypedTerminals:  TypedTerminal TypedTerminals;
TypedTerminal:  ID TOK-13 Type;
TypedTerminal:  ID TOK-16 Type TOK-17;
TypedTerminal:  ID;
Rules:  Rule;
Rules:  Rules SEPARATOR Rule;
Rule:  LHS TOK-11 OptBar RHS;
OptBar:  TOK-12;
OptBar: ;
LHS:  ID TOK-13 Type;
LHS:  ID;
RHS:  Production;
RHS:  Production TOK-12 RHS;
Production:  NamedFields Action Precedence;
NamedFields:  TOK-22;
NamedFields:  NamedField;
NamedFields:  NamedField NamedFields;
NamedField:  FIELD;
NamedField:  ID TOK-13 ID;
FIELD:  ID;
FIELD:  TOK-16 NamedFields TOK-17 REPEAT;
REPEAT:  TOK-36;
REPEAT:  TOK-37;
REPEAT:  TOK-38;
Action: ;
Action:  CODE;
Precedence: ;
Precedence:  TOK-31 ID;
Type:  ID;
Type:  ID TOK-8 Types TOK-9;
Type:  TOK-16 Types TOK-17;
Type:  TOK-16 TOK-17;
Types:  Type;
Types:  Type TOK-18 Types;