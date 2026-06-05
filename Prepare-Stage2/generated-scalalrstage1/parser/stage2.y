
// notation stage2
%define lr.type lalr
%token ID NUM CODE COMMENT LEXICALERROR TOK-8 TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 TOK-19 TOK-20 TOK-21 TOK-22 SEPARATOR TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32 TOK-33 TOK-34 TOK-35 TOK-36 TOK-37 TOK-38 TOK-39
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
// "." TOK-19
// "+" TOK-20
// "*" TOK-21
// "?" TOK-22
// "%path" TOK-24
// "%type" TOK-25
// "%empty" TOK-26
// "%notation" TOK-27
// "%package" TOK-28
// "%token" TOK-29
// "%left" TOK-30
// "%right" TOK-31
// "%non" TOK-32
// "%rules" TOK-33
// "%include" TOK-34
// "%prec" TOK-35
// "%tables" TOK-36
// "%dialect" TOK-37
// "%scalalr" TOK-38
// "%signature" TOK-39
%%
Notation:  Prefix TOK-33 INCLUDE Rules OPTNL;
Prefix: ;
Prefix:  Prefix TOK-27 ID;
Prefix:  Prefix TOK-28 ID;
Prefix:  Prefix TOK-24 ID;
Prefix:  Prefix TOK-36 ID;
Prefix:  Prefix TOK-34 CODE;
Prefix:  Prefix TOK-29 TypedTerminals;
Prefix:  Prefix TOK-30 TypedTerminals;
Prefix:  Prefix TOK-31 TypedTerminals;
Prefix:  Prefix TOK-32 TypedTerminals;
Prefix:  Prefix TOK-35 TypedTerminals;
Prefix:  Prefix TOK-37 ID;
Prefix:  Prefix TOK-38 ID;
Prefix:  Prefix TOK-39 ID;
INCLUDE:  TOK-34 CODE SEPARATOR;
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
NamedFields:  TOK-26;
NamedFields:  NamedField;
NamedFields:  NamedField NamedFields;
NamedField:  FIELD;
NamedField:  ID TOK-13 FIELD;
FIELD:  ID;
FIELD:  TOK-16 NamedFields TOK-17 REPEAT;
FIELD:  TOK-16 NamedFields TOK-17 TOK-19 TOK-19 TOK-19;
REPEAT:  TOK-22;
REPEAT:  TOK-21;
REPEAT:  TOK-20;
REPEAT:  TOK-21 TOK-19 TOK-19;
REPEAT:  TOK-20 TOK-19 TOK-19;
Action: ;
Action:  CODE;
Precedence: ;
Precedence:  TOK-35 ID;
Type:  ID;
Type:  ID TOK-8 Types TOK-9;
Type:  TOK-16 Types TOK-17;
Type:  TOK-16 TOK-17;
Types:  Type;
Types:  Type TOK-18 Types;