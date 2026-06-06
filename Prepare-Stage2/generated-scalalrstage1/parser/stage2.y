
// notation stage2
%define lr.type lalr
%token ID NUM CODE COMMENT LEXICALERROR TOK-8 TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 TOK-19 TOK-20 SEPARATOR TOK-22 TOK-23 TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32 TOK-33 TOK-34 TOK-35 TOK-36 TOK-37
%token TOK-38
%token TOK-39 TOK-40
%token TOK-41
%token TOK-42 // "=>" (appears in the definition of Action)
%token TOK-43 // "$" (appears in the definition of ScalaAtom)
%right TOK-38
%left TOK-39 TOK-40
%right TOK-41
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
// "*" TOK-19
// "?" TOK-20
// "%path" TOK-22
// "%type" TOK-23
// "%empty" TOK-24
// "%notation" TOK-25
// "%package" TOK-26
// "%token" TOK-27
// "%left" TOK-28
// "%right" TOK-29
// "%non" TOK-30
// "%rules" TOK-31
// "%include" TOK-32
// "%prec" TOK-33
// "%tables" TOK-34
// "%dialect" TOK-35
// "%scalalr" TOK-36
// "%signature" TOK-37
// "::" TOK-38
// "+" TOK-39
// "-" TOK-40
// "." TOK-41
// "=>" TOK-42
// "$" TOK-43
%%
Notation:  Prefix TOK-31 INCLUDE Rules OPTNL;
Prefix: ;
Prefix:  Prefix TOK-25 ID;
Prefix:  Prefix TOK-26 ID;
Prefix:  Prefix TOK-22 ID;
Prefix:  Prefix TOK-34 ID;
Prefix:  Prefix TOK-32 CODE;
Prefix:  Prefix TOK-27 TypedTerminals;
Prefix:  Prefix TOK-28 TypedTerminals;
Prefix:  Prefix TOK-29 TypedTerminals;
Prefix:  Prefix TOK-30 TypedTerminals;
Prefix:  Prefix TOK-33 TypedTerminals;
Prefix:  Prefix TOK-35 ID;
Prefix:  Prefix TOK-36 ID;
Prefix:  Prefix TOK-37 ID;
INCLUDE:  TOK-32 CODE SEPARATOR;
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
NamedFields:  TOK-24;
NamedFields:  NamedField;
NamedFields:  NamedField NamedFields;
NamedField:  FIELD;
NamedField:  ID TOK-13 FIELD;
FIELD:  ID;
FIELD:  TOK-16 NamedFields TOK-17 REPEAT;
FIELD:  TOK-16 NamedFields TOK-17 TOK-41 TOK-41 TOK-41;
REPEAT:  TOK-20;
REPEAT:  TOK-19;
REPEAT:  TOK-39;
REPEAT:  TOK-19 TOK-41 TOK-41;
REPEAT:  TOK-39 TOK-41 TOK-41;
Precedence: ;
Precedence:  TOK-33 ID;
Type:  ID;
Type:  ID TOK-8 Types TOK-9;
Type:  TOK-16 Types TOK-17;
Type:  TOK-16 TOK-17;
Types:  Type;
Types:  Type TOK-18 Types;
Action: ;
Action:  CODE;
Action:  TOK-42 Scala;
Scala:  ScalaAtom;
Scala:  ScalaAtom TOK-16 Scalas TOK-17;
Scala:  Scala TOK-41 Scala;
Scala:  Scala TOK-38 Scala;
Scala:  Scala TOK-39 Scala;
Scala:  Scala TOK-40 Scala;
Scalas: ;
Scalas:  ScalaPlus;
ScalaPlus:  Scala;
ScalaPlus:  ScalaPlus TOK-18 Scala;
ScalaAtom:  ID;
ScalaAtom:  TOK-43 ID;
ScalaAtom:  NUM;
ScalaAtom:  TOK-16 Scala TOK-17;