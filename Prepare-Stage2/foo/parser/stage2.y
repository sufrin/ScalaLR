
// notation stage2
%define lr.type lalr
%token ID NUM CODE STRING COMMENT LEXICALERROR TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 TOK-19 TOK-20 TOK-21 SEPARATOR TOK-23 TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32 TOK-33 TOK-34 TOK-35 TOK-36 TOK-37 TOK-38 TOK-39
%token TOK-40
%token TOK-41 TOK-42
%token TOK-43
%token TOK-44 // "$" (appears in the definition of ScalaID)
%right TOK-40
%left TOK-41 TOK-42
%right TOK-43
// Special symbols
// "[" TOK-9
// "]" TOK-10
// ";" TOK-11
// "=" TOK-12
// "|" TOK-13
// ":" TOK-14
// "{" TOK-15
// "}" TOK-16
// "(" TOK-17
// ")" TOK-18
// "," TOK-19
// "*" TOK-20
// "?" TOK-21
// "=>" TOK-23
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
// "::" TOK-40
// "+" TOK-41
// "-" TOK-42
// "." TOK-43
// "$" TOK-44
%%
Notation:  Prefix TOK-33 INCLUDE Rules OPTNL;
Prefix: ;
Prefix:  Prefix TOK-27 ID;
Prefix:  Prefix TOK-28 ID;
Prefix:  Prefix TOK-24 STRINGorID;
Prefix:  Prefix TOK-36 STRINGorID;
Prefix:  Prefix TOK-34 CODE;
Prefix:  Prefix TOK-29 TypedTerminals;
Prefix:  Prefix TOK-30 TypedTerminals;
Prefix:  Prefix TOK-31 TypedTerminals;
Prefix:  Prefix TOK-32 TypedTerminals;
Prefix:  Prefix TOK-35 TypedTerminals;
Prefix:  Prefix TOK-37 STRINGorID;
Prefix:  Prefix TOK-38 STRINGorID;
Prefix:  Prefix TOK-39 STRINGorID;
INCLUDE:  TOK-34 CODE SEPARATOR;
INCLUDE: ;
OPTNL: ;
OPTNL:  SEPARATOR;
STRINGorID:  ID;
STRINGorID:  STRING;
TypedTerminals: ;
TypedTerminals:  TypedTerminal TypedTerminals;
TypedTerminal:  STRINGorID TOK-14 Type;
TypedTerminal:  STRINGorID TOK-17 Type TOK-18;
TypedTerminal:  STRINGorID;
Rules:  Rule;
Rules:  Rules SEPARATOR Rule;
Rule:  LHS TOK-12 OptBar RHS;
OptBar:  TOK-13;
OptBar: ;
LHS:  ID TOK-14 Type;
LHS:  ID;
RHS:  Production;
RHS:  Production TOK-13 RHS;
Production:  NamedFields Action Precedence;
NamedFields:  TOK-26;
NamedFields:  NamedField;
NamedFields:  NamedField NamedFields;
NamedField:  FIELD;
NamedField:  ID TOK-14 FIELD;
FIELD:  ID;
FIELD:  STRING;
FIELD:  TOK-17 NamedFields TOK-18 REPEAT;
REPEAT:  TOK-21;
REPEAT:  TOK-20;
REPEAT:  TOK-41;
REPEAT:  TOK-20 TOK-43 TOK-43;
REPEAT:  TOK-41 TOK-43 TOK-43;
REPEAT:  TOK-43 TOK-43 TOK-43;
Precedence: ;
Precedence:  TOK-35 ID;
Type:  ID;
Type:  ID TOK-9 Types TOK-10;
Type:  TOK-17 Types TOK-18;
Type:  TOK-17 TOK-18;
Types:  Type;
Types:  Type TOK-19 Types;
Action: ;
Action:  CODE;
Action:  TOK-23 Scala;
Scala:  ScalaAtom;
Scala:  ScalaID TOK-17 Scalas TOK-18;
Scala:  ScalaID TOK-43 ScalaID TOK-17 Scalas TOK-18;
Scala:  ScalaID TOK-43 ScalaID;
Scala:  Scala TOK-40 Scala;
Scala:  Scala TOK-41 Scala;
Scala:  Scala TOK-42 Scala;
Scalas: ;
Scalas:  ScalaPlus;
ScalaPlus:  ScalaPlus TOK-19 Scala;
ScalaPlus:  Scala;
ScalaAtom:  ScalaID;
ScalaAtom:  NUM;
ScalaAtom:  STRING;
ScalaAtom:  TOK-17 Scalas TOK-18;
ScalaID:  ID;
ScalaID:  TOK-44 ID;