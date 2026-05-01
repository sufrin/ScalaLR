
// notation stage2
%define lr.type lalr
%token ID NUM CODE COMMENT LEXICALERROR TOK-8 TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 NL TOK-20 TOK-21 TOK-22 TOK-23 TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32 TOK-33 TOK-34
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
%%
command:  Notation;
Notation:  Prologue RULES INCLUDE Rules;
Prologue: ;
Prologue:  Prologue TOK-23 ID;
Prologue:  Prologue TOK-24 ID;
Prologue:  Prologue TOK-20 ID;
Prologue:  Prologue TOK-32 ID;
Prologue:  Prologue TOK-30 CODE;
RULES:  TOK-29;
INCLUDE:  TOK-30 CODE NL;
INCLUDE: ;
Rules:  Rule;
Rules:  Rules NL Rule;
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
NamedField:  ID;
NamedField:  ID TOK-13 ID;
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