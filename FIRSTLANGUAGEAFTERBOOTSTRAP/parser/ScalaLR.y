
// notation ScalaLR
%define lr.type lalr
%token ID NUM CODE COMMENT LEXICALERROR TOK-8 TOK-9 TOK-10 TOK-11 TOK-12 TOK-13 TOK-14 TOK-15 TOK-16 TOK-17 TOK-18 NL TOK-20 TOK-21 TOK-22 TOK-23 TOK-24 TOK-25 TOK-26 TOK-27 TOK-28 TOK-29 TOK-30 TOK-31 TOK-32
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
%%
command:  Notation;
Notation:  TOK-23 ID TOK-24 ID TOK-20 ID Tables OptInclude Tokens TOK-29 OptInclude Rules OptSemicolon;
OptInclude:  TOK-22;
OptInclude:  TOK-30 CODE;
Tokens: ;
Tokens:  TokenSpec Tokens;
TokenSpec:  TOK-26 TypedTerminals;
TokenSpec:  TOK-27 TypedTerminals;
TokenSpec:  TOK-28 TypedTerminals;
TokenSpec:  TOK-25 TypedTerminals;
TypedTerminals:  TypedTerminal;
TypedTerminals:  TypedTerminal TypedTerminals;
TypedTerminal:  ID TOK-13 Type;
TypedTerminal:  ID TOK-16 Type TOK-17;
TypedTerminal:  ID;
Tables: ;
Tables:  TOK-32 ID;
Rules:  Rule;
Rules:  Rules TOK-10 Rule;
Rule:  LHS TOK-11 RHS;
OptSemicolon: ;
OptSemicolon:  TOK-10;
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