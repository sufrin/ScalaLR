
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
Types:  Type;
Types:  Type TOK-18 Types;
Type:  ID;
Type:  ID TOK-8 Types TOK-9;
Type:  TOK-16 Types TOK-17;
Type:  TOK-16 TOK-17;
Precedence: ;
Precedence:  TOK-31 ID;
Action: ;
Action:  CODE;
NamedField:  ID;
NamedField:  ID TOK-13 ID;
NamedFields:  TOK-22;
NamedFields:  NamedField;
NamedFields:  NamedField NamedFields;
Production:  NamedFields Action Precedence;
RHS:  Production;
RHS:  Production TOK-12 RHS;
LHS:  ID TOK-13 Type;
LHS:  ID;
OptSemicolon: ;
OptSemicolon:  TOK-10;
Rule:  LHS TOK-11 RHS;
Rules:  Rule;
Rules:  Rules TOK-10 Rule;
Tables: ;
Tables:  TOK-32 ID;
TypedTerminal:  ID TOK-13 Type;
TypedTerminal:  ID TOK-16 Type TOK-17;
TypedTerminal:  ID;
TypedTerminals:  TypedTerminal;
TypedTerminals:  TypedTerminal TypedTerminals;
TokenSpec:  TOK-26 TypedTerminals;
TokenSpec:  TOK-27 TypedTerminals;
TokenSpec:  TOK-28 TypedTerminals;
TokenSpec:  TOK-25 TypedTerminals;
Tokens: ;
Tokens:  TokenSpec Tokens;
OptInclude:  TOK-22;
OptInclude:  TOK-30 CODE;
Notation:  TOK-23 ID TOK-24 ID TOK-20 ID Tables OptInclude Tokens TOK-29 OptInclude Rules OptSemicolon;
command:  Notation;