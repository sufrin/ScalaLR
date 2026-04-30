
// notation Expr
%define lr.type lalr
%token ID TOK-4 TOK-5 TOK-6 TOK-7 TOK-8 LEXICALERROR
%token TOK-10
%token TOK-11
%left TOK-10
%left TOK-11
// Special symbols
// "(" TOK-4
// ")" TOK-5
// "[" TOK-6
// "]" TOK-7
// ";" TOK-8
// "+" TOK-10
// "*" TOK-11
%%
exprs:  expr;
exprs:  exprs TOK-8 expr;
exprs:  error;
expr:  ID;
expr:  expr TOK-11 expr;
expr:  expr TOK-10 expr;
expr:  TOK-4 expr TOK-5;
expr:  TOK-6 expr TOK-7;