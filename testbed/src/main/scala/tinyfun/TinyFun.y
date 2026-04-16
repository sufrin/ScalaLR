
// notation TinyFun
%define lr.type lalr
%token NUM ID TOK-5 TOK-6 TOK-7 TOK-8 TOK-9 LEXICALERROR NL
%token TOK-12 TOK-13
%token TOK-14 TOK-15
%left TOK-12 TOK-13
%left TOK-14 TOK-15
// Special symbols
// "(" TOK-5
// ")" TOK-6
// "[" TOK-7
// "]" TOK-8
// "," TOK-9
// "+" TOK-12
// "-" TOK-13
// "*" TOK-14
// "/" TOK-15
%%
loop: ;
loop:  loop command NL;
loop:  error NL;
command:  expr;
expr:  ID;
expr:  NUM;
expr:  expr TOK-14 expr;
expr:  expr TOK-12 expr;
expr:  expr TOK-15 expr;
expr:  expr TOK-13 expr;
expr:  TOK-5 expr TOK-6;
expr:  ID TOK-5 exprs TOK-6;
exprs:  expr;
exprs:  exprs TOK-9 expr;