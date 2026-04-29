
// notation TinyFun
%define lr.type lalr
%token NUM ID TOK-5 TOK-6 TOK-7 TOK-8 TOK-9 LEXICALERROR NL QUIT
%token TOK-13
%token TOK-14 TOK-15
%token TOK-16 TOK-17
%token TOK-18
%right TOK-13
%left TOK-14 TOK-15
%left TOK-16 TOK-17
%right TOK-18
// Special symbols
// "(" TOK-5
// ")" TOK-6
// "[" TOK-7
// "]" TOK-8
// "," TOK-9
// "=" TOK-13
// "+" TOK-14
// "-" TOK-15
// "*" TOK-16
// "/" TOK-17
// "^" TOK-18
%%
loop: ;
loop:  loop command NL;
loop:  loop error NL;
command:  exprs;
command:  QUIT;
command: ;
expr:  ID;
expr:  NUM;
expr:  ID TOK-13 expr;
expr:  expr TOK-18 expr;
expr:  expr TOK-16 expr;
expr:  expr TOK-14 expr;
expr:  expr TOK-17 expr;
expr:  expr TOK-15 expr;
expr:  TOK-5 expr TOK-6;
expr:  ID TOK-5 exprs TOK-6;
exprs:  expr;
exprs:  exprs TOK-9 expr;