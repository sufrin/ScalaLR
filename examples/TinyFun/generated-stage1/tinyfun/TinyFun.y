
// notation TinyFun
%define lr.type lalr
%token NUM ID TOK-5 TOK-6 TOK-7 TOK-8 TOK-9 LEXICALERROR NL QUIT LOG
%token TOK-14
%token TOK-15 TOK-16
%token TOK-17 TOK-18
%token TOK-19
%right TOK-14
%left TOK-15 TOK-16
%left TOK-17 TOK-18
%right TOK-19
// Special symbols
// "(" TOK-5
// ")" TOK-6
// "[" TOK-7
// "]" TOK-8
// "," TOK-9
// "=" TOK-14
// "+" TOK-15
// "-" TOK-16
// "*" TOK-17
// "/" TOK-18
// "^" TOK-19
%%
loop: ;
loop:  loop command NL;
loop:  loop error NL;
command:  exprs;
command:  QUIT;
command:  LOG;
command: ;
expr:  ID;
expr:  NUM;
expr:  ID TOK-14 expr;
expr:  expr TOK-19 expr;
expr:  expr TOK-17 expr;
expr:  expr TOK-15 expr;
expr:  expr TOK-18 expr;
expr:  expr TOK-16 expr;
expr:  TOK-5 expr TOK-6;
expr:  TOK-5 error TOK-6;
expr:  ID TOK-5 exprs TOK-6;
exprs:  expr;
exprs:  exprs TOK-9 expr;