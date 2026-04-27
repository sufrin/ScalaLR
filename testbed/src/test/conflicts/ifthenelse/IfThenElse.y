
// notation IfThenElse
%define lr.type canonical-lr
%token IF THEN ELSE ID TOK-7
// Special symbols
// "+" TOK-7
%%
expr:  ID;
expr:  expr TOK-7 ID;
expr:  IF expr THEN expr;
expr:  IF expr THEN expr ELSE expr;