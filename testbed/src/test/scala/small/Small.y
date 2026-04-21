
// notation Small
%define lr.type canonical-lr
%token ID TOK-4 LEXICALERROR
// Special symbols
// ";" TOK-4
%%
ids:  idList;
idList:  ID;
idList:  idList TOK-4 idList;