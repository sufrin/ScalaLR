
// notation SAB
%define lr.type canonical-lr
%token a
%%
S:  A;
S:  B;
A:  a;
B:  a;