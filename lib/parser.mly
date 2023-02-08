%{
  open Ast
%}

%token LPARENT "("
%token RPARENT ")"
%token LSBRAK "["
%token RSBRAK "]"
%token <string> NAME
%token <Ast.value> VALUE
%token EOF

%start <ast list> main

%%

expr:
  | v = VALUE { Const v }
  | n = NAME { Var n }
  | "[" v = list(expr) "]"
    { Const (VList v) }

ast:
  | e = expr { Value e }
  | "(" n = NAME p = list(ast) ")"
    { Apply (n, p) }

main:
  | astl = list(ast) EOF { astl }
