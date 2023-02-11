%{
  open Ast
%}

%token LPARENT "("
%token RPARENT ")"
%token LSBRAK "["
%token RSBRAK "]"
%token COMMA
%token <string> NAME
%token <Ast.value> VALUE
%token DEFUN DEFAR
%token EOF

%start <ast list> main

%%

expr:
  | v = VALUE { Const v }
  | n = NAME { Var n }
  | "(" n = NAME p = list(expr) ")"
    { Apply (n, p) }

ast:
  | "(" DEFAR n = NAME e = expr ")"
    { Defar (n, e) }
  | "(" DEFUN n = NAME "[" p = list(NAME)? "]" a = expr ")"
    { 
      let p = match p with Some p -> p | None -> [] in
      Defun (n, p, a) 
    }
  | e = expr { Value e }

main:
  | astl = list(ast) EOF { astl }
