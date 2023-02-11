type value = VInt of int | VString of string
type expr = Const of value | Var of string | Apply of string * expr list

type ast =
  | Defun of string * string list * expr
  | Defar of string * expr
  | Value of expr
