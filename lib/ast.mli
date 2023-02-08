type value =
  | VInt of int
  | VString of string
  | VList of expr list

and expr =
  | Const of value
  | Var of string

type ast =
  | Apply of string * ast list
  | Value of expr
