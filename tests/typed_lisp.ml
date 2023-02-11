let () =
  let open Typed_lisp__Ast in
  assert (
    Typed_lisp.parse_code "(defun add [a b] (+ a b)) (add 10 15)"
    = [
        Defun ("add", [ "a"; "b" ], Apply ("+", [ Var "a"; Var "b" ]));
        Value (Apply ("add", [ Const (VInt 10); Const (VInt 15) ]));
      ]
      (* Apply("defun", [Value (Var"add");  *)
      (*     Value (Const (VList  *)
      (*       [Var "a"; Var "b"] *)
      (*     ));  *)
      (*     Apply ("+", [Value (Var "a"); Value (Var "b")]); *)
      (*   ] *)
      (* ); *)
      (* Apply ("add", [Value (Const (VInt 10)); Value (Const (VInt 15))]) *))

let () =
  let open Typed_lisp__Ast in
  assert (
    Typed_lisp.parse_code "(defar a (+ 5 10)) a"
    = [
        Defar ("a", Apply ("+", [ Const (VInt 5); Const (VInt 10) ]));
        Value (Var "a");
      ]
      (* Apply("defar", [Value (Var "a");  *)
      (*     Apply ("+", [Value (Const (VInt 5)); Value (Const (VInt 10))]); *)
      (*   ] *)
      (* ); *)
      (* Value (Var "a") *))

let _ =
  let open Typed_lisp__Typechecker in
  let open Typed_lisp__Typedast in
  let open Typed_lisp__Ast in
  let code = Typed_lisp.parse_code "(+ 10 15)" in
  assert (
    typechecker baseMap code
    = [
        {
          ast = Value (Apply ("+", [ Const (VInt 10); Const (VInt 15) ]));
          typ = TInt;
        };
      ])

let _ =
  let open Typed_lisp__Typechecker in
  let open Typed_lisp__Typedast in
  let open Typed_lisp__Ast in
  let code = Typed_lisp.parse_code "(defun id [x] x)" in
  assert (
    typechecker baseMap code
    = [
        {
          ast = Defun ("id", [ "x" ], Var "x");
          typ = TSeq (TVar { id = 1; def = None }, TVar { id = 1; def = None });
        };
      ])

let _ =
  let open Typed_lisp__Typechecker in
  let open Typed_lisp__Typedast in
  let open Typed_lisp__Ast in
  let code = Typed_lisp.parse_code "(defun id [x y] (+ x y)) (id 10)" in
  assert (
    typechecker baseMap code
    = [
        {
          ast = Defun ("id", [ "x"; "y" ], Apply ("+", [ Var "x"; Var "y" ]));
          typ =
            TSeq
              ( TVar { id = 2; def = Some TInt },
                TSeq (TVar { id = 3; def = Some TInt }, TInt) );
        };
        {
          ast = Value (Apply ("id", [ Const (VInt 10) ]));
          typ = TSeq (TInt, TInt);
        };
      ])
