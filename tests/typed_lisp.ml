let () =
  let open Typed_lisp__Ast in
  
  assert (
    (Typed_lisp.parse_code "(defun add [a b] (+ a b)) (add 10 15)")
    = ([
      Apply("defun", [Value (Var"add"); 
          Value (Const (VList 
            [Var "a"; Var "b"]
          )); 
          Apply ("+", [Value (Var "a"); Value (Var "b")]);
        ]
      );
      Apply ("add", [Value (Const (VInt 10)); Value (Const (VInt 15))])
    ]))

let () =
  let open Typed_lisp__Ast in
  assert (
    (Typed_lisp.parse_code "(defar a (+ 5 10)) a")
    = ([
      Apply("defar", [Value (Var "a"); 
          Apply ("+", [Value (Const (VInt 5)); Value (Const (VInt 10))]);
        ]
      );
      Value (Var "a")
    ]))

