open Ast
open Typedast

let create_tvar =
  let r = ref 0 in
  fun () ->
    incr r;
    { id = !r; def = None }

let rec head = function
  | TVar { def = Some t; _ } -> head t
  | TSeq (t1, t2) -> TSeq (head t1, head t2)
  | _ as t -> t

let rec str_of_typ = function
  | TInt -> "int"
  | TString -> "string"
  | TVoid -> "void"
  | TSeq (t1, t2) -> "(" ^ str_of_typ t1 ^ " -> " ^ str_of_typ t2 ^ ")"
  | TVar { def = Some t; _ } -> str_of_typ t
  | TVar { def = None; _ } -> "'a"

let occur tv t =
  let t = head t in
  match t with TVar t -> t = tv | _ -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | TInt, TInt | TString, TString -> ()
  | TVar v1, TVar v2 when v1.id = v2.id -> ()
  | TVar v1, t2 ->
      if occur v1 t2 then failwith "";
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, TVar _ -> unify t2 t1
  | TSeq (t11, t12), TSeq (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | TSeq (t11, _), t2 -> unify t11 t2
  | t1, TSeq (t21, _) -> unify t1 t21
  | t1, t2 ->
      failwith ("Expected " ^ str_of_typ t1 ^ " but got " ^ str_of_typ t2)

module VMap = Map.Make (String)

let baseMap : Typedast.typ VMap.t =
  VMap.empty |> VMap.add "+" (TSeq (TInt, TSeq (TInt, TInt)))

let rec type_of_expr map = function
  | Var s -> VMap.find s map
  | Const v -> ( match v with VInt _ -> TInt | VString _ -> TString)
  | Apply (s, expr) ->
      let t = VMap.find s map in

      let rec seq = function
        | [] -> TVar (create_tvar ())
        | e :: tl -> TSeq (type_of_expr map e, seq tl)
      in

      let rec h t l =
        match (head t, l) with
        | TSeq (t1, t'), expr_1 :: exprl ->
            unify t1 (type_of_expr map expr_1);
            h t' exprl
        | t, [] -> head t
        | TVar ({ def = None; _ } as t), exprl ->
            t.def <- Some (seq exprl);
            h (TVar t) exprl
        | _, _ -> assert false
      in

      h t expr

let rec typechecker map : Ast.ast list -> Typedast.ast list = function
  | Defar (s, expr) :: ll ->
      let t = type_of_expr map expr in
      let map = VMap.add s t map in
      { ast = Defar (s, expr); typ = TVoid } :: typechecker map ll
  | Value v :: ll ->
      let t = type_of_expr map v in
      { ast = Value v; typ = t } :: typechecker map ll
  | Defun (s, params, ast) :: ll ->
      let map_params =
        List.fold_left
          (fun m ss -> VMap.add ss (TVar (create_tvar ())) m)
          map params
      in
      let t = type_of_expr map_params ast in
      let final_t =
        if params = [] then TSeq (TVoid, t)
        else
          let rec join = function
            | [] -> t
            | ss :: tl ->
                let t = VMap.find ss map_params in
                TSeq (t, join tl)
          in
          join params
      in
      let map = VMap.add s final_t map in
      { ast = Defun (s, params, ast); typ = final_t } :: typechecker map ll
  | [] -> []
