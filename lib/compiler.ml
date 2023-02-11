open Ast
open Typedast

let ctx = Llvm.create_context ()
let the_module = Llvm.create_module ctx "Typed_Lisp"
let builder = Llvm.builder ctx
let i32 = Llvm.i32_type ctx
let i8 = Llvm.i8_type ctx
let void = Llvm.void_type ctx
let string = Llvm.pointer_type i8

module VMap = Map.Make (String)

let back_to_main fname =
  let f =
    match Llvm.lookup_function fname the_module with
    | Some f -> f
    | None -> assert false
  in
  Llvm.position_at_end (Llvm.entry_block f) builder

let create_entry_block_alloca ctx name the_function typ =
  let builder =
    Llvm.builder_at ctx (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca typ name builder

let rec typ_lltyp = function
  | TInt -> i32
  | TString -> string
  | TVoid -> void
  | TSeq _ as t ->
      let rec h acc = function
        | (TInt | TVoid | TString | TVar _) as ret -> (typ_lltyp ret, snd acc)
        | TSeq ((TSeq _ as t1), t2) ->
            h (fst acc, (typ_lltyp t1 |> Llvm.pointer_type) :: snd acc) t2
        | TSeq (t1, t2) -> h (fst acc, typ_lltyp t1 :: snd acc) t2
      in
      let ret_t, arr_t = h (typ_lltyp TVoid, []) (Typechecker.head t) in
      let arr_t = List.rev arr_t in
      Llvm.function_type ret_t (Array.of_list arr_t)
  | TVar { def = None; _ } -> i32
  | TVar { def = Some t; _ } -> typ_lltyp t
(* | t -> Typechecker.head t |> typ_lltyp *)

let rec gen_expr map = function
  | Var s -> (
      try VMap.find s map
      with _ -> (
        match Llvm.lookup_function s the_module with
        | Some f -> f
        | None -> failwith ("failed to find variable with name: " ^ s)))
  | Const (VInt v) -> Llvm.const_int (Llvm.i32_type ctx) v
  | Const (VString _s) -> failwith "Compiling strings is not implemented :("
  | Apply (s, params) ->
      let f =
        match Llvm.lookup_function s the_module with
        | Some f -> f
        | None -> (
            try VMap.find s map
            with _ -> failwith ("Function " ^ s ^ " was not found!"))
      in
      Llvm.build_call f
        (List.map
           (fun a ->
             let e = gen_expr map a in
             e)
           params
        |> Array.of_list)
        "" builder

let gen map = function
  | { ast = Defun (s, params, exec); typ = t } ->
      (* let fun_t =  *)
      let fun_t = typ_lltyp t in
      let f = Llvm.define_function s fun_t the_module in
      back_to_main s;
      let map_fun : Llvm.llvalue VMap.t =
        Array.fold_left
          (fun (map, i) v ->
            let name = List.nth params i in
            let t = Llvm.param_types fun_t in
            let alloca = create_entry_block_alloca ctx name f t.(i) in
            let _ = Llvm.build_store v alloca builder in
            let alloca = Llvm.build_load alloca "" builder in
            (VMap.add name alloca map, i + 1))
          (map, 0) (Llvm.params f)
        |> fst
      in
      let ret = gen_expr map_fun exec in
      Llvm.build_ret ret builder |> ignore;
      back_to_main "main";
      map
  | { ast = Value expr; typ = _t } ->
      gen_expr map expr |> ignore;
      map
  | { ast = Defar (s, expr); typ = t } ->
      let f = Llvm.block_parent (Llvm.insertion_block builder) in
      let expr = gen_expr map expr in
      let alloca = create_entry_block_alloca ctx s f (typ_lltyp t) in
      Llvm.build_store expr alloca builder |> ignore;
      VMap.add s alloca map

let compile astl =
  let pf_t = Llvm.function_type i32 [| i32; i32 |] in
  let add_f = Llvm.define_function "+" pf_t the_module in
  back_to_main "+";
  let x1 = create_entry_block_alloca ctx "x" add_f i32 in
  let _ = Llvm.build_store (Llvm.param add_f 0) x1 builder in

  let x1 = Llvm.build_load x1 "" builder in
  let y1 = create_entry_block_alloca ctx "y" add_f i32 in
  let _ = Llvm.build_store (Llvm.param add_f 1) y1 builder in
  let y1 = Llvm.build_load y1 "" builder in
  Llvm.build_ret (Llvm.build_add x1 y1 "" builder) builder |> ignore;

  let fun_t = Llvm.function_type i32 [||] in
  let _ = Llvm.define_function "main" fun_t the_module in
  back_to_main "main";
  let map : Llvm.llvalue VMap.t = VMap.empty in

  let _ = List.fold_left gen map astl in
  Llvm.build_ret (gen_expr map (Const (VInt 0))) builder |> ignore;
  Llvm.string_of_llmodule the_module
