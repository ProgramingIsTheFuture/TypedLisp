type typ = TInt | TString | TVoid | TSeq of typ * typ | TVar of tvar
and tvar = { id : int; mutable def : typ option }

type ast = { ast : Ast.ast; typ : typ }
