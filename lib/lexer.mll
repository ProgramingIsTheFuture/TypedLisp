{
  open Parser
  open Ast
}

rule token = parse
| [' ' '\n' '\t'] { token lexbuf }
| '(' {LPARENT}
| ')' {RPARENT}
| '['{LSBRAK}
| ']'{RSBRAK}
| ['a'-'z' 'A'-'Z' '+' '+']+ as n {NAME n}
| ['0'-'9']+ as i {VALUE (VInt (int_of_string i))}
| '"'[^ '"']+'"' as s {VALUE (VString s)}
| eof { EOF }
| _ as c {failwith (Format.sprintf "Failed to lex character: %c" c)}
