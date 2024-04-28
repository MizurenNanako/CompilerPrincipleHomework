(* open Typing *)

let dump_token out tk =
  match tk with
  | Parser.INT i64 -> Printf.fprintf out "ICONST(%s)" (Int64.to_string i64)
  | FLOAT f -> Printf.fprintf out "FCONST(%f)" f
  | ID s -> Printf.fprintf out "ID(%s)" s
  | SEMI -> Printf.fprintf out "SEMI"
  | COMMA -> Printf.fprintf out "COMMA"
  | ASSIGNOP -> Printf.fprintf out "ASSIGNOP"
  | GT -> Printf.fprintf out "GT"
  | LT -> Printf.fprintf out "LT"
  | GEQ -> Printf.fprintf out "GEQ"
  | LEQ -> Printf.fprintf out "LEQ"
  | EEQ -> Printf.fprintf out "EEQ"
  | NEQ -> Printf.fprintf out "NEQ"
  | PLUS -> Printf.fprintf out "PLUS"
  | MINUS -> Printf.fprintf out "MINUS"
  | STAR -> Printf.fprintf out "STAR"
  | DIV -> Printf.fprintf out "DIV"
  | AND -> Printf.fprintf out "AND"
  | OR -> Printf.fprintf out "OR"
  | DOT -> Printf.fprintf out "DOT"
  | NOT -> Printf.fprintf out "NOT"
  | KINT -> Printf.fprintf out "INT"
  | KFLOAT -> Printf.fprintf out "FLOAT"
  | LP -> Printf.fprintf out "LP"
  | RP -> Printf.fprintf out "RP"
  | LB -> Printf.fprintf out "LB"
  | RB -> Printf.fprintf out "RB"
  | LC -> Printf.fprintf out "LC"
  | RC -> Printf.fprintf out "RC"
  | STRUCT -> Printf.fprintf out "STRUCT"
  | RETURN -> Printf.fprintf out "RETURN"
  | IF -> Printf.fprintf out "IF"
  | ELSE -> Printf.fprintf out "ELSE"
  | WHILE -> Printf.fprintf out "WHILE"
  | EOF -> Printf.fprintf out "EOF"

let () =
  let fn = Sys.argv.(1) in
  let infl = In_channel.open_text fn in
  let lexbuf = Lexing.from_channel infl in
  let rec loop () =
    let tk = Lexer.read lexbuf in
    match tk with
    | EOF -> ()
    | _ as k ->
        Printf.fprintf stdout "%a\tLine: %i\n" dump_token k
          lexbuf.lex_curr_p.pos_lnum;
        loop ()
  in
  try loop () with Lexer.LexicalError msg -> Printf.fprintf stdout "%s\n" msg
