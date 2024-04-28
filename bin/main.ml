open Typing

let report (buf : Lexing.lexbuf) (msg : string) =
  Printf.sprintf "File \"%s\":%i:%i %s" buf.lex_curr_p.pos_fname
    buf.lex_curr_p.pos_lnum
    (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol + 1)
    msg

let run_lexer () =
  let fn = Sys.argv.(1) in
  let infl = In_channel.open_text fn in
  let lexbuf = Lexing.from_channel infl in
  let rec loop () =
    let tk = Lexer.read lexbuf in
    match tk with
    | EOF -> ()
    | _ as k ->
        Printf.fprintf stdout "%a\tLine: %i\n" Lexer.dump_token k
          lexbuf.lex_curr_p.pos_lnum;
        loop ()
  in
  try loop ()
  with Lexer.LexicalError msg ->
    Printf.fprintf stdout "LexicalError: %s\n" msg

let run_parser () =
  let fn = Sys.argv.(1) in
  let infl = In_channel.open_text fn in
  let lexbuf = Lexing.from_channel infl in
  Lexing.set_filename lexbuf fn;
  try
    let ast = Parser.program Lexer.read lexbuf in
    Printf.fprintf stdout "%a\n" AST.dump ast
  with _ -> Printf.fprintf stdout "%s\n" (report lexbuf "SyntaxError")
(* raise e *)
;;

ignore run_lexer;
run_parser ()
