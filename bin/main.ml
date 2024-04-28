open Typing
open Dotutils

let report (buf : Lexing.lexbuf) (msg : string) =
  Printf.sprintf "File \"%s\":%i:%i %s" buf.lex_curr_p.pos_fname
    buf.lex_curr_p.pos_lnum
    (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol)
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

let run_parser_graph () =
  let fn = Sys.argv.(1) in
  let infl = In_channel.open_text fn in
  let lexbuf = Lexing.from_channel infl in
  Lexing.set_filename lexbuf fn;
  try
    let ast = Parser.program Lexer.read lexbuf in
    let graph = DotGraph.of_program ast in
    Printf.fprintf stdout "%a\n" DotGraph.dump graph
    (* Printf.fprintf stdout "%a\n" AST.dump ast *)
  with _ ->
    Printf.fprintf stdout "%s\n" (report lexbuf "SyntaxError");
    exit 1
(* raise e *)
;;

ignore run_lexer;
ignore run_parser;
run_parser_graph ()
