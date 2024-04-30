(* open Typing *)
open Dotutils
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* let report (buf : Lexing.lexbuf) (msg : string) =
  Printf.sprintf "File \"%s\":%i:%i %s" buf.lex_curr_p.pos_fname
    buf.lex_curr_p.pos_lnum
    (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol)
    msg *)

(*
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
   ;; *)

let parse_from filename =
  let text, lexbuf = L.read filename in
  match Parser.program Lexer.read lexbuf with
  | v -> Ok v
  | exception Lexer.LexicalError msg ->
      Printf.fprintf stdout "LexicalError: %s\n" msg;
      Error ""
  | exception Parser.Error -> Error text

module I = UnitActionsParser.MenhirInterpreter

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> "???"

let succeed _v = assert false

let fail text buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let indication =
    Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  let message = Parser_messages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  Printf.fprintf stdout "%s%s%s%!" location indication message;
  exit 1

let second_wind filename text =
  let lexbuf = L.init filename (Lexing.from_string text) in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let () =
  let filename = Sys.argv.(1) in
  let res = parse_from filename in
  match res with
  | Ok ast ->
      let graph = DotGraph.of_program ast in
      Printf.fprintf stdout "%a\n" DotGraph.dump graph
  | Error text -> second_wind filename text
