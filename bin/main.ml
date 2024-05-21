(* open Typing *)
open Dotutils
open UnitActionsParser
open MenhirLib

let attempt_parse filename =
  let text, lexbuf = LexerUtil.read filename in
  match Parser.program Lexer.read lexbuf with
  | v -> Ok v
  | exception Lexer.LexicalError msg ->
      Printf.fprintf stdout "LexicalError: %s\n" msg;
      Error ""
  | exception Parser.Error -> Error text

let second_wind filename text =
  let util_env checkpoint =
    match checkpoint with
    | MenhirInterpreter.HandlingError env -> env
    | _ -> assert false
  in
  let util_state checkpoint : int =
    match MenhirInterpreter.top (util_env checkpoint) with
    | Some (MenhirInterpreter.Element (s, _, _, _)) ->
        MenhirInterpreter.number s
    | None -> 0
  in
  let show text positions =
    ErrorReports.extract text positions
    |> ErrorReports.sanitize |> ErrorReports.compress
    |> ErrorReports.shorten 20 (* max width 43 *)
  in
  let util_get text checkpoint i =
    match MenhirInterpreter.get i (util_env checkpoint) with
    | Some (MenhirInterpreter.Element (_, _, pos1, pos2)) ->
        show text (pos1, pos2)
    | None -> "???"
  in
  let succeed_handler _v = assert false in
  let fail text buffer (checkpoint : _ MenhirInterpreter.checkpoint) =
    let location = LexerUtil.range (ErrorReports.last buffer) in
    let indication =
      Printf.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer)
    in
    let message = Parser_messages.message (util_state checkpoint) in
    let message = ErrorReports.expand (util_get text checkpoint) message in
    Printf.fprintf stdout "%s%s%s%!" location indication message;
    exit 1
  in
  let lexbuf = LexerUtil.init filename (Lexing.from_string text) in
  let supplier = MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supply_hendler = ErrorReports.wrap_supplier supplier in
  let checkpoint_hendler =
    UnitActionsParser.Incremental.program lexbuf.lex_curr_p
  in
  MenhirInterpreter.loop_handle succeed_handler (fail text buffer)
    supply_hendler checkpoint_hendler

let run_lexer_only filename =
  let infl = In_channel.open_text filename in
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

let run_parser_mexpr filename =
  let res = attempt_parse filename in
  match res with
  | Ok ast -> Printf.fprintf stdout "%a\n" Syntatical_util.Repr.dump ast
  | Error text -> second_wind filename text

let run_parser_graph filename =
  let res = attempt_parse filename in
  match res with
  | Ok ast ->
      let graph = Syntatical_util.ToGraph.of_program ast in
      Printf.fprintf stdout "%a\n" DotGraph.dump graph
  | Error text -> second_wind filename text

let () =
  let is_lexer_only = ref false in
  let is_parser_mexpr = ref false in
  let is_parser_graph = ref true in
  let opts =
    [
      ("--lexer", Arg.Set is_lexer_only, "Set lexer only mode");
      ("--mexpr", Arg.Set is_parser_mexpr, "Set parser output mode to mexpr");
      ( "--graph",
        Arg.Set is_parser_graph,
        "Set parser output mode to dot file, default." );
    ]
  in
  let anon s =
    if !is_lexer_only then run_lexer_only s
    else if !is_parser_mexpr then run_parser_mexpr s
    else run_parser_graph s
  in
  let usage = Printf.sprintf "%s [options] <filename>" Sys.argv.(0) in
  Arg.parse opts anon usage
