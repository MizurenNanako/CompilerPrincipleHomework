module A = Typing.AST
module G = Dotutils.DotGraph
module L = Lexer
module P = Parser
module U = UnitActionsParser
module I = U.MenhirInterpreter
module LU = MenhirLib.LexerUtil
module ER = MenhirLib.ErrorReports

let run_lexer_only (filename : string) =
  let ch = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel ch in
  let ll () = L.read lexbuf in
  let rec _loop () =
    let open Printf in
    match ll () with
    | EOF -> printf "done.\n"
    | k ->
        printf "%a\n" L.dump_token k;
        _loop ()
  in
  Lexing.set_filename lexbuf filename;
  try _loop ()
  with L.LexicalError msg ->
    flush stdout;
    Printf.printf "LexicalError: %s\n" msg

let run_ast_attempt (filename : string) =
  let text, lexbuf = LU.read filename in
  try
    let ast = P.program L.read lexbuf in
    Ok ast
  with
  | L.LexicalError msg ->
      flush stdout;
      Printf.printf "LexicalError: %s\n" msg;
      exit (-1)
  | P.Error -> Error text

let run_checker (filename : string) (text : string) =
  let lexbuf = LU.init filename (Lexing.from_string text) in
  let error_buffer, supplier =
    ER.wrap_supplier (I.lexer_lexbuf_to_supplier L.read lexbuf)
  in

  (* Now set the start state and start to run *)
  let start_state = U.Incremental.program lexbuf.lex_curr_p in

  let rec loop (state : unit I.checkpoint) =
    match state with
    | InputNeeded _ ->
        let state' = I.offer state (supplier ()) in
        loop state'
    | Accepted _ ->
        (* We know this won't happen *)
        assert false
    | Rejected ->
        (* We know this will happen *)
        ()
    | Shifting (_, _, _) ->
        let state' = I.resume state in
        loop state'
    | AboutToReduce (_, _) ->
        let state' = I.resume state in
        loop state'
    | HandlingError e ->
        (* We will report the error *)
        let location = LU.range (ER.last error_buffer) in
        let error_info posi =
          ER.extract text posi |> ER.sanitize |> ER.compress
          |> ER.shorten 20 (* max width 43 *)
        in
        let indication = ER.show error_info error_buffer in
        let util_get i =
          match I.get i e with
          | Some (I.Element (_, _, pos1, pos2)) -> error_info (pos1, pos2)
          | None -> "???"
        in
        let message =
          ER.expand util_get
            (Parser_messages.message (I.current_state_number e))
        in
        Printf.fprintf stdout "%sSyntax error %s.\n%s%!" location indication
          message;

        (* Try to discard stack elements until at last reduce/shift  *)
        (* let rec discard_loop (env : unit I.env) =
             let last_env_opt = I.pop env in
             match last_env_opt with
             | None ->
                 (* already at bottom, so stop *)
                 env
             | Some last_env -> (
                 (* test if this is right *)
                 match I.env_has_default_reduction last_env with
                 | true ->
                     (* fixed *)
                     last_env
                 | false ->
                     (* keep tracing back *)
                     discard_loop last_env)
           in
           (* we shall continue *)
           let fixed_env = discard_loop e in
           let fixed_state = I.input_needed fixed_env in *)
        (* let state' = I.resume fixed_state in *)
        (* loop fixed_state *)
        loop (I.resume state)
  in
  loop start_state

let run_parser_mexpr (filename : string) =
  let ans = run_ast_attempt filename in
  match ans with
  | Ok ast -> Printf.printf "%a\n" A.dump ast
  | Error text -> run_checker filename text

let run_parser_graph (filename : string) =
  let ans = run_ast_attempt filename in
  match ans with
  | Ok ast ->
      let g = G.of_program ast in
      Printf.printf "%a\n" G.dump g
  | Error text -> run_checker filename text

let () =
  let is_lexer_only = ref false in
  let is_parser_mexpr = ref false in
  let is_parser_graph = ref true in
  let exec_list = ref [] in
  let opts =
    [
      ("--lexer", Arg.Set is_lexer_only, "Set lexer only mode");
      ("--mexpr", Arg.Set is_parser_mexpr, "Set parser output mode to mexpr");
      ( "--graph",
        Arg.Set is_parser_graph,
        "Set parser output mode to dot file, default." );
    ]
  in
  let anon s = exec_list := s :: !exec_list in
  let usage = Printf.sprintf "%s [options] <filename>" Sys.argv.(0) in
  Arg.parse opts anon usage;
  let act =
    if !is_lexer_only then run_lexer_only
    else if !is_parser_mexpr then run_parser_mexpr
    else run_parser_graph
  in
  List.iter act !exec_list
