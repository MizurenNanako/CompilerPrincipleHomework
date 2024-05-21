(** The true lexer we are gonna use, 
    this is a pure functional lexer, with a discard stack *)

module Lexer = struct
  type token = Lexical.LexicalToken.token
  type pos = Lexical.LexicalPosition.t
  type range = Lexical.LexicalRange.t

  type state = {
    filename : string;
    lexing_at : pos;
    looking_at : token option;
    lexbuf : Lexing.lexbuf;
  }

  let init filename =
    let file = In_channel.open_text filename in
    let lexbuf = Lexing.from_channel file in
    Lexing.set_filename lexbuf filename;
    {
      filename;
      lexing_at = Lexical.LexicalPosition.from_lexbuf lexbuf;
      looking_at = None;
      lexbuf;
    }

  let init_stdin () =
    let lexbuf = Lexing.from_channel stdin in
    Lexing.set_filename lexbuf "[stdin]";
    {
      filename = "[stdin]";
      lexing_at = Lexical.LexicalPosition.from_lexbuf lexbuf;
      looking_at = None;
      lexbuf;
    }

  let peek (state : state) = Option.get state.looking_at

  let next (state : state) =
    {
      state with
      looking_at = Some (Raw_lexer.read state.lexbuf);
      lexing_at = Lexical.LexicalPosition.from_lexbuf state.lexbuf;
    }

  let get (state : state) =
    let tk = peek state in
    let st = next state in
    (tk, st)
end
