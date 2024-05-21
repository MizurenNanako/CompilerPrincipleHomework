(** a better lexical position module for lexer *)
module LexicalPosition = struct
  type t = { lineno : int; col : int }

  let from_lexbuf (lexbuf : Lexing.lexbuf) =
    let lcp = lexbuf.lex_curr_p in
    { lineno = lcp.pos_lnum; col = lcp.pos_cnum - lcp.pos_bol + 1 }

  let dump out pos =
    Printf.fprintf out "line %i col %i" pos.lineno pos.col
end

module LexicalRange = struct
  type pos_t = LexicalPosition.t
  type t = pos_t * pos_t

  (** Trivally forms the pair. *)
  let form (a : pos_t) (b : pos_t) : t = (a, b)

  (** [join a b] for [a=(a1, a2)] [b=(b1, b2)], returns [(a1, b2)] *)
  let join (a : t) (b : t) : t = (fst a, snd b)

  (** [from_lexbuf lexbuf] gets lexical range from lexbuf by [lex_start_p] and [lex_curr_p]. *)
  let from_lexbuf (lexbuf : Lexing.lexbuf) =
    let lsp, lcp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    ( {
        LexicalPosition.lineno = lsp.pos_lnum;
        col = lsp.pos_cnum - lsp.pos_bol + 1;
      },
      {
        LexicalPosition.lineno = lcp.pos_lnum;
        col = lcp.pos_cnum - lcp.pos_bol + 1;
      } )

  let dump out rng =
    let a, b = rng in
    Printf.fprintf out "from %a to %a" LexicalPosition.dump a
      LexicalPosition.dump b
end

(** The type of tokens. *)
module LexicalToken = struct
  type token =
    | WHILE
    | STRUCT
    | STAR
    | SEMI
    | RP
    | RETURN
    | RC
    | RB
    | PLUS
    | OR
    | NOT
    | NEQ
    | MINUS
    | LT
    | LP
    | LEQ
    | LC
    | LB
    | KINT
    | KFLOAT
    | INT of int64
    | IF
    | ID of string
    | GT
    | GEQ
    | FLOAT of float
    | EOF
    | ELSE
    | EEQ
    | DOT
    | DIV
    | COMMA
    | ASSIGNOP
    | AND

  let dump out tk =
    match tk with
    | INT i64 -> Printf.fprintf out "ICONST(%s)" (Int64.to_string i64)
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
end

module LexicalLiteral = struct
  exception BadLiteral of string

  type integer = {
    data : int64;
    sign : integer_sign_type;
    size : integer_size_type;
  }

  and integer_sign_type = Signed | Unsigned
  and integer_size_type = Normal | Long | LongLong

  type floating = { data : float; size : float_size_type }
  and float_size_type = Float | Double | LongDouble

  let dumb_int = { data = 0L; sign = Signed; size = Normal }
  let dumb_float = { data = 0.; size = Double }

  let _int_folder_maker base (acc : integer) ch =
    let ord ch =
      match ch with
      | '0' -> 0L
      | '1' -> 1L
      | '2' -> 2L
      | '3' -> 3L
      | '4' -> 4L
      | '5' -> 5L
      | '6' -> 6L
      | '7' -> 7L
      | '8' -> 8L
      | '9' -> 9L
      | 'a' | 'A' -> 10L
      | 'b' | 'B' -> 11L
      | 'c' | 'C' -> 12L
      | 'd' | 'D' -> 13L
      | 'e' | 'E' -> 14L
      | 'f' | 'F' -> 15L
      | _ ->
          raise
            (BadLiteral (Printf.sprintf "Not number charactor: %c" ch))
    in
    let adv b =
      match base with
      | 2L -> Int64.shift_left b 1
      | 4L -> Int64.shift_left b 2
      | 8L -> Int64.shift_left b 3
      | 16L -> Int64.shift_left b 4
      | _ -> Int64.mul b base
    in
    match ch with
    | 'l' | 'L' -> (
        match acc.size with
        | Normal -> { acc with size = Long }
        | Long -> { acc with size = LongLong }
        | LongLong -> raise (BadLiteral "Too much suffix"))
    | 'u' | 'U' | 'x' | 'X' | 'b' | 'B' ->
        { acc with sign = Unsigned }
    | '\'' | '+' -> acc
    | '-' -> { acc with data = Int64.neg acc.data }
    | _ as nch ->
        { acc with data = Int64.add (adv acc.data) (ord nch) }

  let int_of_dec (data : string) =
    let folder = _int_folder_maker 10L in
    String.fold_left folder dumb_int data

  (* Oct starts with 0 *)
  let int_of_oct (data : string) =
    let folder = _int_folder_maker 8L in
    String.fold_left folder dumb_int data

  (* Hex starts with 0x or 0X, first 2 char is useless *)
  let int_of_hex (data : string) =
    let folder = _int_folder_maker 16L in
    String.fold_left folder dumb_int data

  (* Bin starts with 0b or 0B, first 2 char is useless *)
  let int_of_bin (data : string) =
    let folder = _int_folder_maker 2L in
    String.fold_left folder dumb_int data

  let float_of_lit (data : string) =
    let ldata = String.length data - 1 in
    let _data, _size =
      match String.unsafe_get data ldata with
      | 'f' | 'F' -> (String.sub data 0 ldata, Float)
      | 'l' | 'L' -> (String.sub data 0 ldata, LongDouble)
      | _ -> (data, Double)
    in
    { data = float_of_string _data; size = _size }

  (* This works surprising well. *)
  let char_of_lit (data : string) =
    Int64.of_int (int_of_char data.[1])

  (* useless *)
  (* let string_of_lit (data : string) = String.sub data 1 (String.length data - 2) *)

  (* Fuck ocaml standrad lib *)
  let uint64_to_string (d : int64) =
    let buf = Buffer.create 20 in
    let rec cnv (d : int64) =
      match d with
      | (0L | 1L | 2L | 3L | 4L | 5L | 6L | 7L | 8L | 9L) as t ->
          Buffer.add_char buf
            (char_of_int (Int64.to_int t + int_of_char '0'))
      | _ ->
          let dr = Int64.unsigned_rem d 10L in
          let dd = Int64.unsigned_div (Int64.sub d dr) 10L in
          cnv dd;
          cnv dr
    in
    cnv d;
    String.of_bytes @@ Buffer.to_bytes buf
end
