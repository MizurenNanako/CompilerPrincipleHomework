{
  open Parser
  open Lexical
  open Lexical.Literal
  exception LexicalError of string

  (* Unlex lasr charator *)
  let lexbuf_vomit (buf : Lexing.lexbuf) =
      buf.lex_curr_pos <- pred buf.lex_curr_pos;
      buf.lex_curr_p <-
          { buf.lex_curr_p with pos_cnum = pred buf.lex_curr_p.pos_cnum }

  let report (buf: Lexing.lexbuf) (msg: string) =
      raise (LexicalError (Printf.sprintf "File \"%s\":%i:%i %s"
              (buf.lex_curr_p.pos_fname)
              (buf.lex_curr_p.pos_lnum)
              (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol + 1)
              msg
            ))

  
  let dump_token out tk =
    match tk with
    | Parser.INT (i64,_) -> Printf.fprintf out "ICONST(%s)" (Int64.to_string i64)
    | FLOAT (f,_) -> Printf.fprintf out "FCONST(%f)" f
    | ID (s,_) -> Printf.fprintf out "ID(%s)" s
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
}

let zero          = '0'
let identifier    = ['A'-'Z' 'a'-'z' '_' '$']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']*
let oct_char      = ['0'-'7']
let hex_char      = ['0'-'9' 'A'-'F' 'a'-'f']
let numbody       = ['0'-'9']
(* let numbody       = ['0'-'9' '\''] *)
(* let literial_dec  = ['+' '-']? ['1'-'9'] numbody* *)
let literial_dec  = ['1'-'9'] numbody*
let literial_oct  = zero ['0'-'9']+
let literial_hex  = zero ['x' 'X'] hex_char*
let literial_bin  = zero ['b' 'B'] ['0' '1']*
(* let wholenumber   = ['+' '-']? ['1'-'9'] numbody* *)
let wholenumber   = ['+' '-']? numbody+
let fraction      = numbody+
let significand   = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent      = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literial_real = (significand exponent? | wholenumber exponent)

rule read = parse
| " " { read lexbuf }
| "\n" { Lexing.new_line lexbuf; read lexbuf }
| "//" [^ '\n']* { read lexbuf }
| "/*" { skip_comment lexbuf }
| (literial_real as s) { FLOAT (float_of_string s, Range.of_lexbuf lexbuf) }
| (literial_dec as s) { INT ((int_of_dec s).data, Range.of_lexbuf lexbuf) }
| (literial_oct as s) { INT ((int_of_oct s).data, Range.of_lexbuf lexbuf) }
| (literial_hex as s) { INT ((int_of_hex s).data, Range.of_lexbuf lexbuf) }
| (literial_bin as s) { INT ((int_of_bin s).data, Range.of_lexbuf lexbuf) }
| zero { INT (0L, Range.of_lexbuf lexbuf) }
| ";" { SEMI }
| "," { COMMA }
| "=" { ASSIGNOP }
| ">" { GT }
| "<" { LT }
| ">=" { GEQ }
| "<=" { LEQ }
| "==" { EEQ }
| "!=" { NEQ }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "/" { DIV }
| "&&" { AND }
| "||" { OR }
| "." { DOT }
| "!" { NOT }
| "int" { KINT }
| "float" { KFLOAT }
| "(" { LP }
| ")" { RP }
| "[" { LB }
| "]" { RB }
| "{" { LC }
| "}" { RC }
| "struct" { STRUCT }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| (identifier as s) { ID (s, Range.of_lexbuf lexbuf) }
| eof { EOF }
| _ as k { report lexbuf (Printf.sprintf "Unexpected: %c" k) }

and skip_comment = parse
| "*/" { read lexbuf }
| "\n" { Lexing.new_line lexbuf; skip_comment lexbuf }
| eof { report lexbuf "Unterminated block comment" }
| _ { skip_comment lexbuf }