{
  open Lexical.LexicalToken
  open Lexical.LexicalLiteral
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
| (literial_real as s) { FLOAT (float_of_string s) }
| (literial_dec as s) { INT ((int_of_dec s).data) }
| (literial_oct as s) { INT ((int_of_oct s).data) }
| (literial_hex as s) { INT ((int_of_hex s).data) }
| (literial_bin as s) { INT ((int_of_bin s).data) }
| zero { INT 0L }
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
| (identifier as s) { ID s }
| eof { EOF }
| _ as k { report lexbuf (Printf.sprintf "Unexpected: %c" k) }

and skip_comment = parse
| "*/" { read lexbuf }
| "\n" { Lexing.new_line lexbuf; skip_comment lexbuf }
| eof { report lexbuf "Unterminated block comment" }
| _ { skip_comment lexbuf }