{
  open Parser
  open Lexical
  exception SyntaxError of string

  (* Unlex lasr charator *)
  let lexbuf_vomit (buf : Lexing.lexbuf) =
      buf.lex_curr_pos <- pred buf.lex_curr_pos;
      buf.lex_curr_p <-
          { buf.lex_curr_p with pos_cnum = pred buf.lex_curr_p.pos_cnum }
}

let identifier       = ['A'-'Z' 'a'-'z' '_' '$']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']*
let filename         = ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '.' '/']+
let zero             = '0'
let oct_char         = ['0'-'7']
let hex_char         = ['0'-'9' 'A'-'F' 'a'-'f']
let isuffix_         = ['l' 'L'] | ['l' 'L'] ['l' 'L'] | ['z' 'Z']
let isuffix          = ( ['u' 'U']? isuffix_ | isuffix_ ['u' 'U']? | ['u' 'U'])
let numbody          = ['0'-'9' '\'']
let literial_dec     = ['+' '-']? ['1'-'9'] numbody* isuffix?
let literial_oct     = zero ['0'-'9']+ isuffix?
let literial_hex     = zero ['x' 'X'] hex_char* isuffix?
let literial_bin     = zero ['b' 'B'] ['0' '1']* isuffix?
let fsuffix          = ['f' 'F' 'l' 'L']
let wholenumber      = ['+' '-']? ['1'-'9'] numbody*
let fraction         = numbody+
let significand      = (wholenumber "." fraction) | ("." fraction) | (wholenumber ".")
let exponent         = ['e' 'E' 'p' 'P'] ['+' '-']? ['0'-'9']+
let literial_real    = (significand exponent? | wholenumber exponent) fsuffix?
let escape_char_x    = "\\x" hex_char hex_char
let escape_char_ctl  = ['\'' '\\' '\n'] | ("\\" ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\'])
let excape_char_oct  = "\\" oct_char oct_char? oct_char?
let excape_u16char   = "\\u" hex_char hex_char hex_char hex_char
let excape_u32char   = "\\u" hex_char hex_char hex_char hex_char hex_char hex_char hex_char hex_char
let normal_char      = [^ '\n' '\'' '\"' '\\']
let char_            = normal_char | escape_char_x | escape_char_ctl | excape_char_oct | excape_u16char | excape_u32char
(* wtf c allow '"' !? *)
let literial_char    = (['u' 'U' 'L'] | "u8")? "\'" ('\"' | char_) char_? "\'"
(* let literial_string  = "\"" (char_* as x) "\"" *)
let hash             = ("#"|"%:")
let prep_body        = [^ '\r' '\n']*
let whitespace       = [' ' '\t']+
let newline          = "\n"
let linecomment      = "//"
(* let linecomment      = "//" [^ '\r' '\n']* *)
let blockcomment     = "/*"
(* let blockcomment     = "/*" ([^ '*']* "*"+ [^ '*' '/'])* ([^ '*']* "*"+ | [^ '*']* ) "*/" *)

rule read = parse
| linecomment         { singleline_comment lexbuf; read lexbuf }
| blockcomment        { multiline_comment lexbuf; read lexbuf }
(* | linecomment          { Linecomment (Lexing.lexeme lexbuf) } *)
(* | blockcomment         { Blockcomment (Lexing.lexeme lexbuf) } *)
| newline             { Lexing.new_line lexbuf; emptylines lexbuf }
| whitespace          { read lexbuf }
| "\\\n"              { Lexing.new_line lexbuf; SYM_CONCAT }
| "\\\n" | whitespace { Lexing.new_line lexbuf; read lexbuf }
| zero                { Zero }
| literial_dec        { Literal_int (int_of_dec (Lexing.lexeme lexbuf)) }
| literial_oct        { Literal_int (int_of_oct (Lexing.lexeme lexbuf)) }
| literial_hex        { Literal_int (int_of_hex (Lexing.lexeme lexbuf)) }
| literial_bin        { Literal_int (int_of_bin (Lexing.lexeme lexbuf)) }
| literial_real       { Literal_real (float_of_lit (Lexing.lexeme lexbuf)) }
| literial_char       { Literal_char (char_of_lit (Lexing.lexeme lexbuf)) }
| '\"'                { literial_string (Buffer.create 20) lexbuf }
| eof | "\000"        { Eof }
| "("                 { LPAREN }
| ")"                 { RPAREN }
| "[" | "<:"          { LBRACK }
| "]" | ":>"          { RBRACK }
| "{" | "<%"          { LBRACE }
| "}" | "%>"          { RBRACE }
| "auto"              { Kw_auto }
| "break"             { Kw_break }
| "case"              { Kw_case }
| "char"              { Kw_char }
| "const"             { Kw_const }
| "continue"          { Kw_continue }
| "default"           { Kw_default }
| "do"                { Kw_do }
| "double"            { Kw_double }
| "else"              { Kw_else }
| "enum"              { Kw_enum }
| "extern"            { Kw_extern }
| "float"             { Kw_float }
| "for"               { Kw_for }
| "goto"              { Kw_goto }
| "if"                { Kw_if }
| "int"               { Kw_int }
| "long"              { Kw_long }
| "register"          { Kw_register }
| "return"            { Kw_return }
| "short"             { Kw_short }
| "signed"            { Kw_signed }
| "sizeof"            { Kw_sizeof }
| "static"            { Kw_static }
| "struct"            { Kw_struct }
| "switch"            { Kw_switch }
| "typedef"           { Kw_typedef }
| "union"             { Kw_union }
| "unsigned"          { Kw_unsigned }
| "void"              { Kw_void }
| "volatile"          { Kw_volatile }
| "while"             { Kw_while }
| "inline"            { Kw_inline }
| "restrict"          { Kw_restrict }
| "_Bool"             { Kw__Bool }
| "_Complex"          { Kw__Complex }
| "_Imaginary"        { Kw__Imaginary }
| "_Alignas"          { Kw__Alignas }
| "_Alignof"          { Kw__Alignof }
| "_Atomic"           { Kw__Atomic }
| "_Generic"          { Kw__Generic }
| "_Noreturn"         { Kw__Noreturn }
| "_Static_assert"    { Kw__Static_assert }
| "_Thread_local"     { Kw__Thread_local }
| "="                 { Op_eq }
| "+="                { Op_addeq }
| "-="                { Op_subeq }
| "*="                { Op_muleq }
| "/="                { Op_diveq }
| "%="                { Op_modeq }
| "<<="               { Op_shleq }
| ">>="               { Op_shreq }
| "&="                { Op_bitandeq }
| "|="                { Op_bitoreq }
| "^="                { Op_bitxoreq }
| "++"                { Op_inc }
| "--"                { Op_dec }
| "~"                 { Op_compl }
| "+"                 { Op_add }
| "-"                 { Op_sub }
| "*"                 { Op_mul }
| "/"                 { Op_div }
| "%"                 { Op_mod }
| "<<"                { Op_shl }
| ">>"                { Op_shr }
| "&"                 { Op_bitand }
| "|"                 { Op_bitor }
| "^"                 { Op_bitxor }
| "!"                 { Op_not }
| "&&"                { Op_and }
| "||"                { Op_or }
| "=="                { Op_eeq }
| "!="                { Op_neq }
| "<"                 { Op_lt }
| ">"                 { Op_gt }
| "<="                { Op_leq }
| ">="                { Op_geq }
| "."                 { Op_dot }
| "->"                { Op_to }
| ","                 { Op_comma }
| "?"                 { Op_question }
| ":"                 { Op_colon }
| ";"                 { Op_semicolon }
| "..."               { Op_ellipsis }
| identifier          { Identifier (Lexing.lexeme lexbuf) }
| _                   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and literial_string buf = parse
| '\"'      { Literal_string (Buffer.contents buf) }
| '\\' '\\' { Buffer.add_char buf '\\'; literial_string buf lexbuf }
| '\\' '\"' { Buffer.add_char buf '\"'; literial_string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; literial_string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; literial_string buf lexbuf }
| '\\' 'r'  { Buffer.add_char buf '\r'; literial_string buf lexbuf }
| '\\' 'b'  { Buffer.add_char buf '\b'; literial_string buf lexbuf }
| '\\' '\n'         
            { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; literial_string buf lexbuf }
| '\\'      { Buffer.add_char buf '\\'; literial_string buf lexbuf }
| [^ '\n' '\r' '\"' '\\']+ as s
            { Buffer.add_string buf s; literial_string buf lexbuf }
| _ { raise (SyntaxError ("Invaild string literal: " ^ Lexing.lexeme lexbuf)) }

(* Multiply empty lines to one token *)
and emptylines = parse
| newline          { Lexing.new_line lexbuf; emptylines lexbuf }
| linecomment      { singleline_comment lexbuf; emptylines lexbuf }
| blockcomment     { multiline_comment lexbuf; emptylines lexbuf }
| eof | "\000"     { Lexing.new_line lexbuf; Eof }
| _                {
                        lexbuf_vomit lexbuf;
                        Newline
                   }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"          { () }
  | eof | '\000'  { raise (SyntaxError "unterminated comment") }
  | '\n'          { Lexing.new_line lexbuf; multiline_comment lexbuf }
  | _             { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'          { Lexing.new_line lexbuf }
  | "\\\n"        { Lexing.new_line lexbuf; singleline_comment lexbuf }
  | eof | '\000'  { () }
  | _             { singleline_comment lexbuf }
