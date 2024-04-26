// Meta
%token Err
%token Zero

// With Info
%token <string>                          Identifier
%token <Lexical.integer>                 Literal_int
%token <Lexical.floating>                Literal_real
%token <int64>                           Literal_char
%token <string>                          Literal_string
%token <string>                          Linecomment
%token <string>                          Blockcomment

// Parenthesis
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE

// Keywords
// (C98)
%token Kw_auto
%token Kw_break
%token Kw_case
%token Kw_char
%token Kw_const
%token Kw_continue
%token Kw_default
%token Kw_do
%token Kw_double
%token Kw_else
%token Kw_enum
%token Kw_extern
%token Kw_float
%token Kw_for
%token Kw_goto
%token Kw_if
%token Kw_int
%token Kw_long
%token Kw_register
%token Kw_return
%token Kw_short
%token Kw_signed
%token Kw_sizeof
%token Kw_static
%token Kw_struct
%token Kw_switch
%token Kw_typedef
%token Kw_union
%token Kw_unsigned
%token Kw_void
%token Kw_volatile
%token Kw_while
// (C99)
%token Kw_inline
%token Kw_restrict
%token Kw__Bool
%token Kw__Complex
%token Kw__Imaginary
// (C11)
%token Kw__Alignas
%token Kw__Alignof
%token Kw__Atomic
%token Kw__Generic
%token Kw__Noreturn
%token Kw__Static_assert
%token Kw__Thread_local

// Operators
// assignment
%token Op_eq         // =
%token Op_addeq      // +=
%token Op_subeq      // -=
%token Op_muleq      // *=
%token Op_diveq      // /=
%token Op_modeq      // %=
%token Op_shleq      // <<=
%token Op_shreq      // >>=
%token Op_bitandeq   // &=
%token Op_bitoreq    // |=
%token Op_bitxoreq   // ^=
// inc/dec
%token Op_inc        // ++
%token Op_dec        // --
// arithmatic
%token Op_compl      // ~
%token Op_add        // +
%token Op_sub        // -
%token Op_mul        // *
%token Op_div        // /
%token Op_mod        // %
%token Op_shl        // <<
%token Op_shr        // >>
%token Op_bitand     // &
%token Op_bitor      // |
%token Op_bitxor     // ^
// logical
%token Op_not        // !
%token Op_and        // &&
%token Op_or         // ||
// comparison
%token Op_eeq        // ==
%token Op_neq        // !=
%token Op_lt         // <
%token Op_gt         // >
%token Op_leq        // <=
%token Op_geq        // >=
// member
%token Op_dot        // .
%token Op_to         // ->
// other
%token Op_comma      // ,
%token Op_question   // ?
%token Op_colon      // :
%token Op_semicolon  // ;
%token Newline
%token Eof

%start <Typing.AST.t>  translation_unit
%%

translation_unit:
    | external_declaration translation_unit
    | external_declaration Eof
    {}

external_declaration:
    | Op_comma
    {}
;