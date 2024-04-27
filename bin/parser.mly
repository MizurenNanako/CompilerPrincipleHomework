%{
    open Typing
%}

// Meta
%token Err

// With Info
%token <string>                          Identifier
%token <Lexical.integer>                 Literal_int
%token <Lexical.floating>                Literal_real
%token <int64>                           Literal_char
%token <string>                          Literal_string
// %token <string>                          Linecomment
// %token <string>                          Blockcomment

// Parenthesis
%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token LBRACE "{"
%token RBRACE "}"

// Keywords
// (C98)
// %token Kw_auto "auto"
%token Kw_break "break"
%token Kw_case "case"
%token Kw_char "char"
%token Kw_const "const"
%token Kw_continue "continue"
%token Kw_default "default"
%token Kw_do "do"
%token Kw_double "double"
%token Kw_else "else"
%token Kw_enum "enum"
// %token Kw_extern "extern"
%token Kw_float "float"
%token Kw_for "for"
%token Kw_goto "goto"
%token Kw_if "if"
%token Kw_int "int"
%token Kw_long "long"
// %token Kw_register "register"
%token Kw_return "return"
%token Kw_short "short"
%token Kw_signed "signed"
%token Kw_sizeof "sizeof"
// %token Kw_static "static"
%token Kw_struct "struct"
%token Kw_switch "switch"
// %token Kw_typedef "typedef"
%token Kw_union "union"
%token Kw_unsigned "unsigned"
%token Kw_void "void"
%token Kw_volatile "volatile"
%token Kw_while "while"

// Operators
// assignment
%token Op_eq         "="
%token Op_addeq      "+="
%token Op_subeq      "-="
%token Op_muleq      "*="
%token Op_diveq      "/="
%token Op_modeq      "%="
%token Op_shleq      "<<="
%token Op_shreq      ">>="
%token Op_bitandeq   "&="
%token Op_bitoreq    "|="
%token Op_bitxoreq   "^="
// inc/dec
%token Op_inc        "++"
%token Op_dec        "--"
// arithmatic
%token Op_compl      "~"
%token Op_add        "+"
%token Op_sub        "-"
%token Op_mul        "*"
%token Op_div        "/"
%token Op_mod        "%"
%token Op_shl        "<<"
%token Op_shr        ">>"
%token Op_bitand     "&"
%token Op_bitor      "|"
%token Op_bitxor     "^"
// logical
%token Op_not        "!"
%token Op_and        "&&"
%token Op_or         "||"
// comparison
%token Op_eeq        "=="
%token Op_neq        "!="
%token Op_lt         "<"
%token Op_gt         ">"
%token Op_leq        "<="
%token Op_geq        ">="
// member
%token Op_dot        "."
%token Op_to         "->"
// other
%token Op_comma      ","
%token Op_question   "?"
%token Op_colon      ":"
%token Op_semicolon  ";"
%token Op_ellipsis   "..."
// %token Newline
%token Eof

%start <AST.t>  translation_unit
%%

translation_unit :
| l = external_declaration*; Eof { l }

external_declaration :
| f = function_definition { f }
| d = declaration { d }

(* todo *)
function_definition :
| ds = declaration_specifier*; declarator declaration* sl = compound_statement 
{
    // let ret_type = CType.deduce_spec ds in

}

declaration_specifier :
// | storage_class_specifier {}
| a = type_specifier { (false, a) }
| a = type_qualifier { (a, CType.Undetermined) }

// storage_class_specifier :
// | "auto" {}
// | "register" {}
// | "static" {}
// | "extern" {}
// | "typedef" {}

type_specifier :
| "void" { CType.CVoid }
| "char" { CType.CInt (false, 1) }
| "short" { CType.CInt (false, 2) }
| "int" { CType.CInt (false, 4) }
| "long" { CType.CInt (false, 4) }
| "float" { CType.CReal 4 }
| "double" { CType.CReal 8 }
| "signed" { CType.CInt (false, 4) }
| "unsigned" { CType.CInt (true, 4) }
| a = struct_or_union_specifier { a }
| a = enum_specifier { a }
// | typedef_name {}

(* todo *)
struct_or_union_specifier :
| struct_or_union identifier "{" struct_declaration+ "}" {}
| struct_or_union "{" struct_declaration+ "}" {}
| struct_or_union identifier {}

identifier :
| s = Identifier; { (s, (false, Undetermined)) }

(* todo *)
struct_or_union :
| "struct" {}
| "union" {}

(* todo *)
struct_declaration :
| specifier_qualifier* struct_declarator_list {}

(* todo *)
specifier_qualifier :
| type_specifier {}
| type_qualifier {}

(* todo *)
struct_declarator_list :
| struct_declarator {}
| struct_declarator_list "," struct_declarator {}

(* todo *)
struct_declarator :
| declarator {}
| declarator ":" constant_expression {}
| ":" constant_expression {}

(* todo *)
declarator :
| pointer? direct_declarator {}

(* todo *)
pointer :
| "*" type_qualifier* pointer? {}

type_qualifier :
| "const" { true }
| "volatile" { false }

(* todo *)
direct_declarator :
| identifier {}
| "(" declarator ")" {}
| direct_declarator "[" constant_expression? "]" {}
| direct_declarator "(" parameter_type_list ")" {}
| direct_declarator "(" identifier* ")" {}

constant_expression :
| c = conditional_expression { c }

conditional_expression :
| a = logical_or_expression { a }
| a = logical_or_expression; "?"; b = expression; ":"; c = conditional_expression
{ (AST.IfElseExpr(a, b, c)) }

logical_or_expression :
| a = logical_and_expression { a }
| a = logical_or_expression; "||"; b = logical_and_expression
{ (AST.BinaryOpExpr(OpOr, a, b), CType.Undetermined) }

logical_and_expression :
| i = inclusive_or_expression { i }
| a = logical_and_expression; "&&"; b = inclusive_or_expression
{ (AST.BinaryOpExpr(OpAnd, a, b), CType.Undetermined) }

inclusive_or_expression :
| e = exclusive_or_expression { e }
| a = inclusive_or_expression; "|"; b = exclusive_or_expression
{ (AST.BinaryOpExpr(OpBitor, a, b), CType.Undetermined) }

exclusive_or_expression :
| a = and_expression { a }
| a = exclusive_or_expression; "^"; b = and_expression
{ (AST.BinaryOpExpr(OpBitxor, a, b), CType.Undetermined) }

and_expression :
| e = equality_expression { e }
| a = and_expression; "&"; b = equality_expression
{ (AST.BinaryOpExpr(OpBitand, a, b), CType.Undetermined) }

equality_expression :
| r = relational_expression { r }
| a = equality_expression; "=="; b = relational_expression
{ (AST.BinaryOpExpr(OpEeq, a, b), CType.CInt) }
| a = equality_expression; "!="; b = relational_expression
{ (AST.BinaryOpExpr(OpNeq, a, b), CType.CInt) }

relational_expression :
| s = shift_expression { s }
| a = relational_expression; "<"; b = shift_expression
{ (AST.BinaryOpExpr(OpLt, a, b), CType.CInt) }
| a = relational_expression; ">"; b = shift_expression
{ (AST.BinaryOpExpr(OpGt, a, b), CType.CInt) }
| a = relational_expression; "<="; b = shift_expression
{ (AST.BinaryOpExpr(OpLeq, a, b), CType.CInt) }
| a = relational_expression; ">="; b = shift_expression
{ (AST.BinaryOpExpr(OpGeq, a, b), CType.CInt) }

(* todo *)
shift_expression :
| a = additive_expression { a }
| shift_expression "<<" additive_expression {}
| shift_expression ">>" additive_expression {}

(* todo *)
additive_expression :
| a = multiplicative_expression { a }
| additive_expression "+" multiplicative_expression {}
| additive_expression "-" multiplicative_expression {}

(* todo *)
multiplicative_expression :
| a = cast_expression { a }
| multiplicative_expression "*" cast_expression {}
| multiplicative_expression "/" cast_expression {}
| multiplicative_expression "%" cast_expression {}

(* todo *)
cast_expression :
| a = unary_expression { a }
| "(" type_name ")" cast_expression {}

(* todo *)
unary_expression :
| a = postfix_expression { a }
| "++" unary_expression {}
| "--" unary_expression {}
| unary_operator cast_expression {}
| "sizeof" unary_expression {}
| "sizeof" type_name {}

(* todo *)
postfix_expression :
| a = primary_expression { a }
| postfix_expression "[" expression "]" {}
| postfix_expression "(" assignment_expression* ")" {}
| postfix_expression "." identifier {}
| postfix_expression "->" identifier {}
| postfix_expression "++" {}
| postfix_expression "--" {}

(* todo *)
primary_expression :
| identifier {}
| constant {}
| Literal_string {}
| "(" expression ")" {}

(* todo *)
constant :
| Literal_int {}
| Literal_char {}
| Literal_real {}
// | enumeration_constant {}

(* todo *)
expression :
| assignment_expression {}
| expression "," assignment_expression {}

(* todo *)
assignment_expression :
| conditional_expression {}
| unary_expression assignment_operator assignment_expression {}

(* todo *)
assignment_operator :
| "=" {}
| "*=" {}
| "/=" {}
| "%=" {}
| "+=" {}
| "-=" {}
| "<<=" {}
| ">>=" {}
| "&=" {}
| "^=" {}
| "|=" {}

(* todo *)
unary_operator :
| "&" {}
| "*" {}
| "+" {}
| "-" {}
| "~" {}
| "!" {}

(* todo *)
type_name :
| specifier_qualifier+ abstract_declarator? {}

(* todo *)
parameter_type_list :
| a = parameter_list { a }
| parameter_list "," "..." {}

(* todo *)
parameter_list :
| a = parameter_declaration { a }
| parameter_list "," parameter_declaration {}

(* todo *)
parameter_declaration :
| declaration_specifier+ declarator {}
| declaration_specifier+ abstract_declarator {}
| declaration_specifier+ {}

(* todo *)
abstract_declarator :
| pointer {}
| pointer direct_abstract_declarator {}
| direct_abstract_declarator {}

(* todo *)
direct_abstract_declarator :
|  "(" abstract_declarator ")" {}
| direct_abstract_declarator? "[" constant_expression? "]" {}
| direct_abstract_declarator? "(" parameter_type_list? ")" {}

(* todo *)
enum_specifier :
| "enum" identifier "{" enumerator_list "}" {}
| "enum" "{" enumerator_list "}" {}
| "enum" identifier {}

(* todo *)
enumerator_list :
| enumerator {}
| enumerator_list "," enumerator {}

(* todo *)
enumerator :
| identifier {}
| identifier "=" constant_expression {}

// typedef_name :
// | identifier {}

(* todo *)
declaration :
|  declaration_specifier+ init_declarator* ";" {}

(* todo *)
init_declarator :
| declarator {}
| declarator "=" initzer {}

(* todo *)
initzer :
| assignment_expression {}
| "{" initializer_list "}" {}
| "{" initializer_list "," "}" {}

(* todo *)
initializer_list :
| initzer {}
| initializer_list "," initzer {}

(* todo *)
compound_statement :
| "{" declaration* statement* "}" {}

statement :
| a = labeled_statement { a }
| a = expression_statement { a }
| a = compound_statement { a }
| a = selection_statement { a }
| a = iteration_statement { a }
| a = jump_statement { a }

(* todo *)
labeled_statement :
| identifier ":" statement {}
| "case" constant_expression ":" statement {}
| "default" ":" statement {}

expression_statement :
| eo = expression? ";" { AST.ExprStmt eo }

selection_statement :
| "if"; "("; e = expression; ")"; s = statement { AST.IfStmt (e, s) }
| "if"; "("; e = expression; ")"; s1 = statement; "else"; s2 = statement 
{
    AST.IfElseStmt (e, s1, s2)
}
| "switch"; "("; e = expression; ")"; s = statement { AST.SwitchStmt (e, s) }

iteration_statement :
| "while"; "("; e = expression; ")"; s = statement { AST.WhileStmt (e, s) }
| "do"; s = statement; "while"; "("; e = expression; ")"; ";" { AST.DoWhileStmt (e, s) }
| "for"; "("; eo1 = expression? ";" eo2 = expression? ";" eo3 = expression? ")" s = statement 
{
    AST.ForStmt (eo1, eo2, eo3, s)
}

jump_statement :
| "goto"; i = identifier; ";" { AST.GotoStmt i }
| "continue" ";" { AST.ContinueStmt }
| "break" ";" { AST.BreakStmt }
| "return"; e = expression?; ";" { AST.ReturnStmt e }