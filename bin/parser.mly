%{
    (* open Typing.AST *)
%}

%token <int64> INT
%token <float> FLOAT
%token <string> ID
%token SEMI ";"
%token COMMA ","
%token ASSIGNOP "="

// RELOP
%token GT ">"
%token LT "<"
%token GEQ ">="
%token LEQ "<="
%token EEQ "=="
%token NEQ "!="

%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token DIV "/"
%token AND "&&"
%token OR "||"
%token DOT "."
%token NOT "!"

// %token TYPE
%token KINT "int"
%token KFLOAT "float"
%token LP "("
%token RP ")"
%token LB "["
%token RB "]"
%token LC "{"
%token RC "}"
%token STRUCT "struct"
%token RETURN "return"
%token IF "if"
%token ELSE "else"
%token WHILE "while"

%token EOF

%right "="
%left "||"
%left "&&"
%left "!=" "==" ">=" ">" "<=" "<"
%left "+" "-"
%left "*" "/"
%right UMINUS "!"
%nonassoc "["
%nonassoc "("
%left "."

%nonassoc below_ELSE
%nonassoc ELSE

%start <t> program

%%

program:
| l = ext_def*; EOF {}

ext_def:
| sp = specifier; l = separated_list(COMMA, var_dec); SEMI; {}
// | sp = specifier; SEMI {}
| sp = specifier; fd = fun_dec; cps = comp_st {}

specifier:
| c = ctype {}
| s = struct_specifier {}

struct_specifier:
| STRUCT; t = opt_tag; LC; dl = def*; RC; {}
| STRUCT; t = tag {}

opt_tag:
| s = ID {}
| {}

tag:
| s = ID {}

ctype:
| KINT     {}
| KFLOAT   {}

var_dec:
| i = ID {}
| vd = var_dec; LB; n = INT; RB; {}

fun_dec:
| s = ID; LP; vl = var_list; RP; {}
| s = ID; LP; RP; {}

var_list:
| pd = param_dec; COMMA; vl = var_list {}
| pd = param_dec; {}

param_dec:
| sp = specifier; vd = var_dec {}

comp_st:
| LC; dl = def*; sl = stmt*; RC; {}

stmt:
| e = exp; SEMI; {}
| c = comp_st {}
| RETURN; e = exp; SEMI; {}
| IF; LP; e = exp; RP; s = stmt; %prec below_ELSE {}
| IF; LP; e = exp; RP; s1 = stmt; ELSE; s2 = stmt; {}
| WHILE; LP; e = exp; RP; s = stmt; {}

def:
| sp = specifier; dl = separated_list(COMMA, dec); SEMI; 
{}

dec:
| vd = var_dec; {}
| vd = var_dec; ASSIGNOP; e = exp; {}

exp:
| e1 = exp; ASSIGNOP; e2 = exp; {}
| e1 = exp; AND; e2 = exp; {}
| e1 = exp; OR; e2 = exp; {}
| e1 = exp; GT; e2 = exp; {}
| e1 = exp; LT; e2 = exp; {}
| e1 = exp; GEQ; e2 = exp; {}
| e1 = exp; LEQ; e2 = exp; {}
| e1 = exp; EEQ; e2 = exp; {}
| e1 = exp; NEQ; e2 = exp; {}
| e1 = exp; PLUS; e2 = exp; {}
| e1 = exp; MINUS; e2 = exp; {}
| e1 = exp; STAR; e2 = exp; {}
| e1 = exp; DIV; e2 = exp; {}
| LP; e = exp; RP; {}
| MINUS; e = exp; %prec UMINUS {}
| NOT; e = exp; {}
| e = exp; LP; args = separated_list(COMMA, exp); RP; 
{
}
// | s = ID; LP; RP; {}
| e1 = exp; LB; e2 = exp; RB; {}
| e1 = exp; DOT; s = ID; {}
| s = ID {}
| i = INT {}
| f = FLOAT {}
