%{
    open Typing.AST
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
| ext_def* EOF { $1 }

ext_def:
| specifier separated_list(COMMA, var_dec) SEMI { ExtVarDec ($1, $2) }
// | sp = specifier SEMI { ExtVarDec (sp, []) }
| specifier fun_dec comp_st { ExtFunDec ($1, $2, $3) }

specifier:
| ctype { Spec $1 }
| struct_specifier { StructSpec $1 }

struct_specifier:
| STRUCT opt_tag LC def* RC { StructDef ($2, $4) }
| STRUCT tag { StructDec $2 }

opt_tag:
| ID { Some $1 }
| { None }

tag:
| ID { $1 }

ctype:
| KINT     { CInt }
| KFLOAT   { CFloat }

var_dec:
| ID { VarDecId $1 }
| var_dec LB INT RB { VarDecArr ($1, $3) }

fun_dec:
| ID LP var_list RP { ($1, $3) }
| ID LP RP { ($1, []) }

var_list:
| param_dec COMMA var_list { $1 :: $3 }
| param_dec { [$1] }

param_dec:
| specifier var_dec { ($1, $2) }

comp_st:
| LC def* stmt* RC { ($2, $3) }

stmt:
| exp SEMI { ExprStmt $1 }
| comp_st { CompStmt $1 }
| RETURN exp SEMI { RetStmt $2 }
| IF LP exp RP stmt %prec below_ELSE { IfStmt ($3, $5) }
| IF LP exp RP stmt ELSE stmt { IfElseStmt ($3, $5, $7) }
| WHILE LP exp RP stmt { WhileStmt ($3, $5) }

def:
| specifier separated_list(COMMA, dec) SEMI { ($1, $2) }

dec:
| var_dec { ($1, None) }
| var_dec ASSIGNOP exp { ($1, Some $3) }

exp:
| exp ASSIGNOP exp { BopExpr (OpAssign, $1, $3) }
| exp AND exp { BopExpr (OpAnd, $1, $3) }
| exp OR exp { BopExpr (OpOr, $1, $3) }
| exp GT exp { BopExpr (OpGt, $1, $3) }
| exp LT exp { BopExpr (OpLt, $1, $3) }
| exp GEQ exp { BopExpr (OpGeq, $1, $3) }
| exp LEQ exp { BopExpr (OpLeq, $1, $3) }
| exp EEQ exp { BopExpr (OpEeq, $1, $3) }
| exp NEQ exp { BopExpr (OpNeq, $1, $3) }
| exp PLUS exp { BopExpr (OpPlus, $1, $3) }
| exp MINUS exp { BopExpr (OpMinus, $1, $3) }
| exp STAR exp { BopExpr (OpStar, $1, $3) }
| exp DIV exp { BopExpr (OpDiv, $1, $3) }
| LP exp RP { $2 }
| MINUS exp %prec UMINUS { UopExpr (OpNeg, $2) }
| NOT exp {UopExpr (OpNot, $2) }
| exp LP separated_list(COMMA, exp) RP 
{
    CallExpr ($1, $3)
}
| exp LB exp RB { AccessExpr ($1, $3) }
| exp DOT ID { MemExpr ($1, $3) }
| ID { IdAtom $1 }
| INT { IntAtom $1 }
| FLOAT { FloatAtom $1 }
| error { IntAtom 0L }
