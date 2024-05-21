%{
    open Syntatical.AST
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
| l = ext_def*; EOF { l }

ext_def:
| sp = specifier; l = separated_list(COMMA, var_dec); SEMI; { ExtVarDec (sp, l) }
// | sp = specifier; SEMI { ExtVarDec (sp, []) }
| sp = specifier; fd = fun_dec; cps = comp_st { ExtFunDec (sp, fd, cps) }

specifier:
| c = ctype { Spec c }
| s = struct_specifier { StructSpec s }

struct_specifier:
| STRUCT; t = opt_tag; LC; dl = def*; RC; { StructDef (t, dl) }
| STRUCT; t = tag { StructDec t }

opt_tag:
| s = ID { Some s }
| {  None }

tag:
| s = ID {  s }

ctype:
| KINT     {  CInt }
| KFLOAT   {  CFloat }

var_dec:
| i = ID {  VarDecId i }
| vd = var_dec; LB; n = INT; RB; {  VarDecArr (vd, n) }

fun_dec:
| s = ID; LP; vl = var_list; RP; {  (s, vl) }
| s = ID; LP; RP; {  (s, []) }

var_list:
| pd = param_dec; COMMA; vl = var_list {  pd :: vl }
| pd = param_dec; {  [pd] }

param_dec:
| sp = specifier; vd = var_dec {  (sp, vd) }

comp_st:
| LC; dl = def*; sl = stmt*; RC; {  (dl, sl) }

stmt:
| e = exp; SEMI; {  ExprStmt e }
| c = comp_st {  CompStmt c }
| RETURN; e = exp; SEMI; {  RetStmt e }
| IF; LP; e = exp; RP; s = stmt; %prec below_ELSE {  IfStmt (e, s) }
| IF; LP; e = exp; RP; s1 = stmt; ELSE; s2 = stmt; {  IfElseStmt (e, s1, s2) }
| WHILE; LP; e = exp; RP; s = stmt; {  WhileStmt (e, s) }

def:
| sp = specifier; dl = separated_list(COMMA, dec); SEMI; 
{  (sp, dl) }

dec:
| vd = var_dec; {  (vd, None) }
| vd = var_dec; ASSIGNOP; e = exp; {  (vd, Some e) }

exp:
| e1 = exp; ASSIGNOP; e2 = exp; {  BopExpr (OpAssign, e1, e2) }
| e1 = exp; AND; e2 = exp; {  BopExpr (OpAnd, e1, e2) }
| e1 = exp; OR; e2 = exp; {  BopExpr (OpOr, e1, e2) }
| e1 = exp; GT; e2 = exp; {  BopExpr (OpGt, e1, e2) }
| e1 = exp; LT; e2 = exp; {  BopExpr (OpLt, e1, e2) }
| e1 = exp; GEQ; e2 = exp; {  BopExpr (OpGeq, e1, e2) }
| e1 = exp; LEQ; e2 = exp; {  BopExpr (OpLeq, e1, e2) }
| e1 = exp; EEQ; e2 = exp; {  BopExpr (OpEeq, e1, e2) }
| e1 = exp; NEQ; e2 = exp; {  BopExpr (OpNeq, e1, e2) }
| e1 = exp; PLUS; e2 = exp; {  BopExpr (OpPlus, e1, e2) }
| e1 = exp; MINUS; e2 = exp; {  BopExpr (OpMinus, e1, e2) }
| e1 = exp; STAR; e2 = exp; {  BopExpr (OpStar, e1, e2) }
| e1 = exp; DIV; e2 = exp; {  BopExpr (OpDiv, e1, e2) }
| LP; e = exp; RP; {  e }
| MINUS; e = exp; %prec UMINUS {  UopExpr (OpNeg, e) }
| NOT; e = exp; {UopExpr (OpNot, e) }
| e = exp; LP; args = separated_list(COMMA, exp); RP; 
{
    CallExpr (e, args)
}
// | s = ID; LP; RP; {  CallExpr (s, []) }
| e1 = exp; LB; e2 = exp; RB; {  AccessExpr (e1, e2) }
| e1 = exp; DOT; s = ID; {  MemExpr (e1, s) }
| s = ID {  IdAtom s }
| i = INT {  IntAtom i }
| f = FLOAT {  FloatAtom f }
