module AST = struct
  type t = program
  and program = ext_def list

  and ext_def =
    | ExtVarDec of spec * var_dec list
    | ExtFunDec of spec * fun_dec * comp_st

  and spec = Spec of ctype | StructSpec of struct_spec
  and ctype = CInt | CFloat

  and struct_spec =
    | StructDec of string (*name*)
    | StructDef of string option * def list (*name, body*)

  and var_dec = VarDecId of string | VarDecArr of var_dec * int64
  and fun_dec = string * (spec * var_dec) list
  and def = spec * dec list
  and dec = var_dec * expr option

  and stmt =
    | ExprStmt of expr
    | CompStmt of comp_st
    | RetStmt of expr
    | IfStmt of expr * stmt
    | IfElseStmt of expr * stmt * stmt
    | WhileStmt of expr * stmt

  and comp_st = def list * stmt list

  and expr =
    | UopExpr of uop * expr
    | BopExpr of bop * expr * expr
    | CallExpr of expr * expr list
    | AccessExpr of expr * expr
    | MemExpr of expr * string (* expr.id *)
    | IdAtom of string
    | IntAtom of int64
    | FloatAtom of float

  and uop = OpNeg | OpNot

  and bop =
    | OpAssign
    | OpAnd
    | OpOr
    | OpPlus
    | OpMinus
    | OpStar
    | OpDiv
    | OpGt
    | OpLt
    | OpGeq
    | OpLeq
    | OpEeq
    | OpNeq
end
