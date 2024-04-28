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
    | CallExpr of string * expr list
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

  let dump_uop out u =
    match u with
    | OpNeg -> Printf.fprintf out "OpNeg"
    | OpNot -> Printf.fprintf out "OpNot"

  let dump_bop out b =
    match b with
    | OpAssign -> Printf.fprintf out "OpAssign"
    | OpAnd -> Printf.fprintf out "OpAnd"
    | OpOr -> Printf.fprintf out "OpOr"
    | OpPlus -> Printf.fprintf out "OpPlus"
    | OpMinus -> Printf.fprintf out "OpMinus"
    | OpStar -> Printf.fprintf out "OpStar"
    | OpDiv -> Printf.fprintf out "OpDiv"
    | OpGt -> Printf.fprintf out "OpGt"
    | OpLt -> Printf.fprintf out "OpLt"
    | OpGeq -> Printf.fprintf out "OpGeq"
    | OpLeq -> Printf.fprintf out "OpLeq"
    | OpEeq -> Printf.fprintf out "OpEeq"
    | OpNeq -> Printf.fprintf out "OpNeq"

  let rec dump_expr out e =
    let p = Printf.fprintf in
    match e with
    | UopExpr (u, e') -> p out "%a[%a]" dump_uop u dump_expr e'
    | BopExpr (u, e', e'') ->
        p out "%a[%a, %a]" dump_bop u dump_expr e' dump_expr e''
    | FloatAtom a -> p out "Float[%f]" a
    | IntAtom a -> p out "Int[%s]" (Int64.to_string a)
    | IdAtom a -> p out "Id[%s]" a
    | CallExpr (s, el) -> p out "Call[%s, %a]" s dump_expr_list el
    | MemExpr (e', s) -> p out "Member[%s, %a]" s dump_expr e'
    | AccessExpr (e', e'') -> p out "Access[%a, %a]" dump_expr e' dump_expr e''

  and dump_expr_list out el =
    let p = Printf.fprintf in
    match el with
    | [] -> p out "]"
    | [ a ] -> p out "%a" dump_expr a
    | hd :: tl ->
        p out "%a, " dump_expr hd;
        dump_expr_list out tl

  and dump_fun_dec out (f : fun_dec) =
    let rec _l o l =
      match l with
      | [] -> ()
      | [ (sp, vd) ] ->
          Printf.fprintf o "(%a: %a)]" dump_var_dec vd dump_spec sp
      | (sp, vd) :: tl ->
          Printf.fprintf o "(%a: %a), " dump_var_dec vd dump_spec sp;
          _l o tl
    in
    let s, b = f in
    Printf.fprintf out "FunDec[%s, %a]" s _l b

  and dump_stmt out st =
    let p = Printf.fprintf in
    match st with
    | ExprStmt e -> p out "ExprStmt[%a]" dump_expr e
    | IfStmt (e, s) -> p out "IfStmt[%a, %a]" dump_expr e dump_stmt s
    | IfElseStmt (e, s, s') ->
        p out "IfElseStmt[%a, %a, %a]" dump_expr e dump_stmt s dump_stmt s'
    | RetStmt e -> p out "RetStmt[%a]" dump_expr e
    | WhileStmt (e, s) -> p out "WhileStmt[%a, %a]" dump_expr e dump_stmt s
    | CompStmt c -> p out "%a" dump_comp_stmt c

  and dump_stmt_list o l =
    Printf.fprintf o "StmtList[";
    match l with
    | [] -> ()
    | [ a ] -> Printf.fprintf o "%a]" dump_stmt a
    | hd :: tl ->
        Printf.fprintf o "%a, " dump_stmt hd;
        dump_stmt_list o tl

  and dump_comp_stmt out c =
    let dl, sl = c in
    Printf.fprintf out "%a, %a" dump_def_list dl dump_stmt_list sl

  and dump_def out d =
    let sp, dl = d in
    Printf.fprintf out "Def[%a, %a]" dump_spec sp dump_dec_list dl

  and dump_def_list o l =
    Printf.fprintf o "DefList[";
    match l with
    | [] -> ()
    | [ a ] -> Printf.fprintf o "%a]" dump_def a
    | hd :: tl ->
        Printf.fprintf o "%a, " dump_def hd;
        dump_def_list o tl

  and dump_dec out d =
    match d with
    | vd, Some e -> Printf.fprintf out "Dec[%a, %a]" dump_var_dec vd dump_expr e
    | vd, None -> Printf.fprintf out "Dec[%a]" dump_var_dec vd

  and dump_dec_list o l =
    Printf.fprintf o "DecList[";
    match l with
    | [] -> ()
    | [ a ] -> Printf.fprintf o "%a]" dump_dec a
    | hd :: tl ->
        Printf.fprintf o "%a, " dump_dec hd;
        dump_dec_list o tl

  and dump_var_dec out v =
    match v with
    | VarDecId s -> Printf.fprintf out "VarId[%s]" s
    | VarDecArr (v', n) ->
        Printf.fprintf out "VarArr[%s, %a]" (Int64.to_string n) dump_var_dec v'

  and dump_var_dec_list o l =
    Printf.fprintf o "VarDecList[";
    match l with
    | [] -> ()
    | [ a ] -> Printf.fprintf o "%a]" dump_var_dec a
    | hd :: tl ->
        Printf.fprintf o "%a, " dump_var_dec hd;
        dump_var_dec_list o tl

  and dump_struct_spec out s =
    match s with
    | StructDec s -> Printf.fprintf out "StructDec[%s]" s
    | StructDef (Some s, dl) ->
        Printf.fprintf out "StructDec[%s, StructDef[%a]]" s dump_def_list dl
    | StructDef (None, dl) ->
        Printf.fprintf out "StructDef[%a]" dump_def_list dl

  and dump_spec o s =
    match s with
    | Spec CInt -> Printf.fprintf o "Spec[int]"
    | Spec CFloat -> Printf.fprintf o "Spec[float]"
    | StructSpec st -> Printf.fprintf o "Spec[%a]" dump_struct_spec st

  and dump_ext_def o e =
    match e with
    | ExtVarDec (sp, vdl) ->
        Printf.fprintf o "ExtVarDec[%a, %a]" dump_spec sp dump_var_dec_list vdl
    | ExtFunDec (sp, fd, cps) ->
        Printf.fprintf o "ExtFunDec[%a, %a, %a]" dump_spec sp dump_fun_dec fd
          dump_comp_stmt cps

  and dump_program o l =
    Printf.fprintf o "ExtDefList[";
    match l with
    | [] -> ()
    | [ a ] -> Printf.fprintf o "%a]" dump_ext_def a
    | hd :: tl ->
        Printf.fprintf o "%a, " dump_ext_def hd;
        dump_program o tl

  let dump = dump_program
end
