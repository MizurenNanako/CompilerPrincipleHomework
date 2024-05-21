module Repr = struct
  open Syntatical.AST

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
    | CallExpr (s, el) -> p out "Call[%a, %a]" dump_expr s dump_expr_list el
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
      Printf.fprintf o "ParamList[";
      match l with
      | [] -> Printf.fprintf o "]"
      | [ (sp, vd) ] ->
          Printf.fprintf o "Pair[%a, %a]]" dump_spec sp dump_var_dec vd
      | (sp, vd) :: tl ->
          Printf.fprintf o "Pair[%a, %a], " dump_spec sp dump_var_dec vd;
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

module ToGraph = struct
  open Syntatical
  open Dotutils.DotGraph

  let of_uop (b : AST.uop) =
    match b with
    | OpNeg -> { g_label = "Neg"; g_arrows = [] }
    | OpNot -> { g_label = "Not"; g_arrows = [] }

  let of_bop (b : AST.bop) =
    match b with
    | OpAssign -> { g_label = "Assign"; g_arrows = [] }
    | OpAnd -> { g_label = "And"; g_arrows = [] }
    | OpOr -> { g_label = "Or"; g_arrows = [] }
    | OpPlus -> { g_label = "Plus"; g_arrows = [] }
    | OpMinus -> { g_label = "Minus"; g_arrows = [] }
    | OpStar -> { g_label = "Star"; g_arrows = [] }
    | OpDiv -> { g_label = "Div"; g_arrows = [] }
    | OpGt -> { g_label = "Gt"; g_arrows = [] }
    | OpLt -> { g_label = "Lt"; g_arrows = [] }
    | OpGeq -> { g_label = "Geq"; g_arrows = [] }
    | OpLeq -> { g_label = "Leq"; g_arrows = [] }
    | OpEeq -> { g_label = "Eeq"; g_arrows = [] }
    | OpNeq -> { g_label = "Neq"; g_arrows = [] }

  let of_str s = { g_label = Printf.sprintf "\\\"%s\\\"" s; g_arrows = [] }

  let rec of_expr (e : AST.expr) =
    match e with
    | UopExpr (u, e') ->
        { g_label = "UnaryOpExpr"; g_arrows = [ of_uop u; of_expr e' ] }
    | BopExpr (b, e', e'') ->
        {
          g_label = "BinaryOpExpr";
          g_arrows = [ of_bop b; of_expr e'; of_expr e'' ];
        }
    | CallExpr (e', el) ->
        { g_label = "CallExpr"; g_arrows = [ of_expr e'; of_expr_list el ] }
    | AccessExpr (e', e'') ->
        { g_label = "AccessExpr"; g_arrows = [ of_expr e'; of_expr e'' ] }
    | MemExpr (e', s) ->
        { g_label = "MemExpr"; g_arrows = [ of_expr e'; of_str s ] }
    | IdAtom s -> { g_label = "IdAtom"; g_arrows = [ of_str s ] }
    | IntAtom i ->
        { g_label = "IntAtom"; g_arrows = [ of_str (Int64.to_string i) ] }
    | FloatAtom f ->
        { g_label = "FloatAtom"; g_arrows = [ of_str (string_of_float f) ] }

  and of_expr_list el =
    { g_label = "ExprList"; g_arrows = List.map (fun e -> of_expr e) el }

  let rec of_stmt (s : AST.stmt) =
    match s with
    | ExprStmt e -> { g_label = "ExprStmt"; g_arrows = [ of_expr e ] }
    | CompStmt c -> of_cmpst c
    | RetStmt e -> { g_label = "RetStmt"; g_arrows = [ of_expr e ] }
    | IfStmt (e, s') ->
        { g_label = "IfStmt"; g_arrows = [ of_expr e; of_stmt s' ] }
    | IfElseStmt (e, s', s'') ->
        {
          g_label = "IfElseStmt";
          g_arrows = [ of_expr e; of_stmt s'; of_stmt s'' ];
        }
    | WhileStmt (e, s') ->
        { g_label = "WhileStmt"; g_arrows = [ of_expr e; of_stmt s' ] }

  and of_cmpst l =
    let c1, c2 = l in
    { g_label = "CompStmt"; g_arrows = [ of_def_list c1; of_stmt_list c2 ] }

  and of_stmt_list s =
    { g_label = "StmtList"; g_arrows = List.map (fun e -> of_stmt e) s }

  and of_def d =
    let a, b = d in
    {
      g_label = "Def";
      g_arrows =
        [ { g_label = "DefSpec"; g_arrows = [ of_spec a ] }; of_dec_list b ];
    }

  and of_def_list d =
    { g_label = "DefList"; g_arrows = List.map (fun e -> of_def e) d }

  and of_spec s =
    match s with
    | Spec CInt ->
        {
          g_label = "Spec";
          g_arrows = [ { g_label = "Type"; g_arrows = [ of_str "int" ] } ];
        }
    | Spec CFloat ->
        {
          g_label = "Spec";
          g_arrows = [ { g_label = "Type"; g_arrows = [ of_str "float" ] } ];
        }
    | StructSpec s -> { g_label = "Struct"; g_arrows = [ of_struct_spec s ] }

  and of_struct_spec s =
    match s with
    | StructDec s -> { g_label = "StructDec"; g_arrows = [ of_str s ] }
    | StructDef (Some s, dl) ->
        { g_label = "StructDef"; g_arrows = [ of_str s; of_def_list dl ] }
    | StructDef (None, dl) ->
        { g_label = "StructDef"; g_arrows = [ of_def_list dl ] }

  and of_dec d =
    match d with
    | vd, Some e -> { g_label = "Dec"; g_arrows = [ of_var_dec vd; of_expr e ] }
    | vd, None -> { g_label = "Dec"; g_arrows = [ of_var_dec vd ] }

  and of_dec_list d =
    { g_label = "DecList"; g_arrows = List.map (fun e -> of_dec e) d }

  and of_var_dec vd =
    match vd with
    | VarDecId s -> { g_label = "VarDecId"; g_arrows = [ of_str s ] }
    | VarDecArr (vd, n) ->
        {
          g_label = "VarDecArr";
          g_arrows = [ of_var_dec vd; of_str (Int64.to_string n) ];
        }

  and of_var_dec_list l =
    { g_label = "VarDecList"; g_arrows = List.map (fun e -> of_var_dec e) l }

  let of_fun_dec fd =
    let s, l = fd in
    let l' =
      List.map
        (fun e ->
          let sp, va = e in
          {
            g_label = "Pair";
            g_arrows =
              [
                { g_label = "FuncDecSpec"; g_arrows = [ of_spec sp ] };
                { g_label = "VarDec"; g_arrows = [ of_var_dec va ] };
              ];
          })
        l
    in
    {
      g_label = "FunDec";
      g_arrows = [ of_str s; { g_label = "FunDecList"; g_arrows = l' } ];
    }

  let of_ext_def (ed : AST.ext_def) =
    match ed with
    | ExtVarDec (sp, vdl) ->
        {
          g_label = "ExtVarDec";
          g_arrows = [ of_spec sp; of_var_dec_list vdl ];
        }
    | ExtFunDec (sp, fd, cps) ->
        {
          g_label = "ExtFunDec";
          g_arrows = [ of_spec sp; of_fun_dec fd; of_cmpst cps ];
        }

  let of_ext_def_list edl =
    { g_label = "ExtDefList"; g_arrows = List.map (fun e -> of_ext_def e) edl }

  let of_program (pg : AST.program) =
    { g_label = "Program"; g_arrows = [ of_ext_def_list pg ] }
end
