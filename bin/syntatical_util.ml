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
    | MemExpr (e', s, _) ->
        { g_label = "MemExpr"; g_arrows = [ of_expr e'; of_str s ] }
    | IdAtom (s, _) -> { g_label = "IdAtom"; g_arrows = [ of_str s ] }
    | IntAtom (i, _) ->
        { g_label = "IntAtom"; g_arrows = [ of_str (Int64.to_string i) ] }
    | FloatAtom (f, _) ->
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
