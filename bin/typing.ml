module CType = struct
  exception Invalid_CType

  type t = ct * et (* Qualifier, type *)
  and ct = bool (* NonConst (false) | Const (true) *)

  and et =
    | Undetermined
    | CPointer of t
    | CArray of t * int
    | CFunction of t * t list
    | CStruct of t list
    | CUnion of t list
    | CVoid (* for labels, their type is void *)
    | CInt of bool * int (* is_unsigned, bytes = 1, 2, 4, 8 *)
    | CReal of int (* bytes = 4, 8, 16 *)

  let rec deduce_spec (speclst : t list) : t =
    match speclst with
    | [] -> (false, CInt (false, 4))
    | [ a ] -> a
    | (false, a) :: (true, Undetermined) :: tl
    | (true, Undetermined) :: (false, a) :: tl ->
        (* allow to add const for once *)
        deduce_spec ((true, a) :: tl)
    | (c', Undetermined) :: (c'', t') :: tl
    | (c', t') :: (c'', Undetermined) :: tl ->
        let tl' = (c' || c'', t') :: tl in
        deduce_spec tl'
    | (c', CInt (u', 4)) :: (c'', CInt (u'', 4)) :: tl ->
        (* allow to form long long *)
        let tl' = (c' || c'', CInt (u' || u'', 8)) :: tl in
        deduce_spec tl'
    | (c', CInt (true, _)) :: (c'', CInt (u'', b'')) :: tl ->
        (* allow to consider unsigned *)
        let tl' = (c' || c'', CInt (u'', b'')) :: tl in
        deduce_spec tl'
    | (c', CInt (u', b')) :: (c'', CInt (u'', _)) :: tl ->
        (* allo to fold *)
        let tl' = (c' || c'', CInt (u' || u'', b')) :: tl in
        deduce_spec tl'
    | _ -> raise Invalid_CType

  let rec dump (out : out_channel) (self : t) : unit =
    let _s o' s' =
      match s' with false -> () | true -> Printf.fprintf o' "const "
    in
    let _u o' u' =
      match u' with true -> Printf.fprintf o' "unsigned " | false -> ()
    in
    match self with
    | s, Undetermined -> Printf.fprintf out "%a<error-type>" _s s
    | s, CArray (a, n) -> Printf.fprintf out "%a%a[%i]" _s s dump a n
    | s, CReal 8 -> Printf.fprintf out "%adouble" _s s
    | s, CReal 4 -> Printf.fprintf out "%afloat" _s s
    | s, CReal n -> Printf.fprintf out "%a__Float_%i_T_" _s s n
    | s, CInt (u, 1) -> Printf.fprintf out "%a%achar" _s s _u u
    | s, CInt (u, 2) -> Printf.fprintf out "%a%ashort" _s s _u u
    | s, CInt (u, 4) -> Printf.fprintf out "%a%aint" _s s _u u
    (* | s, CInt (u, 4) -> Printf.fprintf out "%a%along" _s s _u u *)
    | s, CInt (u, 8) -> Printf.fprintf out "%a%along long" _s s _u u
    | s, CInt (u, n) -> Printf.fprintf out "%a%a__Int_%i_T_" _s s _u u n
    | s, CPointer (_, CFunction (r, l)) ->
        (* Constness on function is meaningless *)
        Printf.fprintf out "%a(*%a)(%a)" dump r _s s _dump_list l
    | s, CPointer p -> Printf.fprintf out "%a *%a" dump p _s s
    | _, CFunction (r, l) -> Printf.fprintf out "%a(%a)" dump r _dump_list l
    | s, CStruct l -> Printf.fprintf out "%astruct{%a}" _s s _dump_list l
    | s, CUnion l -> Printf.fprintf out "%aunion{%a}" _s s _dump_list l
    | s, CVoid ->
        (* Constness on void is also meaningless, but we will keep it for deducing. *)
        Printf.fprintf out "%avoid" _s s

  and _dump_list o' l' =
    match l' with
    | [] -> ()
    | [ a ] -> Printf.fprintf o' "%a" dump a
    | hd :: tl ->
        Printf.fprintf o' "%a, " dump hd;
        _dump_list o' tl
end

module AST = struct
  type t = translation_unit

  and translation_unit =
    | GlobalVarDecl of var_decl
    | FuncDecl of func_decl
    | StructDecl of struct_decl
    | UnionDecl of union_decl

  and var_decl = identifier * expr
  and func_decl = identifier * identifier list * stmt list
  and struct_decl = identifier * identifier list
  and union_decl = identifier * identifier list
  and identifier = string * CType.t

  and stmt =
    | ForStmt of
        expr option * expr option * expr option * stmt list (* for (a;b;c) d *)
    | WhileStmt of expr * stmt list (* while (a) b *)
    | DoWhileStmt of expr * stmt list (* do b while (a) *)
    | VarDeclStmt of var_decl
    | IfStmt of expr * stmt list (* if (a) b *)
    | IfElseStmt of expr * stmt list * stmt list (* if (a) b else c *)
    | SwitchStmt of expr * stmt list
    | LabeledStmt of identifier * stmt list
    | GotoStmt of identifier
    | ContinueStmt
    | BreakStmt
    | ReturnStmt of expr option
    | ExprStmt of expr option

  and expr = untyped_expr * CType.t

  and untyped_expr =
    | UnaryOpExpr of unary_operator * expr
    | BinaryOpExpr of binary_operator * expr * expr
      (* No triary expression, it's also IfElseStmt. *)
    | IfElseExpr of expr * expr * expr
    | ObjectExpr of
        expr list (* C allows unnamed object, eq to a corresponding struct. *)

  and unary_operator =
    | OpTypeConv of CType.t
    | OpPos (* + *)
    | OpNeg (* - *)
    | OpAmp (* & *)
    | OpStar (* * *)
    | OpNot (* ! *)
    | OpPreInc (* ++ *)
    | OpPostInc (* ++ *)
    | OpPreDec (* -- *)
    | OpPostDec (* -- *)
    | OpCompl (* ~ *)
    | OpSizeof

  and binary_operator =
    | OpAdd (* + *)
    | OpSub (* - *)
    | OpMul (* * *)
    | OpDiv (* / *)
    | OpMod (* % *)
    | OpAnd (* && *)
    | OpOr (* || *)
    | OpBitand (* & *)
    | OpBitor (* | *)
    | OpBitxor (* ^ *)
    | OpCOMMA (* , *)
    | OpShl
    | OpShr
    | OpEq
    | OpAddeq
    | OpSubeq
    | OpMuleq
    | OpDiveq
    | OpModeq
    | OpShleq
    | OpShreq
    | OpBitandeq
    | OpBitoreq
    | OpBitxoreq
    | OpEeq
    | OpNeq
    | OpLt
    | OpGt
    | OpLeq
    | OpGeq
    | OpDot
    | OpTo

  let dump_op1 (out : out_channel) (uop : unary_operator) : unit =
    let p = Printf.fprintf in
    match uop with
    | OpTypeConv c -> p out "(%a)" CType.dump c
    | OpPos -> p out "Pos"
    | OpNeg -> p out "Neg"
    | OpAmp -> p out "Amp"
    | OpStar -> p out "Star"
    | OpNot -> p out "Not"
    | OpPreInc -> p out "PreInc"
    | OpPostInc -> p out "PostInc"
    | OpPreDec -> p out "PreDec"
    | OpPostDec -> p out "PostDec"
    | OpCompl -> p out "Compl"
    | OpSizeof -> p out "Sizeof"

  let dump_op2 (out : out_channel) (bop : binary_operator) : unit =
    let p = Printf.fprintf in
    match bop with
    | OpAdd -> p out "Add"
    | OpSub -> p out "Sub"
    | OpMul -> p out "Mul"
    | OpDiv -> p out "Div"
    | OpMod -> p out "Mod"
    | OpAnd -> p out "And"
    | OpOr -> p out "Or"
    | OpBitand -> p out "Bitand"
    | OpBitor -> p out "Bitor"
    | OpBitxor -> p out "Bitxor"
    | OpCOMMA -> p out "COMMA"
    | OpShl -> p out "Shl"
    | OpShr -> p out "Shr"
    | OpEq -> p out "Eq"
    | OpAddeq -> p out "Addeq"
    | OpSubeq -> p out "Subeq"
    | OpMuleq -> p out "Muleq"
    | OpDiveq -> p out "Diveq"
    | OpModeq -> p out "Modeq"
    | OpShleq -> p out "Shleq"
    | OpShreq -> p out "Shreq"
    | OpBitandeq -> p out "Bitandeq"
    | OpBitoreq -> p out "Bitoreq"
    | OpBitxoreq -> p out "Bitxoreq"
    | OpEeq -> p out "Eeq"
    | OpNeq -> p out "Neq"
    | OpLt -> p out "Lt"
    | OpGt -> p out "Gt"
    | OpLeq -> p out "Leq"
    | OpGeq -> p out "Geq"
    | OpDot -> p out "Dot"
    | OpTo -> p out "To"

  let rec dump_stmt out (st : stmt) =
    match st with
    | ForStmt (e, e', e'', sl) ->
        Printf.fprintf out "For(Pre(%a), Cond(%a), Post(%a), Body(%a))"
          dump_expr_option e dump_expr_option e' dump_expr_option e''
          dump_stmt_list sl
    | WhileStmt (e, sl) ->
        Printf.fprintf out "While(Cond(%a), Body(%a))" dump_expr e
          dump_stmt_list sl
    | DoWhileStmt (e, sl) ->
        Printf.fprintf out "DoWhile(Unless(%a), Body(%a))" dump_expr e
          dump_stmt_list sl
    | IfStmt (e, sl) ->
        Printf.fprintf out "If(Cond(%a), Body(%a))" dump_expr e dump_stmt_list
          sl
    | IfElseStmt (e, sl, sl') ->
        Printf.fprintf out "IfElse(Cond(%a), True(%a), False(%a))" dump_expr e
          dump_stmt_list sl dump_stmt_list sl'
    | SwitchStmt (e, s) ->
        Printf.fprintf out "Switch(Cond(%a), Body(%a))" dump_expr e
          dump_stmt_list s
    | LabeledStmt (i, s) ->
        Printf.fprintf out "Labeled(Id(%a), Body(%a))" dump_identifier i
          dump_stmt_list s
    | GotoStmt i -> Printf.fprintf out "Goto(Id(%a))" dump_identifier i
    | ContinueStmt -> Printf.fprintf out "Continue"
    | BreakStmt -> Printf.fprintf out "Continue"
    | ReturnStmt (Some a) -> Printf.fprintf out "Return(%a)" dump_expr a
    | ReturnStmt None -> Printf.fprintf out "Return"
    | ExprStmt a -> Printf.fprintf out "Expr(%a)" dump_expr_option a
    | VarDeclStmt a -> Printf.fprintf out "%a" dump_var_decl a

  and dump_stmt_list out sl =
    match sl with
    | [] -> ()
    | [ a ] -> Printf.fprintf out "%a" dump_stmt a
    | hd :: tl ->
        Printf.fprintf out "%a, " dump_stmt hd;
        dump_stmt_list out tl

  and dump_untyped_expr out utexpr =
    match utexpr with
    | UnaryOpExpr (uop, e) ->
        Printf.fprintf out "%a(%a)" dump_op1 uop dump_expr e
    | BinaryOpExpr (bop, e1, e2) ->
        Printf.fprintf out "%a(%a, %a)" dump_op2 bop dump_expr e1 dump_expr e2
    | IfElseExpr (e, s, s') ->
        Printf.fprintf out "Select(Cond(%a), True(%a), False(%a))" dump_expr e
          dump_expr s dump_expr s'
    | ObjectExpr l ->
        let rec _l o' l' =
          match l' with
          | [] -> ()
          | [ a ] -> Printf.fprintf o' "%a" dump_expr a
          | hd :: tl ->
              Printf.fprintf o' "%a, " dump_expr hd;
              _l o' tl
        in
        Printf.fprintf out "Obj(%a)" _l l

  and dump_expr out (e : expr) =
    let ue, ct = e in
    Printf.fprintf out "<%a>%a" CType.dump ct dump_untyped_expr ue

  and dump_expr_option out (e : expr option) =
    match e with Some a -> dump_expr out a | None -> ()

  and dump_identifier out (id : identifier) : unit =
    let s, ct = id in
    Printf.fprintf out "<%a>Id(\"%s\")" CType.dump ct s

  and dump_identifier_list out idl =
    match idl with
    | [] -> ()
    | [ a ] -> Printf.fprintf out "%a" dump_identifier a
    | hd :: tl ->
        Printf.fprintf out "%a, " dump_identifier hd;
        dump_identifier_list out tl

  and dump_var_decl out (vd : var_decl) : unit =
    let id, e = vd in
    Printf.fprintf out "VarDecl(%a <- %a)" dump_identifier id dump_expr e

  let dump_translation_unit out (tr : translation_unit) : unit =
    match tr with
    | GlobalVarDecl a -> Printf.fprintf out "Global(%a)" dump_var_decl a
    | StructDecl a ->
        let id, idl = a in
        Printf.fprintf out "StructDecl(%a <- Struct(%a))" dump_identifier id
          dump_identifier_list idl
    | UnionDecl a ->
        let id, idl = a in
        Printf.fprintf out "UnionDecl(%a <- Union(%a))" dump_identifier id
          dump_identifier_list idl
    | FuncDecl a ->
        let id, idl, s = a in
        Printf.fprintf out "FuncDecl(%a <- Func(Param(%a), Body(%a)))"
          dump_identifier id dump_identifier_list idl dump_stmt_list s

  let dump out self =
    Printf.fprintf out "TranslationUnit(%a)" dump_translation_unit self
end
