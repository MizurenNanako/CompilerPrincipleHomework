module CType = struct
  exception Invalid_CType

  type t = ct * et (* Qualifier, type *)
  and ct = Default | Const

  and et =
    | Undetermined
    | CPointer of t
    | CArray of t * int
    | CFunction of t * t list
    | CStruct of t list
    | CUnion of t list
    | CVoid
    | CChar of bool (* is_unsigned *)
    | CShort of bool (* is_unsigned *)
    | CInt of bool (* is_unsigned *)
    | CLong of bool (* is_unsigned *)
    | CFloat
    | CDouble
    (* Following types are longer than 1 word *)
    | CLongLong of bool

  let combine_spec speclst =
    match speclst with
    | [] -> CInt false
    | [ CLong u1; CLong u2; CInt u3 ] -> CLongLong (u1 || u2 || u3)
    | [ CShort u1; CInt u2 ] -> CShort (u1 || u2)
    | [ CLong u1; CInt u2 ] -> CLong (u1 || u2)
    | [ CLong u1; CLong u2 ] -> CLongLong (u1 || u2)
    | [ a ] -> a
    | _ -> raise Invalid_CType
    
  let rec dump (out : out_channel) (self : t) : unit =
    let _s o' s' =
      match s' with Default -> () | Const -> Printf.fprintf o' "const "
    in
    let _u o' u' =
      match u' with true -> Printf.fprintf o' "unsigned " | false -> ()
    in
    match self with
    | s, Undetermined -> Printf.fprintf out "%a<error-type>" _s s
    | s, CArray (a, n) -> Printf.fprintf out "%a%a[%i]" _s s dump a n
    | s, CDouble -> Printf.fprintf out "%adouble" _s s
    | s, CFloat -> Printf.fprintf out "%afloat" _s s
    | s, CChar u -> Printf.fprintf out "%a%achar" _s s _u u
    | s, CShort u -> Printf.fprintf out "%a%ashort" _s s _u u
    | s, CInt u -> Printf.fprintf out "%a%aint" _s s _u u
    | s, CLong u -> Printf.fprintf out "%a%along" _s s _u u
    | s, CLongLong u -> Printf.fprintf out "%a%along long" _s s _u u
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
    | ForStmt of for_stmt
    | WhileStmt of while_stmt
    | DoWhileStmt of do_while_stmt
    | VarDeclStmt of var_decl
    | IfElseStmt of if_else_stmt
    | EvalStmt of expr

  and for_stmt = stmt * expr * stmt * stmt list (* for (a;b;c) d *)
  and while_stmt = expr * stmt list (* while (a) b *)
  and do_while_stmt = expr * stmt list (* do b while (a) *)
  and if_else_stmt = expr * stmt list * stmt list (* if (a) b else c *)
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
    | ForStmt a ->
        let s, e, s', sl = a in
        Printf.fprintf out "For(Pre(%a), Cond(%a), Post(%a), Body(%a))"
          dump_stmt s dump_expr e dump_stmt s' dump_stmt_list sl
    | WhileStmt a ->
        let e, sl = a in
        Printf.fprintf out "While(Cond(%a), Body(%a))" dump_expr e
          dump_stmt_list sl
    | DoWhileStmt a ->
        let e, sl = a in
        Printf.fprintf out "DoWhile(Unless(%a), Body(%a))" dump_expr e
          dump_stmt_list sl
    | IfElseStmt a ->
        let e, sl, sl' = a in
        Printf.fprintf out "IfElse(Cond(%a), True(%a), False(%a))" dump_expr e
          dump_stmt_list sl dump_stmt_list sl'
    | EvalStmt a -> Printf.fprintf out "%a" dump_expr a
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

  and dump_expr out e =
    let ue, ct = e in
    Printf.fprintf out "<%a>%a" CType.dump ct dump_untyped_expr ue

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
