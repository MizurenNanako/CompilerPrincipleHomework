exception SemanticError of string

(** [AST] only cares about few things:
      - data
      - code
      - ref
      data are merely data, code are stmt blocks, and ref are names.

      For a C function definition, literals are data, all declarations
         are ref binding data/code to name. ref will not be preserved in
         target byte code.
*)
module AST = struct
  type identifier_type = String.t

  module Data = struct
    type id_t = identifier_type

    (** [CType] is the type of the data *)
    module CType = struct
      type t =
        | CVoid
        | CInt
        | CFloat
        | CPtr of t
        | CStruct of c_record_type
        | CUnion of c_record_type
        | CArray of t * int
        | CFunc of t * t list

      and c_record_type = (id_t * t) list
      (** this has a memberof operation *)

      let member_type (t : c_record_type) (id : id_t) =
        match List.find_opt (fun (id', _) -> id' = id) t with
        | Some (_, ty) -> ty
        | None ->
            raise
              (SemanticError ("member not found in record type: " ^ id))
    end

    (** This is how value will be stored in memory or registers, 
        it dosn't matter with its type. *)
    module CValue = struct
      type t =
        | CInt32 of int32
        | CFloat32 of float
        | CTuple of t list (* for product type *)
    end

    type t = {
      position : Lexical.LexicalRange.t;
      ctype : CType.t;
      cvalue : CValue.t;
    }
  end

  module Code = struct
    type id_t = identifier_type
  end

  module Ref = struct
    type id_t = identifier_type

    type t = { symbol : id_t; ref_type : ref_type }

    and ref_type =
      | Internal  (** internal linkage reference *)
      | External  (** external linkage reference *)
  end
end
