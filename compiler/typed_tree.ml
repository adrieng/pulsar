module T = Source_tree.Make(
  struct
    module Id =
      struct
        type t = Ident.t
        let print = Ident.print_source
        let compare = Ident.compare
      end

    module PatAnnot = Type
    module ExpAnnot = Type
    module EquAnnot = Type
    module PhrAnnot = Warp.Utils.PrintableOrderedUnit
  end
)

let coerce_with e c ty =
  let open T in
  match c, e.e_desc with
  | Coercion.Id, _ ->
     e
  | _, ESub { ctx; exp; res; } ->
     { e with e_desc = ESub { ctx; exp; res = Coercion.seq (res, c); }; }
  | _ ->
    {
      e_desc = T.ESub { ctx = []; exp = e; res = c; };
      e_loc = e.e_loc;
      e_ann = ty;
    }
