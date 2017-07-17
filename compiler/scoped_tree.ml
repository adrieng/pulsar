module T = Source_tree.Make(
  struct
    type id = Ident.t
    let print_id = Ident.print
    let compare_id = Ident.compare

    type ann = unit
    let print_ann (_ : Format.formatter) () = (() : unit)
    let compare_ann () () = 0
  end
)
