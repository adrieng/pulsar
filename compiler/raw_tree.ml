module T = Source_tree.Make(
  struct
    type id = string
    let print_id fmt id = Format.fprintf fmt "%s" id
    let compare_id = Warp.Utils.compare_string

    type ann = unit
    let print_ann (_ : Format.formatter) () = (() : unit)
    let compare_ann () () = 0
  end
)

open T

let join_loc e1 e2 =
  Loc.join e1.loc e2.loc

let make_app e1 e2 =
  {
    desc = App (e1, e2);
    loc = join_loc e1 e2;
    ann = ();
  }
