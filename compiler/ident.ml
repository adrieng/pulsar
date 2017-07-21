type kind = Source | Internal

type t =
  {
    num : int;
    name : string;
    name_num : int;
    kind : kind;
  }

type ctx =
  {
    mutable global_cpt : int;
    available_name_nums : (string, int ref) Hashtbl.t;
  }

let make_ctx () =
  {
    global_cpt = 0;
    available_name_nums = Hashtbl.create 1000;
  }

let _current_ctx = ref (make_ctx ())

let get_current_ctx () = !_current_ctx

let set_current_ctx ctx = _current_ctx := ctx

let reset_ctx () = set_current_ctx (make_ctx ())

let make_ident kind name =
  let available_name_num =
    try Hashtbl.find !_current_ctx.available_name_nums name
    with Not_found ->
      let r = ref 0 in
      Hashtbl.add !_current_ctx.available_name_nums name r;
      r
  in
  let id =
    {
      num = !_current_ctx.global_cpt;
      name = name;
      name_num = !available_name_num;
      kind = kind;
    }
  in
  !_current_ctx.global_cpt <- !_current_ctx.global_cpt + 1;
  incr available_name_num;
  id

let compare b1 b2 = b2.num - b1.num

let make_source = make_ident Source
let make_internal = make_ident Internal

let make_suffix b s = make_internal (b.name ^ s)

let make_prefix s b = make_internal (s ^ b.name)

let refresh id = make_ident id.kind id.name

let hash id = Hashtbl.hash id.num

let equal id1 id2 = 0 = compare id1 id2

type t_ = t
module Ident_ordered =
struct
  type t = t_
  let compare = compare
end

let to_string id =
  let print_short = id.kind = Source && id.name_num = 0 in
  if print_short then id.name else Printf.sprintf "%s_%d" id.name id.name_num

let print fmt id = Format.fprintf fmt "%s" (to_string id)

let print_source fmt id =
  match id.kind with
  | Source ->
     Format.fprintf fmt "%s" id.name
  | Internal ->
     Format.fprintf fmt "%s" (to_string id)

module Set =
struct
  module M = Set.Make(Ident_ordered)

  include M

  let print fmt s =
    let l = M.fold (fun x l -> x :: l) s [] in
    Warp.Print.(pp_list
                  ~pp_sep:pp_comma
                  print) fmt l

  let unions ss =
    let rec loop acc ss =
      match ss with
      | [] -> acc
      | s :: ss ->
         loop (union acc s) ss
    in
    loop empty ss
end

module Env =
struct
  module M = Map.Make(Ident_ordered)

  include M

  let of_assoc_list l =
    List.fold_left (fun env (id, x) -> M.add id x env) M.empty l

  let to_assoc_list env =
    fold (fun k v l -> (k, v) :: l) env []

  let union env1 env2 = M.fold (fun k v env2 -> M.add k v env2) env1 env2

  let trim env set =
    let add k v env = if Set.mem k set then M.add k v env else env in
    M.fold add env M.empty

  let mapfold f env acc =
    let add k v (env, acc) =
      let (k, v), acc = f (k, v) acc in
      M.add k v env, acc
    in
    M.fold add env (M.empty, acc)

  let mapfold_elems f env acc =
    let add k v (env, acc) =
      let v, acc = f v acc in
      M.add k v env, acc
    in
    M.fold add env (M.empty, acc)

  let cardinal env = M.fold (fun _ _ n -> n + 1) env 0

  open Format

  let print
        ?(key_val_sep = Warp.Print.pp_comma)
        ?(binding_sep = Warp.Print.pp_breakable_space)
        print_val
        fmt env =
    let size = cardinal env in
    fprintf fmt "@[";
    ignore
      (M.fold
         (fun k v n ->
           fprintf fmt "@[%a%a%a@]"
             print k
             key_val_sep ()
             print_val v;
           if n < size then binding_sep fmt ();
           n + 1)
         env
         1);
    fprintf fmt "@]"
end
