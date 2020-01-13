(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017 Adrien Guatto
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the LICENSE file in the top-level directory.
 *)

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

type ('a, 'b) sum = Left of 'a | Right of 'b

module Zero =
  ((struct type t = unit end) : (sig type t end))

type zero = Zero.t

let zero_elim (_ : zero) = assert false

type cmp = LT | GT | EQ

let comp x y = if x < y then LT else if x > y then GT else EQ

let zclamp x = max 0 x

let rec index x l =
  match l with
  | [] -> raise Not_found
  | y :: l -> if x = y then 0 else 1 + index x l

let repeat n x =
  let rec walk n l =
    if n = 0 then l else walk (n - 1) (x :: l)
  in
  walk n []

let range n m =
  assert (n <= m);
  let rec walk acc i =
    if i > m then List.rev acc else walk (i :: acc) (i + 1)
  in
  walk [] n

let rec for_all_range f n m =
  (n > m) || (f n && for_all_range f (n + 1) m)

let transpose ll =
  match ll with
  | [] -> []
  | l :: ll ->
    let rec walk acc ll =
      match ll with
      | [] -> acc
      | x_l :: ll -> walk (List.map2 (fun x l -> x :: l) x_l acc) ll
    in
    List.map List.rev (walk (List.map (fun x -> [x]) l) ll)

let rec map3 f l1 l2 l3 =
  match l1, l2, l3 with
  | [], [], [] -> []
  | x1 :: l1, x2 :: l2, x3 :: l3 -> f x1 x2 x3 :: map3 f l1 l2 l3
  | _ -> invalid_arg "map3"

let rec map4 f l1 l2 l3 l4 =
  match l1, l2, l3, l4 with
  | [], [], [], [] ->
    []
  | x1 :: l1, x2 :: l2, x3 :: l3, x4 :: l4 ->
    f x1 x2 x3 x4 :: map4 f l1 l2 l3 l4
  | _ ->
    invalid_arg "map4"

let tailrec_map f l = List.rev (List.rev_map f l)

let rec split_3 l =
  match l with
  | [] -> ([], [], [])
  | (x, y, z) :: l ->
    let xl, yl, zl = split_3 l in
    (x :: xl, y :: yl, z :: zl)

let fold_left_1 f l =
  match l with
  | x :: l -> List.fold_left f x l
  | _ -> invalid_arg "fold_left_1: list too short"

let rec fold_left_3 f acc l1 l2 l3 =
  match l1, l2, l3 with
  | [], [], [] -> acc
  | x1 :: l1, x2 :: l2, x3 :: l3 ->
    let acc = f acc x1 x2 x3 in
    fold_left_3 f acc l1 l2 l3
  | _ ->
    invalid_arg "fold_left3: list too short"

let rec mapfold f l acc =
  match l with
  | [] -> [], acc
  | x :: l ->
    let l, acc = mapfold f l acc in
    let x, acc = f x acc in
    x :: l, acc

let rec mapfold_left f acc l =
  match l with
  | [] -> acc, []
  | x :: l ->
    let acc, x = f acc x in
    let acc, l = mapfold_left f acc l in
    acc, x :: l

let rec mapfold_2 f l1 l2 acc =
  match l1, l2 with
  | [], [] -> [], acc
  | x1 :: l1, x2 :: l2 ->
    let l, acc = mapfold_2 f l1 l2 acc in
    let x, acc = f x1 x2 acc in
    x :: l, acc
  | [], _ :: _ | _ :: _, [] ->
    invalid_arg "mapfold_2"

let rec mapfold_left_2 f acc l1 l2 =
  match l1, l2 with
  | [], [] -> acc, []
  | x1 :: l1, x2 :: l2 ->
    let acc, x = f acc x1 x2 in
    let acc, l = mapfold_left_2 f acc l1 l2 in
    acc, x :: l
  | [], _ :: _ | _ :: _, [] ->
    invalid_arg "mapfold_left_2"

let split_at n l =
  let rec aux n acc l =
    if n = 0 then List.rev acc, l
    else
      match l with
      | [] ->
        invalid_arg "split_at: list too short"
      | x :: l ->
        aux (n - 1) (x :: acc) l
  in
  assert (n >= 0);
  aux n [] l

let iter_opt f x =
  match x with
  | None -> ()
  | Some x -> f x

let map_opt f x =
  match x with
  | None -> None
  | Some x -> Some (f x)

let fold_opt_2 f x y acc =
  match x, y with
  | None, None -> acc
  | Some x, Some y -> f x y
  | None, Some _ | Some _, None -> invalid_arg "fold_opt_2"

let get_opt = function
  | None -> invalid_arg "get_opt: None"
  | Some x -> x

let get_opt_ref r = get_opt !r

let fold_opt f o acc =
  match o with
  | None -> acc
  | Some x -> f x acc

let mapfold_opt f o acc =
  match o with
  | None -> None, acc
  | Some x ->
    let x, acc = f x acc in
    Some x, acc

let get_1 l =
  match l with
  | [] -> invalid_arg "get_1"
  | x :: l -> x, l

let get_single l =
  match l with
  | [x] -> x
  | _ -> invalid_arg "get_single"

(* /!\ watch out aliasing /!\ *)
let map_opt_ref alias f r acc =
  match !r with
  | None -> r, acc
  | Some x ->
    let v, acc = f x acc in
    (if alias then (r := Some v; r) else ref (Some v)), acc

(* let rec znth l n = *)
(*   match l with *)
(*   | [] -> invalid_arg "znth: list too short" *)
(*   | v :: l -> if n = Z.zero then v else znth l (Z.pred n) *)

let rec last l =
  match l with
  | [] -> invalid_arg "last: empty list"
  | [x] -> x
  | _ :: l -> last l

let find_rank ?(eq = (=)) x l =
  let rec walk acc l =
    match l with
    | [] -> raise Not_found
    | y :: l -> if eq x y then acc else walk (acc + 1) l
  in
  walk 0 l

let compare_both c k = if c <> 0 then c else k ()

let compare_string (x : string) (y : string) = Pervasives.compare x y

let compare_int (x : int) (y : int) = Pervasives.compare x y

let compare_bool x y =
  let tag_to_int x =
    match x with
    | true -> 0
    | false -> 1
  in
  match x, y with
  | true, true | false, false -> 0
  | (true | false), _ -> compare_int (tag_to_int x) (tag_to_int y)

let compare_char (x : char) (y : char) = Pervasives.compare x y

let compare_float (x : float) (y : float) = Pervasives.compare x y

let compare_nativeint x y = Pervasives.compare x y

let compare_opt compare x y =
  let tag_of_opt opt =
    match opt with
    | None -> 0
    | Some _ -> 1
  in
  match x, y with
  | None, None -> 0
  | Some x, Some y -> compare x y
  | (None | Some _), _ -> compare_int (tag_of_opt x) (tag_of_opt y)

let rec compare_list compare x_l y_l =
  let list_to_tag l =
    match l with
    | [] -> 0
    | _ :: _ -> 1
  in
  match x_l, y_l with
  | [], [] -> 0
  | x :: x_l, y :: y_l ->
    compare_both (compare x y) (fun () -> compare_list compare x_l y_l)
  | ([] | _ :: _), _ ->
    compare_int (list_to_tag x_l) (list_to_tag y_l)

let rec fold_int i f acc = if i = 0 then acc else f (fold_int (i - 1) f acc)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b = (a * b) / gcd a b

module type ExtMap =
sig
  include Map.S
  val print :
    ?sep:string ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
  exception Non_disjoint of key
  val disjoint_union : 'a t -> 'a t -> 'a t

  (** [merge_biased_left ~winner ~loser] merges [winner] and [loser], preferring
      the bindings in [winner] in case of duplication. *)
  val merge_biased : winner:'a t -> loser:'a t -> 'a t

  val to_list : 'a t -> (key * 'a) list

  val of_list : (key * 'a) list -> 'a t
end

module MakeMap(S : Map.OrderedType) : ExtMap with type key = S.t =
struct
  module M = Map.Make(S)
  include M

  let print ?(sep = ",") print_key print_value fmt map =
    let r = ref (cardinal map) in
    Format.fprintf fmt "@[";
    iter
      (fun k v ->
        decr r;
        Format.fprintf fmt "%a = %a"
          print_key k
          print_value v;
        if !r > 0
        then Format.fprintf fmt "%s@ " sep)
      map;
    Format.fprintf fmt "@]"

  exception Non_disjoint of key

  let disjoint_union m1 m2 =
    let add k v m2 =
      if mem k m2 then raise (Non_disjoint k) else add k v m2
    in
    fold add m1 m2

  let merge_biased ~winner ~loser =
    let select_left _ v1 v2 =
      match v1, v2 with
      | None, _ -> v2
      | _, None -> v1
      | Some _, Some _ -> v1
    in
    merge select_left winner loser

  let of_list l =
    List.fold_left (fun env (id, x) -> M.add id x env) M.empty l

  let to_list env =
    fold (fun k v l -> (k, v) :: l) env []
end

module OrderedString =
struct
  type t = string
  let compare = compare_string
end

module OrderedInt =
struct
  type t = int
  let compare = compare_int
end

module String_set = Set.Make(OrderedString)
module Env = MakeMap(OrderedString)
module Int_set = Set.Make(OrderedInt)
module Int_map = MakeMap(OrderedInt)

let time_call f x =
  let start = Unix.gettimeofday () in
  let r = f x in
  let stop = Unix.gettimeofday () in
  stop -. start, r

let output_to_temp_file name ext f x =
  let out_fn, out = Filename.open_temp_file name ext in
  f out x;
  close_out out;
  out_fn

let print_to_string f x =
  ignore (Format.flush_str_formatter ());
  f Format.str_formatter x;
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

let list_of_option o =
  match o with
  | None -> []
  | Some l -> [l]

let flatten_option_list l =
  let add l o = list_of_option o @ l in
  List.fold_left add [] l

let assert1 l =
  match l with
  | [] -> invalid_arg "assert1: empty list"
  | x :: _ -> x

let assert2 l =
  match l with
  | [] -> invalid_arg "assert2: empty list"
  | [_] -> invalid_arg "assert2: list too short"
  | x :: y :: _ -> x, y

let add_overflow x y =
  let open Int in
  let xor = logxor x y in
  (logor
     xor
     (logxor
        (add
           (logxor x (logand (lognot xor) min_int))
           y)
        y)) >= Int.zero

let flip f x y = f y x

let double_flip f x y =
  let y, x = (flip f x y) in
  x, y

let make_imperative_var init =
  let r = ref init in
  (fun () -> !r), (fun s -> r := s)

let make_gen_sym () =
  let r = ref 0 in
  fun s -> incr r; s ^ string_of_int !r

module Make = ((functor (S : Map.OrderedType) ->
struct
  module M = Map.Make(S)

  type key = int

  module Key_map = Map.Make(struct type t = key let compare x y = x - y end)

  type t = key M.t * S.t Key_map.t

  let make gen col =
    let rec add i col (((to_int, to_t) as table) : t) =
      match gen col with
      | Some (elem, col) ->
        let table = M.add elem i to_int, Key_map.add i elem to_t in
        add (i + 1) col table
      | None ->
        table
    in
    add 0 col (M.empty, Key_map.empty)

  let find_key (to_int, _) elem = M.find elem to_int
  let find_elem (_, to_t) key = Key_map.find key to_t
end)
:
(
functor (S : Map.OrderedType) ->
  sig
    type key
    type t
    val make : ('a -> (S.t * 'a) option) -> 'a -> t
    val find_key : t -> S.t -> key
    val find_elem : t -> key -> S.t
  end
))

module OrderedTuple(T1 : Map.OrderedType)(T2 : Map.OrderedType)
  : Map.OrderedType with type t = T1.t * T2.t =
struct
  type t = T1.t * T2.t
  let compare (x1, y1) (x2, y2) =
    let c = T1.compare x1 x2 in
    if c = 0 then T2.compare y1 y2 else c
end

type 'a bin_tree =
  | Leaf of 'a
  | Node of 'a bin_tree * 'a bin_tree

let rec print_bin_tree print fmt bt =
  match bt with
  | Leaf x -> print fmt x
  | Node (left, right) ->
    Format.fprintf fmt "@[(%a,@ %a)@]"
      (print_bin_tree print) left
      (print_bin_tree print) right

let rec fold_bin_tree_df f acc bt =
  match bt with
  | Leaf x -> f acc x
  | Node (left, right) ->
    fold_bin_tree_df f (fold_bin_tree_df f acc left) right

module MEMOIZE(H : Hashtbl.HashedType) =
struct
  module Ht = Hashtbl.Make(H)

  include Ht

  let memoize f =
    let ht = Ht.create 17 in
    fun x ->
      try Ht.find ht x
      with Not_found ->
        let y = f x in
        Ht.add ht x y;
        y

  let memoize_rec f =
    let ht = Ht.create 17 in
    let rec memoized_f x =
      try Ht.find ht x
      with Not_found ->
        let y = f memoized_f x in
        Ht.add ht x y;
        y
    in
    memoized_f
end

let int_of_string_exp =
  let int_exp_re =
    let int_re = "\\([0-9]+\\)" in
    Printf.sprintf "%s\\(\\.%s[eE][+-]?%s\\)?"
      int_re
      int_re
      int_re
  in
  let re = Str.regexp int_exp_re in
  fun s ->
    ignore (Str.search_forward re s 0);
    let v_s = Str.matched_group 1 s in
    let s =
      try
        let e = int_of_string (Str.matched_group 4 s) in
        let f_s = Str.matched_group 3 s in
        let e_m = min e (String.length f_s) in
        (* check that dropped part only holds 0s *)
        String.iter
          (fun c -> assert (c = '0'))
          (String.sub f_s e_m (String.length f_s - e_m));
        v_s ^ String.sub f_s 0 e_m ^ String.make (e - e_m) '0'
      with Not_found ->
        v_s
    in
    int_of_string s

let hashtable_of_assoc_list l =
  let ht = Hashtbl.create 100 in
  List.iter (fun (x, v) -> Hashtbl.add ht x v) l;
  ht

let is_ascii i = i >= 0 && i < 128

module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end

module type PrintableOrderedType =
sig
  type t
  include PrintableType with type t := t
  include Map.OrderedType with type t := t
end

module PrintableOrderedUnit =
struct
  type t = unit
  let print (_ : Format.formatter) () = ()
  let compare () () = 0
end

module PrintableOrderedString =
struct
  type t = string
  let print fmt s =
    Format.fprintf fmt "%s" s
  let compare (s1 : string) (s2 : string) =
    Pervasives.compare s1 s2
end

let mod_b1 x y = succ (pred x mod y)

let div_b1 x y = pred x / y
