type invertible =
  | Wrap
  | Unwrap
  | Concat of Warp_type.t * Warp_type.t
  | Decat of Warp_type.t * Warp_type.t
  | Dist
  | Fact
  | Infl
  | Defl

type t =
  | Id
  | Seq of t * t
  | Arr of t * t
  | Prod of t * t
  | Warped of Warp_type.t * t
  | Invertible of invertible
  | Delay of Warp_type.t * Warp_type.t

let compare_invertible i1 i2 =
  let tag_to_int i =
    match i with
    | Wrap -> 0
    | Unwrap -> 1
    | Concat _ -> 2
    | Decat _ -> 3
    | Dist -> 4
    | Fact -> 5
    | Infl -> 6
    | Defl -> 7
  in
  match i1, i2 with
  | Wrap, Wrap
  | Unwrap, Unwrap
  | Dist, Dist
  | Fact, Fact
  | Infl, Infl
  | Defl, Defl
    ->
     0
  | Concat (p1, q1), Concat (p2, q2)
  | Decat (p1, q1), Decat (p2, q2)
    ->
     Warp.Utils.compare_both
       (Warp_type.compare p1 p2)
       (fun () -> Warp_type.compare q1 q2)
  | (Wrap | Unwrap | Concat _ | Decat _ | Dist | Fact | Infl | Defl), _ ->
     Warp.Utils.compare_int (tag_to_int i1) (tag_to_int i2)

let equal_invertible i1 i2 =
  0 = compare_invertible i1 i2

let print_invertible fmt i =
  match i with
  | Wrap ->
     Format.fprintf fmt "wrap"
  | Unwrap ->
     Format.fprintf fmt "unwrap"
  | Concat (p, q) ->
     Format.fprintf fmt "@[concat@ %a@ %a@]"
       Warp_type.print p
       Warp_type.print q
  | Decat (p, q) ->
     Format.fprintf fmt "@[decat@ %a@ %a@]"
       Warp_type.print p
       Warp_type.print q
  | Dist ->
     Format.fprintf fmt "dist"
  | Fact ->
     Format.fprintf fmt "fact"
  | Infl ->
     Format.fprintf fmt "infl"
  | Defl ->
     Format.fprintf fmt "defl"

let compare c1 c2 =
  let tag_to_int c =
    match c with
    | Id -> 0
    | Seq _ -> 1
    | Arr _ -> 2
    | Prod _ -> 3
    | Warped _ -> 4
    | Invertible _ -> 5
    | Delay _ -> 6
  in
  match c1, c2 with
  | Id, Id ->
     0
  | Seq (c1_1, c1_2), Seq (c2_1, c2_2)
  | Arr (c1_1, c1_2), Arr (c2_1, c2_2)
  | Prod (c1_1, c1_2), Prod (c2_1, c2_2)
    ->
     Warp.Utils.compare_both
       (compare c1_1 c2_1)
       (fun () -> compare c1_2 c2_2)
  | Warped (p1, c1), Warped (p2, c2) ->
     Warp.Utils.compare_both
       (Warp_type.compare p1 p2)
       (fun () -> compare c1 c2)
  | Invertible i1, Invertible i2 ->
     compare_invertible i1 i2
  | Delay (p1, q1), Delay (p2, q2) ->
     Warp.Utils.compare_both
       (Warp_type.compare p1 p2)
       (fun () -> Warp_type.compare q1 q2)
  | (Id | Seq _ | Arr _ | Prod _ | Warped _ | Invertible _ | Delay _), _ ->
     Warp.Utils.compare_int (tag_to_int c1) (tag_to_int c2)

let equal c1 c2 =
  compare c1 c2 = 0

type ctx =
  {
    in_prod : bool;
    in_arr : bool;
  }

let rec print ctx fmt c =
  match c with
  | Id ->
     Format.fprintf fmt "id"
  | Seq (c1, c2) ->
     Format.fprintf fmt "@[%a;@ %a@]"
       (print ctx) c1
       (print ctx) c2
  | Arr (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       (print ctx) c1
       Pp.print_arr ()
       (print ctx) c2
  | Prod (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       (print ctx) c1
       Pp.print_times ()
       (print ctx) c2
  | Warped (p, c) ->
     Format.fprintf fmt "@[%a@ %a %a@]"
       Warp_type.print p
       Pp.print_mod ()
       (print ctx) c
  | Invertible i ->
     print_invertible fmt i
  | Delay (p, q) ->
     Format.fprintf fmt "@[delay %a %a@]"
       Warp_type.print p
       Warp_type.print q

let print =
  print { in_prod = false; in_arr = false; }
