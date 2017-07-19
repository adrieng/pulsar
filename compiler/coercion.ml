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

let priority c =
  match c with
  | Id | Invertible _ | Delay _ ->
     0
  | Warped _ ->
     10
  | Arr _ ->
     20
  | Prod _ ->
     30
  | Seq _ ->
     40

let rec print pri fmt c =
  let pri' = priority c in
  let print = print pri' in
  let paren = pri < pri' in
  if paren then Format.fprintf fmt "(@[";
  begin match c with
  | Id ->
     Format.fprintf fmt "id"
  | Seq (c1, c2) ->
     Format.fprintf fmt "@[%a;@ %a@]"
       print c1
       print c2
  | Arr (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       (print_under_arr pri) c1
       Pp.print_arr ()
       print c2
  | Prod (c1, c2) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       print c1
       Pp.print_times ()
       print c2
  | Warped (p, c) ->
     Format.fprintf fmt "@[%a@ %a (@[<hov>%a@])@]"
       Warp_type.print p
       Pp.print_mod ()
       print c
  | Invertible i ->
     print_invertible fmt i
  | Delay (p, q) ->
     Format.fprintf fmt "@[delay %a %a@]"
       Warp_type.print p
       Warp_type.print q
  end;
  if paren then Format.fprintf fmt "@])"

and print_under_arr pri fmt c =
  match c with
  | Arr _ ->
     Format.fprintf fmt "(@[%a@])"
       (print pri) c
  | _ ->
     print pri fmt c

let print =
  print 500

let invert i =
  match i with
  | Wrap ->
     Unwrap
  | Unwrap ->
     Wrap
  | Concat (p, q) ->
     Decat (p, q)
  | Decat (p, q) ->
     Concat (p, q)
  | Dist ->
     Fact
  | Fact ->
     Dist
  | Infl ->
     Defl
  | Defl ->
     Infl

exception Ill_typed

let get_base ty =
  match ty with
  | Type.Base bty ->
     bty
  | _ ->
     raise Ill_typed

let get_fun ty =
  match ty with
  | Type.Fun (ty1, ty2) ->
     ty1, ty2
  | _ ->
     raise Ill_typed

let get_prod ty =
  match ty with
  | Type.Prod (ty1, ty2) ->
     ty1, ty2
  | _ ->
     raise Ill_typed

let get_warped ty =
  match ty with
  | Type.Warped (p, ty) ->
     p, ty
  | _ ->
     raise Ill_typed

let get_warped_check q ty =
  let p, ty = get_warped ty in
  if not (Warp_type.equal p q) then raise Ill_typed;
  ty

(* Typing *)

let output_type_invertible i ty =
  match i with
  | Wrap ->
     Type.Warped (Warp_type.one, ty)
  | Unwrap ->
     get_warped_check Warp_type.one ty
  | Concat (p, q) ->
     let ty = get_warped_check p ty in
     let ty = get_warped_check q ty in
     Type.Warped (Warp_type.on p q, ty)
  | Decat (p, q) ->
     let ty = get_warped_check (Warp_type.on p q) ty in
     Type.Warped (p, Type.Warped (q, ty))
  | Dist ->
     let p, ty = get_warped ty in
     let ty1, ty2 = get_prod ty in
     Type.(Prod (Warped (p, ty1), Warped (p, ty2)))
  | Fact ->
     let ty1, ty2 = get_prod ty in
     let p, ty1 = get_warped ty1 in
     let q, ty2 = get_warped ty2 in
     Type.(Prod (Warped (p, ty1), Warped (p, ty2)))
  | Infl ->
     ignore @@ get_base ty;
     Type.Warped (Warp_type.omega, ty)
  | Defl ->
     let ty = get_warped_check Warp_type.omega ty in
     ignore @@ get_base ty;
     ty

let rec output_type c ty =
  match c with
  | Id ->
     ty
  | Seq (c1, c2) ->
     output_type c2 (output_type c1 ty)
  | Arr (c1, c2) ->
     let ty1', ty2 = get_fun ty in
     Type.Fun (input_type c1 ty1', output_type c2 ty2)
  | Prod (c1, c2) ->
     let ty1, ty2 = get_prod ty in
     Type.Prod (output_type c1 ty1, output_type c2 ty2)
  | Warped (p, c) ->
     let ty = get_warped_check p ty in
     let ty = output_type c ty in
     Type.Warped (p, ty)
  | Invertible i ->
     output_type_invertible i ty
  | Delay (p, q) ->
     let ty = get_warped_check p ty in
     if not Warp_type.(q <= p) then raise Ill_typed;
     Type.Warped (q, ty)

and input_type c ty =
  match c with
  | Id ->
     ty
  | Seq (c1, c2) ->
     input_type c1 (input_type c2 ty)
  | Arr (c1, c2) ->
     let ty1, ty2' = get_fun ty in
     Type.Fun (output_type c1 ty1, input_type c2 ty2')
  | Prod (c1, c2) ->
     let ty1', ty2' = get_prod ty in
     Type.Prod (input_type c1 ty1', input_type c2 ty2')
  | Warped (p, c) ->
     let ty = get_warped_check p ty in
     let ty = input_type c ty in
     Type.Warped (p, ty)
  | Invertible i ->
     output_type_invertible (invert i) ty
  | Delay (p, q) ->
     let ty = get_warped_check q ty in
     if not Warp_type.(q <= p) then raise Ill_typed;
     Type.Warped (p, ty)

(* Equational theory *)

let rec seq (c1, c2) =
  match c1, c2 with
  | Seq (c1, c2), c3 ->
     begin match seq (c2, c3) with
     | Seq (c2, c3) ->
        Seq (seq (c1, c2), c3)
     | c2 ->
        seq (c1, c2)
     end

  | c1, Seq (c2, c3) ->
     begin match seq (c1, c2) with
     | Seq (c1, c2) ->
        Seq (seq (c1, c2), c3)
     | c1 ->
        seq (c1, c3)
     end

  | Id, _ ->
     c2

  | _, Id ->
     c1

  | Prod (c11, c12), Prod (c21, c22) ->
     prod (seq (c11, c21), seq (c12, c22))

  | Arr (c11, c12), Arr (c21, c22) ->
     arr (seq (c21, c11), seq (c12, c22))

  | Invertible i, Invertible i' when equal_invertible i (invert i') ->
     Id

  | _ ->
     Seq (c1, c2)

and seqs cs =
  match cs with
  | [] ->
     Id
  | [c] ->
     c
  | c :: cs ->
     seq (c, seqs cs)

and arr (c1, c2) =
  match c1, c2 with
  | Id, Id ->
     Id

  | _ ->
     Arr (c1, c2)

and prod (c1, c2) =
  match c1, c2 with
  | Id, Id ->
     Id

  | _ ->
     Prod (c1, c2)

and warped (p, c) =
  let open Warp_type in
  match c with
  | Id ->
     Id

  | Seq (c1, c2) ->
     seq (warped (p, c1), warped (p, c2))

  | Prod (c1, c2) ->
     seqs
       [
         Invertible Dist;
         prod (warped (p, c1), warped (p, c2));
         Invertible Fact;
       ]

  | Invertible Wrap ->
     Invertible (Decat (p, one))

  | Invertible Unwrap ->
     Invertible (Concat (p, one))

  | Invertible Defl ->
     seqs
       [
         Invertible (Concat (p, omega));
         Delay (on p omega, p);
       ]

  | Delay (q, r) ->
     seqs
       [
         Invertible (Concat (p, q));
         Delay (on p q, on p r);
         Invertible (Decat (p, r));
       ]

  | _ ->
     Warped (p, c)

and delay (p, q) =
  if Warp_type.equal p q
  then Id
  else Delay (p, q)

let rec reduce c =
  match c with
  | Id ->
     Id
  | Seq (c1, c2) ->
     seq (reduce c1, reduce c2)
  | Arr (c1, c2) ->
     arr (reduce c1, reduce c2)
  | Prod (c1, c2) ->
     prod (reduce c1, reduce c2)
  | Warped (p, c) ->
     warped (p, reduce c)
  | Invertible i ->
     Invertible i
  | Delay (p, q) ->
     delay (p, q)
