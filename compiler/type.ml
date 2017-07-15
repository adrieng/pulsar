(* Base types *)

type base =
  | Unit
  | Bool
  | Char
  | Int
  | Float

let string_of_base bty =
  match bty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"

let print_base fmt bty =
  Format.fprintf fmt "%s" (string_of_base bty)

let rec compare_base bty1 bty2 =
  if bty1 == bty2 then 0
  else
    let tag_to_int bty =
      match bty with
      | Unit -> 0
      | Bool -> 1
      | Char -> 2
      | Int -> 3
      | Float -> 4
    in
    match bty1, bty2 with
    | Unit, Unit | Bool, Bool | Char, Char | Int, Int | Float, Float ->
      0
    | (Unit | Bool | Char | Int | Float), _ ->
      Warp.Utils.compare_int (tag_to_int bty1) (tag_to_int bty2)

let equal_base bty1 bty2 =
  compare_base bty1 bty2 = 0

(* Types *)

type t =
  | Base of base
  | Stream of base
  | Prod of t * t
  | Fun of t * t
  | Warped of Warp_type.t * t

let rec print fmt ty =
  let in_pp_box f fmt x =
    Format.fprintf fmt "@[%a@]" f x
  in

  let in_pp_par f fmt x =
    Format.fprintf fmt "(%a)" (in_pp_box f) x
  in

  let rec print_prod fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print fmt ty
    | Prod (ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        print_prod ty1
        Pp.print_times ()
        print_prod ty2
    | Fun _ ->
      in_pp_par print_fun fmt ty
    | Warped _ ->
      in_pp_box print_box fmt ty

  and print_fun fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print fmt ty
    | Prod _ ->
      in_pp_box print_prod fmt ty
    | Fun ((Base _ | Stream _ | Warped _ | Prod _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_box print) ty1
        Pp.print_arr ()
        print_fun ty2
    | Fun ((Fun _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_par print_fun) ty1
        Pp.print_arr ()
        print_fun ty2
    | Warped _ ->
      in_pp_box print_box fmt ty

  and print_box fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print fmt ty
    | Prod _ ->
      in_pp_par print_prod fmt ty
    | Fun _ ->
      in_pp_par print_fun fmt ty
    | Warped (ck, ty) ->
      Format.fprintf fmt "%a@ %a %a"
        Warp_type.print ck
        Pp.print_mod ()
        print_box ty
  in
  match ty with
  | Base bty ->
     print_base fmt bty
  | Stream bty ->
    Format.fprintf fmt "stream %a" print_base bty
  | Prod _ ->
    in_pp_box print_prod fmt ty
  | Fun _ ->
    in_pp_box print_fun fmt ty
  | Warped _ ->
    in_pp_box print_box fmt ty

let rec normalize ty =
  let box p ty =
    if Warp_type.is_unit p then ty else Warped (p, ty)
  in

  let rec push p ty =
    match ty with
    | Base _ | Stream _ ->
      box p ty                  (* FIXME *)
    | Prod (ty1, ty2) ->
      Prod (push p ty1, push p ty2)
    | Fun (ty1, ty2) ->
      box p (Fun (normalize ty1, normalize ty2))
    | Warped (p', ty) ->
      push (Warp_type.on p p') ty
  in

  push Warp_type.unit ty

let rec compare ty1 ty2 =
  if ty1 == ty2 then 0
  else
    let tag_to_int ty =
      match ty with
      | Base _ -> 0
      | Stream _ -> 1
      | Prod _ -> 2
      | Fun _ -> 3
      | Warped _ -> 4
    in
    match ty1, ty2 with
    | Base bty1, Base bty2 ->
       compare_base bty1 bty2
    | Stream bty1, Stream bty2 ->
      compare_base bty1 bty2
    | Prod (ty1, ty2), Prod (ty1', ty2')
    | Fun (ty1, ty2), Fun (ty1', ty2') ->
      Warp.Utils.compare_both
        (compare ty1 ty1')
        (fun () -> compare ty2 ty2')
    | Warped (ck, ty), Warped (ck', ty') ->
      Warp.Utils.compare_both
        (Warp_type.compare ck ck')
        (fun () -> compare ty ty')
    | (Base _ | Stream _ | Prod _ | Fun _ | Warped _), _ ->
      Warp.Utils.compare_int (tag_to_int ty1) (tag_to_int ty2)

let equal ty1 ty2 = compare ty1 ty2 = 0

let equiv ty1 ty2 = equal (normalize ty1) (normalize ty2)