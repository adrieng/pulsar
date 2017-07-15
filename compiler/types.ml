(* Base types *)

type bty =
  | Unit
  | Bool
  | Char
  | Int
  | Float

let string_of_base_ty bty =
  match bty with
  | Unit -> "unit"
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"

let print_base_ty fmt bty =
  Format.fprintf fmt "%s" (string_of_base_ty bty)

let rec compare_bty bty1 bty2 =
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

let equal_bty bty1 bty2 = compare_bty bty1 bty2 = 0

(* Types *)

type ty =
  | Base of bty
  | Stream of bty
  | Prod of ty * ty
  | Fun of ty * ty
  | Warped of Warp_type.t * ty

let rec print_ty fmt ty =
  let in_pp_box f fmt x =
    Format.fprintf fmt "@[%a@]" f x
  in

  let in_pp_par f fmt x =
    Format.fprintf fmt "(%a)" (in_pp_box f) x
  in

  let rec print_prod_ty fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print_ty fmt ty
    | Prod (ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        print_prod_ty ty1
        Pp.print_times ()
        print_prod_ty ty2
    | Fun _ ->
      in_pp_par print_fun_ty fmt ty
    | Warped _ ->
      in_pp_box print_box_ty fmt ty

  and print_fun_ty fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print_ty fmt ty
    | Prod _ ->
      in_pp_box print_prod_ty fmt ty
    | Fun ((Base _ | Stream _ | Warped _ | Prod _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_box print_ty) ty1
        Pp.print_arr ()
        print_fun_ty ty2
    | Fun ((Fun _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_par print_fun_ty) ty1
        Pp.print_arr ()
        print_fun_ty ty2
    | Warped _ ->
      in_pp_box print_box_ty fmt ty

  and print_box_ty fmt ty =
    match ty with
    | Base _ | Stream _ ->
      print_ty fmt ty
    | Prod _ ->
      in_pp_par print_prod_ty fmt ty
    | Fun _ ->
      in_pp_par print_fun_ty fmt ty
    | Warped (ck, ty) ->
      Format.fprintf fmt "%a@ %a %a"
        Warp_type.print ck
        Pp.print_mod ()
        print_box_ty ty
  in
  match ty with
  | Base bty ->
     print_base_ty fmt bty
  | Stream bty ->
    Format.fprintf fmt "stream %a" print_base_ty bty
  | Prod _ ->
    in_pp_box print_prod_ty fmt ty
  | Fun _ ->
    in_pp_box print_fun_ty fmt ty
  | Warped _ ->
    in_pp_box print_box_ty fmt ty

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

let rec compare_ty ty1 ty2 =
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
       compare_bty bty1 bty2
    | Stream bty1, Stream bty2 ->
      compare_bty bty1 bty2
    | Prod (ty1, ty2), Prod (ty1', ty2')
    | Fun (ty1, ty2), Fun (ty1', ty2') ->
      Warp.Utils.compare_both
        (compare_ty ty1 ty1')
        (fun () -> compare_ty ty2 ty2')
    | Warped (ck, ty), Warped (ck', ty') ->
      Warp.Utils.compare_both
        (Warp_type.compare ck ck')
        (fun () -> compare_ty ty ty')
    | (Base _ | Stream _ | Prod _ | Fun _ | Warped _), _ ->
      Warp.Utils.compare_int (tag_to_int ty1) (tag_to_int ty2)

let equal_ty ty1 ty2 = compare_ty ty1 ty2 = 0

let equiv_ty ty1 ty2 = equal_ty (normalize ty1) (normalize ty2)
