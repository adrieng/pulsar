(* Base types *)

type bty =
  | Bool
  | Char
  | Int
  | Float

let string_of_base_ty bty =
  match bty with
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
      | Bool -> 0
      | Char -> 1
      | Int -> 2
      | Float -> 3
    in
    match bty1, bty2 with
    | Bool, Bool | Char, Char | Int, Int | Float, Float ->
      0
    | (Bool | Char | Int | Float), _ ->
      Clock.Utils.compare_int (tag_to_int bty1) (tag_to_int bty2)

let equal_bty bty1 bty2 = compare_bty bty1 bty2 = 0

(* Types *)

type ty =
  | Unit
  | Stream of bty
  | Prod of ty * ty
  | Fun of ty * ty
  | Box of Clock_type.t * ty

let rec print_ty fmt ty =
  let in_pp_box f fmt x =
    Format.fprintf fmt "@[%a@]" f x
  in

  let in_pp_par f fmt x =
    Format.fprintf fmt "(%a)" (in_pp_box f) x
  in

  let rec print_prod_ty fmt ty =
    match ty with
    | Unit | Stream _ ->
      print_ty fmt ty
    | Prod (ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        print_prod_ty ty1
        Pp.print_times ()
        print_prod_ty ty2
    | Fun _ ->
      in_pp_par print_fun_ty fmt ty
    | Box _ ->
      in_pp_box print_box_ty fmt ty

  and print_fun_ty fmt ty =
    match ty with
    | Unit | Stream _ ->
      print_ty fmt ty
    | Prod _ ->
      in_pp_box print_prod_ty fmt ty
    | Fun ((Unit | Stream _ | Box _ | Prod _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_box print_ty) ty1
        Pp.print_arr ()
        print_fun_ty ty2
    | Fun ((Fun _) as ty1, ty2) ->
      Format.fprintf fmt "%a@ %a %a"
        (in_pp_par print_fun_ty) ty1
        Pp.print_arr ()
        print_fun_ty ty2
    | Box _ ->
      in_pp_box print_box_ty fmt ty

  and print_box_ty fmt ty =
    match ty with
    | Unit | Stream _ ->
      print_ty fmt ty
    | Prod _ ->
      in_pp_par print_prod_ty fmt ty
    | Fun _ ->
      in_pp_par print_fun_ty fmt ty
    | Box (ck, ty) ->
      Format.fprintf fmt "%a@ %a %a"
        Clock_type.print ck
        Pp.print_mod ()
        print_box_ty ty
  in
  match ty with
  | Unit ->
    Format.fprintf fmt "unit"
  | Stream bty ->
    Format.fprintf fmt "stream %a" print_base_ty bty
  | Prod _ ->
    in_pp_box print_prod_ty fmt ty
  | Fun _ ->
    in_pp_box print_fun_ty fmt ty
  | Box _ ->
    in_pp_box print_box_ty fmt ty

let rec normalize ty =
  let box p ty =
    if Clock_type.is_unit p then ty else Box (p, ty)
  in

  let rec push p ty =
    match ty with
    | Unit ->
      Unit
    | Stream _ ->
      box p ty
    | Prod (ty1, ty2) ->
      Prod (push p ty1, push p ty2)
    | Fun (ty1, ty2) ->
      box p (Fun (normalize ty1, normalize ty2))
    | Box (p', ty) ->
      push (Clock_type.on p p') ty
  in

  push Clock_type.unit ty

let rec compare_ty ty1 ty2 =
  if ty1 == ty2 then 0
  else
    let tag_to_int ty =
      match ty with
      | Unit -> 1
      | Stream _ -> 2
      | Prod _ -> 3
      | Fun _ -> 4
      | Box _ -> 5
    in
    match ty1, ty2 with
    | Unit, Unit ->
      0
    | Stream bty1, Stream bty2 ->
      compare_bty bty1 bty2
    | Prod (ty1, ty2), Prod (ty1', ty2')
    | Fun (ty1, ty2), Fun (ty1', ty2') ->
      Clock.Utils.compare_both
        (compare_ty ty1 ty1')
        (fun () -> compare_ty ty2 ty2')
    | Box (ck, ty), Box (ck', ty') ->
      Clock.Utils.compare_both
        (Clock_type.compare ck ck')
        (fun () -> compare_ty ty ty')
    | (Unit | Stream _ | Prod _ | Fun _ | Box _), _ ->
      Clock.Utils.compare_int (tag_to_int ty1) (tag_to_int ty2)

let equal_ty ty1 ty2 = compare_ty ty1 ty2 = 0

let equiv_ty ty1 ty2 = equal_ty (normalize ty1) (normalize ty2)
