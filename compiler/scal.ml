type t =
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float

let print fmt s =
  match s with
  | Bool b -> Format.fprintf fmt "%b" b
  | Char c -> Format.fprintf fmt "'%c'" c
  | Int i -> Format.fprintf fmt "%i" i
  | Float f -> Format.fprintf fmt "%f" f

let compare s1 s2 =
  if s1 == s2 then 0
  else
    let tag_to_int s =
      match s with
      | Bool _ -> 0
      | Char _ -> 1
      | Int _ -> 2
      | Float _ -> 3
    in
    match s1, s2 with
    | Bool b1, Bool b2 ->
      Warp.Utils.compare_bool b1 b2
    | Char c1, Char c2 ->
      Warp.Utils.compare_char c1 c2
    | Int i1, Int i2 ->
      Warp.Utils.compare_int i1 i2
    | Float f1, Float f2 ->
      Warp.Utils.compare_float f1 f2
    | (Bool _ | Char _ | Int _ | Float _), _ ->
      Warp.Utils.compare_int (tag_to_int s1) (tag_to_int s2)

let type_of s =
  match s with
  | Bool _ -> Types.Bool
  | Char _ -> Types.Char
  | Int _ -> Types.Int
  | Float _ -> Types.Float
