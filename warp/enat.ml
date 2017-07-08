type t =
  | Inf
  | Fin of int

let print_utf8 =
  ref true

let print fmt ii =
  match ii with
  | Inf ->
     if !print_utf8
     then Format.fprintf fmt "\xCF\x89"
     else Format.fprintf fmt "inf"
  | Fin i ->
     Format.fprintf fmt "%d" i

let compare (ii1 : t) (ii2 : t) =
  Pervasives.compare ii1 ii2

let equal ii1 ii2 =
  match ii1, ii2 with
  | Fin i1, Fin i2 ->
     i1 = i2
  | Inf, Inf ->
     true
  | (Inf | Fin _), _ ->
     false

let zero =
  Fin 0

let one =
  Fin 1

let succ ii =
  match ii with
  | Fin n ->
     Fin (n + 1)
  | Inf ->
     Inf

let ( <= ) ii1 ii2 =
  match ii1, ii2 with
  | Fin ii1, Fin ii2 ->
     ii1 <= ii2
  | Inf, Fin _ ->
     false
  | (Fin _ | Inf), Inf ->
     true

let ( + ) ii1 ii2 =
  match ii1, ii2 with
  | Fin ii1, Fin ii2 ->
     Fin (ii1 + ii2)
  | Inf, _ | _, Inf ->
     Inf

let ( - ) ii1 ii2 =
  match ii1, ii2 with
  | Fin ii1, Fin ii2 ->
     Fin (ii1 - ii2)
  | Inf, Fin _ ->
     Inf
  | (Fin _ | Inf), Inf ->
     invalid_arg "Ninf.(-)"

let ( * ) ii1 ii2 =
  match ii1, ii2 with
  | Fin ii1, Fin ii2 ->
     Fin (ii1 * ii2)
  | Inf, _ | _, Inf ->
     Inf

let lift name f ii1 ii2 =
  match ii1, ii2 with
  | Fin ii1, Fin ii2 ->
     Fin (f ii1 ii2)
  | Inf, _ | _, Inf ->
     invalid_arg ("Ninf." ^ name)

let ( / ) =
  lift "(/)" ( / )

let min ii1 ii2 =
  match ii1, ii2 with
  | _, Inf ->
     ii1
  | Inf, _ ->
     ii2
  | Fin i1, Fin i2 ->
     Fin (min i1 i2)

let max ii1 ii2 =
  match ii1, ii2 with
  | _, Inf | Inf, _ ->
     Inf
  | Fin i1, Fin i2 ->
     Fin (max i1 i2)

let of_int i =
  Fin i

exception Too_big

let to_int ii =
  match ii with
  | Fin i ->
     i
  | Inf ->
     raise Too_big
