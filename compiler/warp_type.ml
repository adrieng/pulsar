type t =
  | Warp of Warp.Periodic.t
  | On of t * t

let rec print fmt ck =
  match ck with
  | Warp p ->
    Warp.Periodic.print fmt p
  | On (ck1, ck2) ->
    Format.fprintf fmt "@[%a@ %a %a@]"
      print ck1
      Pp.print_mod ()
      print ck2

let rec normalize ck =
  match ck with
  | Warp p ->
    p
  | On (ck1, ck2) ->
    Warp.Periodic.on (normalize ck1) (normalize ck2)

let compare ck1 ck2 =
  Warp.Periodic.compare (normalize ck1) (normalize ck2)

let is_unit ck =
  Warp.Periodic.is_one (normalize ck)

let unit =
  Warp Warp.Periodic.one

let on ck1 ck2 =
  On (ck1, ck2)

let make p =
  Warp p
