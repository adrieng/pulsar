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
      Warp.Print.pp_circledast ()
      print ck2

let print fmt ck =
  match ck with
  | On _ ->
     Format.fprintf fmt "(@[%a@])"
       print ck
  | _ ->
     print fmt ck

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

let equal ck1 ck2 =
  Warp.Periodic.equal (normalize ck1) (normalize ck2)

let unit =
  Warp Warp.Periodic.one

let make p =
  Warp p

let one =
  Warp Warp.Periodic.one

let omega =
  Warp Warp.Periodic.omega

let zero_one =
  let prefix = Warp.Word.singleton 0 in
  let ppattern = Warp.Word.singleton 1 in
  Warp (Warp.Periodic.make_pattern ~prefix ~ppattern)

let on ck1 ck2 =
  let res = On (ck1, ck2) in
  if equal ck1 one || equal ck1 omega || equal ck2 one || equal ck2 omega
  then Warp (normalize res)
  else res

let ( <= ) p q =
  Warp.Periodic.(normalize p <= normalize q)

let div p q =
  Warp (Warp.Periodic.div (normalize p) (normalize q))
