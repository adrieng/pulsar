type t =
  | Clock of Clock.Periodic.t
  | On of t * t

let rec print fmt ck =
  match ck with
  | Clock p ->
    Clock.Periodic.print fmt p
  | On (ck1, ck2) ->
    Format.fprintf fmt "@[%a@ %a %a@]"
      print ck1
      Pp.print_mod ()
      print ck2

let rec normalize ck =
  match ck with
  | Clock p ->
    p
  | On (ck1, ck2) ->
    Clock.Periodic.on (normalize ck1) (normalize ck2)

let compare ck1 ck2 =
  Clock.Periodic.compare (normalize ck1) (normalize ck2)

let is_unit ck =
  Clock.Periodic.is_one (normalize ck)

let unit =
  Clock Clock.Periodic.one

let on ck1 ck2 =
  On (ck1, ck2)

let make p =
  Clock p
