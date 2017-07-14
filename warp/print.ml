open Format

type 'a printer = formatter -> 'a -> unit

let pp_nothing _ _ =
  ()

let pp_break fmt () =
  Format.fprintf fmt "@,"

let pp_breakable_space fmt () =
  Format.fprintf fmt "@ "

let pp_comma fmt () =
  Format.fprintf fmt ",@ "

let pp_bool fmt b =
  Format.fprintf fmt "%b" b

let pp_int fmt i =
  Format.fprintf fmt "%d" i

let pp_string fmt s =
  Format.fprintf fmt "%s" s

let pp_pair
      ?(pp_sep = pp_comma)
      pp_elt_l
      pp_elt_r
      fmt
      (l, r) =
  Format.fprintf fmt "%a%a%a"
    pp_elt_l l
    pp_sep ()
    pp_elt_r r

let pp_opt
      ?(pp_left = pp_nothing)
      ?(pp_right = pp_nothing)
      pp_elt
      fmt
      o =
  match o with
  | None ->
     ()
  | Some x ->
     Format.fprintf fmt "%a%a%a"
       pp_left ()
       pp_elt x
       pp_right ()

let pp_list
      ?(pp_left = pp_nothing)
      ?(pp_right = pp_nothing)
      ?(pp_sep = pp_comma)
      pp_elt
      fmt
      xs =
  let rec loop xs =
    match xs with
    | [] ->
       ()
    | x :: xs ->
       pp_sep fmt ();
       pp_elt fmt x;
       loop xs
  in
  match xs with
  | [] ->
     ()
  | x :: xs ->
     pp_left fmt ();
     pp_elt fmt x;
     loop xs;
     pp_right fmt ()

let pp_array
      ?(pp_left = pp_nothing)
      ?(pp_right = pp_nothing)
      ?(pp_sep = pp_comma)
      pp_elt
      fmt
      arr =
  let n = Array.length arr in
  let rec loop i =
    if i < n
    then
      begin
        pp_sep fmt ();
        pp_elt fmt arr.(i);
        loop (i + 1)
      end
  in
  if n > 0
  then
    begin
      pp_left fmt ();
      pp_elt fmt arr.(0);
      loop 1;
      pp_right fmt ()
    end
