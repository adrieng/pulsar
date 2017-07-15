let use_utf8 =
  ref true

let print_times fmt () =
  if !use_utf8
  then Format.fprintf fmt "\xC3\x97"
  else Format.fprintf fmt "*"

let print_arr fmt () =
  if !use_utf8
  then Format.fprintf fmt "\xE2\x86\x92"
  else Format.fprintf fmt "->"

let print_warr fmt () =
  if !use_utf8
  then Format.fprintf fmt "\xE2\x87\x92"
  else Format.fprintf fmt "=>"

let print_mod fmt () =
  if !use_utf8
  then Format.fprintf fmt "\xE2\x8A\x9B"
  else Format.fprintf fmt "<*>"

let print_lam fmt () =
  if !use_utf8
  then Format.fprintf fmt "\xCE\xBB"
  else Format.fprintf fmt "\\"
