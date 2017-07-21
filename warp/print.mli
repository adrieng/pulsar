val utf8_output : bool ref

type 'a printer = Format.formatter -> 'a -> unit

val string_of : 'a printer -> 'a -> string

val pp_nothing : 'a printer

val pp_space : unit printer

val pp_break : unit printer

val pp_breakable_space : unit printer

val pp_strong_break : unit printer

val pp_comma : unit printer

val pp_semicolon : unit printer

val pp_times : unit printer

val pp_arrow : unit printer

val pp_thick_arrow : unit printer

val pp_circledast : unit printer

val pp_lambda : unit printer

val pp_omega : unit printer

val pp_bool : bool printer

val pp_int : int printer

val pp_string : string printer

val pp_pair :
  ?pp_sep:unit printer ->
  'a printer ->
  'b printer ->
  ('a * 'b) printer

val pp_opt :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  'a printer ->
  'a option printer

val pp_list :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a list printer

val pp_array :
  ?pp_left:unit printer ->
  ?pp_right:unit printer ->
  ?pp_sep:unit printer ->
  'a printer ->
  'a array printer
