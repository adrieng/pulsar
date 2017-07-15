type 'a printer = Format.formatter -> 'a -> unit

val pp_nothing : 'a printer

val pp_break : unit printer

val pp_breakable_space : unit printer

val pp_comma : unit printer

val pp_semicolon : unit printer

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
