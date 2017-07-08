(** A (column, line) position in a source file *)
type pos

(** Create a position; [lnum] and [cnum] should be non-negative *)
val make_pos : lnum:int -> cnum:int -> pos

(** Pretty-print a position *)
val print_pos : Format.formatter -> pos -> unit

(** Lexicographic ordering on positions *)
val (<=) : pos -> pos -> bool

(** Return the minimum position in the lexicographic order *)
val min : pos -> pos -> pos

(** Return the maximum position in the lexicographic order *)
val max : pos -> pos -> pos

(** Extract the line number of a position *)
val lnum : pos -> int

(** Extract the column number of a position *)
val cnum : pos -> int

(** Convert from Lexing.pos to our positions *)
val pos_of_lexing_pos : Lexing.position -> pos

(** A source location, that is a range between two position *)
type loc

(** Create a location in a source file; raises Invalid_arg if [fn] is empty *)
val make_loc : fn:string -> start:pos -> stop:pos -> loc

(** Create a location from a pair of Lexing.position; raises Invalid_arg if the
    positions are not in the same file *)
val loc_of_lexing_pos_pair :
  start:Lexing.position -> stop:Lexing.position -> loc

(** Pretty-print a location *)
val print_loc : Format.formatter -> loc -> unit

(** Pretty-print a location, assuming both start and end are on the same line;
    raise Invalid_arg otherwise *)
val print_loc_sameline : Format.formatter -> loc -> unit

(** The fictional location *)
val nowhere : loc

(** computes the smallest location [l] s.t. [l1] and [l2] are both included in
    [l]; raises Invalid_arg if [l1] and [l2] belong to distinct files *)
val join : loc -> loc -> loc

(** A thing together with its location *)
type 'a located =
  {
    contents : 'a;
    loc : loc;
  }

(** Pretty-print a thing using the provided pretty-printing function. Display
    its location only if called with [disp_loc] set to true. *)
val print_located :
  (Format.formatter -> 'a -> unit) ->
  ?disp_loc : bool ->
  Format.formatter -> 'a located -> unit
