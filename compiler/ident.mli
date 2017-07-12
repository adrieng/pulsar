(** Identifiers with unique identity *)
type t

(** Every ident is associated with exactly one context *)
type ctx

(** Create a new context with no associated ident *)
val make_ctx : unit -> ctx

(** Get the current ambient context *)
val get_current_ctx : unit -> ctx

(** Set the current ambient context *)
val set_current_ctx : ctx -> unit

(** Create a new context and set it as the current ambient context *)
val reset_ctx : unit -> unit

(** Create a fresh identifier coming from a source file with the given name in
    the ambient context *)
val make_source : string -> t

(** Create a fresh identifier for internal compiler use with the given name in
    the ambient context *)
val make_internal : string -> t

(** Create a fresh identifier by appending the given prefix to the name of an
    already existing one *)
val make_suffix : t -> string -> t

(** Create a fresh identifier by appending the given prefix to the name of an
    already existing one *)
val make_prefix : string -> t -> t

(** Create a fresh identifier with the same name and source/internal status as
    an existing one *)
val refresh : t -> t

(** Comparison function for idents a la [Pervasives.compare] *)
val compare : t -> t -> int

(** Hashing function for identifieirs a la [Pervasives.hash] *)
val hash : t -> int

(** Equality testing for idents; fast *)
val equal : t -> t -> bool

(** Extended ident-indexed maps *)
module Env :
  sig
    include Map.S with type key = t
    val of_assoc_list : (key * 'a) list -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val print :
      ?key_val_sep:Warp.Utils.unit_fmt ->
      ?binding_sep:Warp.Utils.unit_fmt ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit
    val mapfold :
      (key * 'a -> 'b -> (key * 'a) * 'b) ->
      'a t ->
      'b ->
      'a t * 'b
    val mapfold_elems :
      ('a -> 'b -> 'a * 'b) ->
      'a t ->
      'b ->
      'a t * 'b
  end

(** Sets of idents with printing *)
module Set :
  sig
    include Set.S with type elt = t
    val print : Format.formatter -> t -> unit
  end

(** Pretty-print an identifier. Guaranteed to be injective for all the
    idents created in the same context. *)
val print : Format.formatter -> t -> unit

(** Turn an identifier into a concrete string. Guaranteed to be injective for
    all the idents created in the same context. *)
val to_string : t -> string
