module type S =
  sig
    (** Warps *)
    type t

    include Utils.PrintableOrderedType with type t := t

    val equal : t -> t -> bool

    (** Warp composition *)
    val on : t -> t -> t

    (** Warp division *)
    val div : t -> t -> t

    (** Precedence test *)
    val ( <= ) : t -> t -> bool

    (** The warp represented by (0) *)
    val zero : t

    (** The warp represented by (1) *)
    val one : t

    (** The warp represented by (\omega) *)
    val omega : t

    (** The warp represented by 0(1) *)
    val zero_one : t
  end
