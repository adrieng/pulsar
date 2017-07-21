type extremal =
  | Zero
  | Omega

type period =
  | Ext of extremal
  | Pat of Word.t

type t =
  private
    {
      u : Word.t;
      v : period;
    }

include Warp_sig.S with type t := t

val extremal : ?prefix:Word.t -> extremal -> t

val pattern : ?prefix:Word.t -> ppattern:Word.t -> t
