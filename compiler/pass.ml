type ctx =
  {
    filename : string;
  }

let make_default ~filename =
  {
    filename;
  }

let current_file ctx =
  ctx.filename

type ('a, 'b) atomic_pass =
  {
    name : string;
    pp_in : 'a Warp.Print.printer;
    pp_out : 'b Warp.Print.printer;
    body : ctx -> 'a -> 'b;
  }

type _ t =
  | Atomic : ('a, 'b) atomic_pass -> ('a -> 'b) t
  | Seq : ('a -> 'b) t * ('b -> 'c) t -> ('a -> 'c) t

let atomic
      ?(pp_in = Warp.Print.pp_nothing)
      ?(pp_out = Warp.Print.pp_nothing)
      ~name
      body =
  Atomic
    {
      name;
      pp_in;
      pp_out;
      body;
    }

let ( >>> ) p1 p2 =
  Seq (p1, p2)

let run_atomic ctx at x =
  if !Options.debug || Options.pass_debug at.name
  then Format.eprintf "(* Running pass %s *)@." at.name;
  let y = at.body ctx x in
  if !Options.debug || Options.pass_debug at.name
  then
    begin
      Format.eprintf "(* Finished running %s *)@." at.name;
      Format.eprintf "%a@." at.pp_out y;
    end;
  y

let run ~ctx p x =
  let rec loop : type a b. (a -> b) t -> a -> b =
    fun p x ->
    match p with
    | Atomic at ->
       run_atomic ctx at x
    | Seq (p1, p2) ->
       let y = loop p1 x in
       loop p2 y
  in
  loop p x

let command_line_arguments p =
  let rec pass_names : type a. a t -> string list =
    fun p ->
    match p with
    | Atomic { name; _ } ->
       [name]
    | Seq (p1, p2) ->
       pass_names p1 @ pass_names p2
  in
  [
    "-debug",
    Arg.Set Options.debug,
    " display debugging information";
    "-debug-pass",
    Arg.Symbol (pass_names p, Options.set_debug),
    " display debugging information for pass";
  ]
