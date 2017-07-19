type filename = string

type pos =
  {
    lnum : int;
    cnum : int;
  }

let (<=) p1 p2 =
  p1.lnum <= p2.lnum || (p1.lnum = p2.lnum && p1.cnum <= p2.cnum)

let min p1 p2 =
  if p1 <= p2 then p1 else p2

let max p1 p2 =
  if p1 <= p2 then p2 else p1

let make_pos ~lnum ~cnum =
  if lnum < 0 then invalid_arg "make_pos: negative line number"
  else if cnum < 0 then invalid_arg "make_pos: negative column number"
  else { lnum; cnum; }

let lnum { lnum; _ } = lnum

let cnum { cnum; _ } = cnum

let print_pos fmt { lnum; cnum; } =
  Format.fprintf fmt "%d:%d" lnum cnum

let dummy_pos = { lnum = -1 ; cnum = -1; }

let pos_of_lexing_pos { Lexing.pos_lnum; Lexing.pos_bol; Lexing.pos_cnum; } =
  { lnum = pos_lnum + 1; cnum = pos_cnum - pos_bol; }

type loc =
  {
    fn : filename;
    start : pos;
    stop : pos;
  }

let make_loc ~fn ~start ~stop =
  if fn = "" then invalid_arg "make_loc: empty file name"
  else if not (start <= stop) then invalid_arg "make_loc: stop < start"
  else { fn; start; stop; }

let loc_of_lexing_pos_pair ~start ~stop =
  let fn = start.Lexing.pos_fname in
  if fn <> stop.Lexing.pos_fname
  then invalid_arg "loc_of_lexing_pos_pair";
  let start = pos_of_lexing_pos start in
  let stop = pos_of_lexing_pos stop in
  make_loc ~fn ~start ~stop

let print_loc fmt { fn; start; stop; }  =
  let print_pos_optional_line fmt pos =
    if pos.lnum = start.lnum
    then Format.fprintf fmt "%d" pos.cnum
    else print_pos fmt pos
  in
  Format.fprintf fmt "\"%s\", from %a to %a"
    fn
    print_pos start
    print_pos_optional_line stop

let print_loc_sameline fmt { fn; start; stop; }  =
  if start.lnum <> stop.lnum then invalid_arg "print_loc_sameline";
  Format.fprintf fmt "\"%s\", line %d, characters %d-%d"
    fn
    start.lnum
    start.cnum
    stop.cnum

let nowhere =
  { fn = ""; start = dummy_pos; stop = dummy_pos; }

let join l1 l2 =
  if l1.fn <> l2.fn then invalid_arg "join";
  {
    fn = l1.fn;
    start = min l1.start l2.start;
    stop = max l1.stop l2.stop;
  }

type 'a located =
  {
    contents : 'a;
    loc : loc;
  }

let print_located print_contents ?(disp_loc = false) fmt { contents; loc; } =
  if disp_loc
  then Format.fprintf fmt "@[%a:@ %a@]" print_loc loc print_contents contents
  else print_contents fmt contents
