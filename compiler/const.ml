type op =
  | Plus
  | Minus
  | Times
  | Div
  | Cons

let print_op fmt op =
  match op with
  | Plus ->
    Format.fprintf fmt "+"
  | Minus ->
    Format.fprintf fmt "-"
  | Times ->
    Pp.print_times fmt ()
  | Div ->
    Format.fprintf fmt "/"
  | Cons ->
    Format.fprintf fmt "::"

let compare_op op1 op2 =
  if op1 == op2 then 0
  else
    let tag_to_int op =
      match op with
      | Plus -> 0
      | Minus -> 1
      | Times -> 2
      | Div -> 3
      | Cons -> 4
    in
    match op1, op2 with
    | Plus, Plus
    | Minus, Minus
    | Times, Times
    | Div, Div
    | Cons, Cons->
      0
    | (Plus | Minus | Times | Div | Cons), _ ->
      Warp.Utils.compare_int (tag_to_int op1) (tag_to_int op2)

type const =
  | Lit of Scal.t
  | Op of op
  | When of Warp.Periodic.t
  | Merge of Warp.Periodic.t

let print_const fmt c =
  match c with
  | Lit s ->
    Scal.print fmt s
  | Op op ->
    Format.fprintf fmt "(%a)"
      print_op op
  | When p ->
    Format.fprintf fmt "@[when@ %a@]"
      Warp.Periodic.print p
  | Merge p ->
    Format.fprintf fmt "@[merge@ %a@]"
      Warp.Periodic.print p

let compare_const c1 c2 =
  if c1 == c2 then 0
  else
    let tag_to_int c =
      match c with
      | Lit _ -> 0
      | Op _ -> 1
      | When _ -> 2
      | Merge _ -> 3
    in
    match c1, c2 with
    | Lit l1, Lit l2 ->
      Scal.compare l1 l2
    | Op op1, Op op2 ->
      compare_op op1 op2
    | When p1, When p2 | Merge p1, Merge p2 ->
      Warp.Periodic.compare p1 p2
    | (Lit _ | Op _ | When _ | Merge _), _ ->
      Warp.Utils.compare_int (tag_to_int c1) (tag_to_int c2)
