type extremal =
  | Zero
  | Omega

type period =
  | Ext of extremal
  | Pat of Word.t

type t =
  {
    u : Word.t;
    v : period;
  }

(** Printing *)

let print_period fmt per =
  match per with
  | Ext Zero ->
     Format.fprintf fmt "0"
  | Ext Omega ->
     if !Warp.Print.utf8_output
     then Format.fprintf fmt "\xCF\x89"
     else Format.fprintf fmt "w"
  | Pat w ->
     Word.print fmt w

let print fmt p =
  Format.fprintf fmt "`%a(%a)"
    Word.print p.u
    print_period p.v

(** Construction and destruction *)

let extremal ?(prefix = Word.empty) ext =
  {
    u = prefix;
    v = Ext ext;
  }

let pattern ?(prefix = Word.empty) ~ppattern =
  if Word.is_empty ppattern
  then invalid_arg "pattern: empty periodic pattern";
  if Word.has_null_weight ppattern
  then invalid_arg "pattern: periodic pattern of null weight";
  {
    u = prefix;
    v = Pat ppattern;
  }

let singleton ?(prefix = Word.empty) en =
  match en with
  | Enat.Fin 0 ->
     extremal ~prefix Zero
  | Enat.Fin n ->
     pattern ~prefix ~ppattern:(Word.singleton 0)
  | Enat.Inf ->
     extremal ~prefix Omega

let at p n =
  assert (n >= 0);
  if n < Word.length p.u
  then Enat.Fin (Word.at p.u n)
  else
    match p.v with
    | Ext Zero ->
       Enat.Fin 0
    | Ext Omega ->
       Enat.Inf
    | Pat v ->
       Enat.Fin (Word.at v @@ (n - Word.length p.u) mod Word.length v)

let drop p n =
  if n < Word.length p.u
  then { p with u = Word.drop n p.u; }
  else
    match p.v with
    | Ext _ ->
       { p with u = Word.empty; }
    | Pat v ->
       let n = (n - Word.length p.u) mod Word.length v in
       let v1, v2 = Word.split_at n v in
       { u = Word.empty; v = Pat (Word.concat [v2; v1]); }

let pop p =
  at p 0, drop p 1

let push n p =
  { p with u = Word.(singleton n ^^ p.u); }

let ones p ii =
  let open Word in

  (* /!\ zero-indexed /!\ *)
  let ones_word w i =
    (* Format.eprintf "ones_word [@[%a@]] %d@." *)
    (*   Word.print w *)
    (*   i; *)
    let w1, _ = split_at i w in
    weight w1
  in

  match ii with
  | Enat.Inf ->
     begin match p.v with
     | Ext Zero ->
        Enat.Fin (weight p.u)
     | Ext Omega | Pat _ ->
        Enat.Inf
     end
  | Enat.Fin 0 ->
     Enat.Fin 0
  | Enat.Fin i ->
     if i <= length p.u
     then Enat.Fin (ones_word p.u i)
     else
       begin match p.v with
       | Ext Zero ->
          Enat.Fin (weight p.u)
       | Ext Omega ->
          Enat.Inf
       | Pat w ->
          let i = i - length p.u in
          let n1 = Utils.div_b1 i (length w) in
          let n2 = Utils.mod_b1 i (length w) in
          Enat.Fin (weight p.u + n1 * weight w + ones_word w n2)
       end

let one =
  pattern (Word.singleton 1)

let zero =
  extremal Zero

let zero_one =
  pattern ~prefix:(Word.singleton 0) ~ppattern:(Word.singleton 1)

let omega =
  extremal Omega

let is_finitary p =
  match p.v with
  | Ext Omega ->
     false
  | Ext Zero | Pat _ ->
     true

let is_stuck p =
  match p.v with
  | Ext Zero ->
     true
  | Ext Omega | Pat _ ->
     false

let is_one p =
  match p.v with
  | Ext _ ->
     false
  | Pat w ->
     Word.all_equal 1 p.u && Word.all_equal 1 w

let reverse_period per =
  match per with
  | Ext e ->
     Ext e
  | Pat v ->
     Pat (Word.rev v)

let normalize_prefix p =
  let rotate per =
    match per with
    | Ext e ->
       Ext e
    | Pat v ->
       Pat Word.(rotate v)
  in

  let head per =
    match per with
    | Ext Zero ->
       0
    | Ext Omega ->
       assert false
    | Pat v ->
       Word.at v 0
  in

  let rec loop revv u =
    if Word.is_empty u
    then Word.empty, revv
    else
      let n1 = head revv in
      let n2 = Word.at u 0 in
      if n1 = n2
      then loop (rotate revv) (Word.drop 1 u)
      else u, revv
  in

  match p.v with
  | Ext Omega ->
     p
  | Ext Zero | Pat _ ->
     let u, v = loop (reverse_period p.v) (Word.rev p.u) in
     let u = Word.rev u in
     let v = reverse_period v in
     { u; v; }

let quick_normalize_period p =
  let v =
    match p.v with
    | Ext _ ->
       p.v
    | Pat v ->
       let open Word in
       let n = at v 0 in
       Pat (if all_equal n v then singleton n else v)
  in
  { p with v; }

let quick_normalize p =
  quick_normalize_period @@ normalize_prefix p

let ladj p =
  (* [adjust_end_one p] returns a word [p'] equal to [p] such that if [p.v]
    is a pattern, [p'.u] and [p'.v] end with a non-zero number. *)
  let adjust_end_one p =
    match p.v with
    | Ext _ ->
       p
    | Pat v ->
       let rec unfold_prefix_until_one u v =
         let open Word in
         let n, v' = split_at 1 v in
         let u = u ^^ n in
         let v = v' ^^ n in
         if weight n = 0
         then unfold_prefix_until_one u v
         else { u; v = Pat v; }
       in
       let prefix_ends_zero =
         Word.(length p.u = 0 || at p.u (length p.u - 1) > 0)
       in
       let period_ends_zero = Word.(at v (length v - 1) > 0) in
       if prefix_ends_zero && period_ends_zero
       then p
       else unfold_prefix_until_one p.u v
  in

  let ladj_pref len p =
    assert (len >= 0);

    let pattern zeroes value =
      let open Word in
      singleton (zeroes + 1) ^^ power (singleton 0) (value - 1)
    in

    let rec loop res zeroes i p =
      assert (i >= 0 && i <= len);
      if i = len
      then
        res, zeroes, p
      else
        let n, p = pop p in
        match n with
        | Enat.Fin 0 ->
           loop res (zeroes + 1) (i + 1) p
        | Enat.Fin value ->
           assert (value > 0);
           loop Word.(res ^^ pattern zeroes value) 0 (i + 1) p
        | Enat.Inf ->
           assert false
    in
    loop Word.empty 0 0 p
  in

  let p = adjust_end_one p in
  let u, zeroes, p = ladj_pref (Word.length p.u) p in
  match p.v with
  | Ext Zero ->
     { u; v = Ext Omega; }
  | Ext Omega ->
     let u = Word.(u ^^ singleton @@ zeroes + 1) in
     { u; v = Ext Zero; }
  | Pat v ->
     assert (zeroes = 0);
     let v, zeroes, _ = ladj_pref (Word.length v) p in
     assert (zeroes = 0);
     { u; v = Pat v; }

let radj p =
  let pl = ladj p in
  let n, pl = pop pl in
  match n with
  | Enat.Fin 0 | Enat.Inf ->
     assert false
  | Enat.Fin n ->
     push (n - 1) pl

let hyper_prefix p q =
  max (Word.length p.u) (Word.length q.u)

let hyper_period p q =
  let len per =
    match per with
    | Ext _ -> 1
    | Pat v -> Word.length v
  in
  Utils.lcm (len p.v) (len q.v)

exception FoundOmega of Word.t

let div p q =
  let rec loop res sum i max =
    if i = max
    then res, sum
    else
      let n = Enat.to_int @@ at q i in
      let sum = sum + (Enat.to_int @@ at p i) in
      let sum, res =
        if n > 0
        then 0, Word.(res ^^ singleton sum ^^ power (singleton 0) (n - 1))
        else sum, res
      in
      loop res sum (i + 1) max
  in
  match p.v, q.v with
  | Ext Omega, Ext Omega ->
     let u_n = min (Word.length p.u) (Word.length q.u) in
     let prefix, _ = loop Word.empty 0 0 u_n in
     extremal ~prefix Omega
  | Ext Omega, _ ->
     let u_n = Word.length p.u in
     let prefix, _ = loop Word.empty 0 0 u_n in
     extremal ~prefix Omega
  | _, Ext Omega ->
     let u_n = Word.length q.u in
     let prefix, sum = loop Word.empty 0 0 u_n in
     let m = Enat.to_int @@ at p (u_n + 1) in
     let prefix = Word.(prefix ^^ singleton (sum + m)) in
     extremal ~prefix Zero
  | _, Ext Zero ->
     let u_n = Word.length q.u in
     let prefix, _ = loop Word.empty 0 0 u_n in
     extremal ~prefix Omega
  | Ext Zero, Pat v ->
     let u_n = max (Word.length p.u) (Word.length q.u) in
     let prefix, _ = loop Word.empty 0 0 u_n in
     extremal ~prefix Zero
  | Pat _, Pat q_v ->
     let u_n = hyper_prefix p q + Word.find_first_non_null_index q_v + 1 in
     let v_n = hyper_period p q in
     let prefix, sum = loop Word.empty 0 0 u_n in
     let ppattern, _ = loop Word.empty sum u_n (u_n + v_n) in
     pattern ~prefix ~ppattern

let period_weight p =
  match p.v with
  | Ext Zero ->
     Enat.Fin 0
  | Ext Omega ->
     Enat.Inf
  | Pat w ->
     Enat.Fin (Word.weight w)

let weight p =
  match p.v with
  | Ext Omega | Pat _ ->
     Enat.Inf
  | Ext Zero ->
     Enat.Fin (Word.weight p.u)

let iof p j =
  ones (ladj p) j

let lbf p j =
  ones (radj p) (Enat.succ j)

let on p q =
  let on_pref len p q =
    let rec loop acc i p q =
      assert (i >= 0 && i <= len);
      if i = len
      then acc, p, q
      else
        let n, p = pop p in
        let s, q =
          match n with
          | Enat.Fin n ->
             let rec sum s j q =
               assert (j >= 0 && j <= n);
               if j = n
               then s, q
               else
                 let m, q = pop q in
                 sum (s + Enat.to_int m) (j + 1) q
             in
             sum 0 0 q
          | Enat.Inf ->
             (* special case *)
             assert (i + 1 = len);
             Enat.to_int (ones q Enat.Inf), q
        in
        loop Word.(acc ^^ singleton s) (i + 1) p q
    in
    loop Word.empty 0 p q
  in

  let iof_p_pref_q p q =
    iof p @@ Enat.of_int @@ Word.length q.u
  in

  let lbf_p_pref_q p q =
    lbf p @@ Enat.of_int @@ Word.length q.u
  in

  let p =
    match p.v, q.v with
    | Ext Zero, Ext Zero ->
       let u_len =
         let open Enat in
         to_int @@ min (of_int @@ Word.length p.u) (iof_p_pref_q p q)
       in
       let u, _, _ = on_pref u_len p q in
       { u; v = Ext Zero; }

    | Ext Omega, Ext Zero ->
       let u_len = min (Word.length p.u + 1) (Word.length q.u) in
       let u, _, _ = on_pref u_len p q in
       { u; v = Ext Zero; }

    | Pat _, Ext Zero ->
       let u_len = Enat.to_int @@ iof_p_pref_q p q in
       let u, _, _ = on_pref u_len p q in
       { u; v = Ext Zero; }

    | Ext Zero, Ext Omega when Word.weight p.u <= Word.length q.u ->
       let u, _, _ = on_pref (Word.length p.u) p q in
       { u; v = Ext Zero; }

    | (Ext _ | Pat _), Ext Omega ->
       let u_len = Enat.to_int @@ lbf_p_pref_q p q in
       let u, _, _ = on_pref u_len p q in
       { u; v = Ext Omega; }

    | Ext ext, Pat _ ->
       let u, _, _ = on_pref (Word.length p.u) p q in
       { u; v = Ext ext; }

    | Pat v1, Pat v2 ->
       let u_len =
         max
           (Word.length p.u)
           Enat.(to_int @@ iof p (of_int @@ Word.length q.u))
       in
       let v_len =
         Utils.lcm (Word.weight v1) (Word.length v2)
         / Word.weight v1 * Word.length v1
       in
       let u, p, q = on_pref u_len p q in
       let v, _, _ = on_pref v_len p q in
       { u; v = Pat v; }
  in
  p

let lc_phase p q =
  max (Word.length p.u) (Word.length q.u)

let period per =
  match per with
  | Ext _ ->
     1
  | Pat v ->
     Word.length v

let lc_period p q =
  max (period p.v) (period q.v)

let lc_length p q =
  lc_phase p q + lc_period p q

let equal p q =
  let len = lc_length p q in
  let rec loop i =
    (i >= len) || (Enat.equal (at p i) (at q i) && loop (i + 1))
  in
  loop 0

let compare_per p1 p2 =
  let tag_to_int tag =
    match tag with
    | Ext Zero ->
       0
    | Ext Omega ->
       1
    | Pat _ ->
       2
  in
  match p1, p2 with
  | Ext Zero, Ext Zero
  | Ext Omega, Ext Omega ->
     0
  | Pat v1, Pat v2 ->
     Word.compare v1 v2
  | (Ext (Zero | Omega) | Pat _), _ ->
     Utils.compare_int (tag_to_int p1) (tag_to_int p2)

let compare p q =
  Utils.compare_both
    (Word.compare p.u q.u)
    (fun () -> compare_per p.v q.v)

let ( <= ) p q =
  let rec loop sum_p sum_q i max =
    if i >= max
    then true
    else
      let sum_p = Enat.(sum_p + at p i) in
      let sum_q = Enat.(sum_q + at q i) in
      Enat.(sum_p <= sum_q) && loop sum_p sum_q (i + 1) max
  in
  loop (Enat.Fin 0) (Enat.Fin 0) 0 (hyper_prefix p q + hyper_period p q)
