(* This file is part of Pulsar, a temporal functional language.
 * Copyright (C) 2017 Adrien Guatto
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the LICENSE file in the top-level directory.
 *)

let verbose =
  ref false

type test =
  | Ones of Clock.Periodic.t * (Enat.t * Enat.t) list
  | On of Clock.Periodic.t * Clock.Periodic.t * Clock.Periodic.t
  | Ladj of Clock.Periodic.t * Clock.Periodic.t
  | Radj of Clock.Periodic.t * Clock.Periodic.t
  | Quick_normalize of Clock.Periodic.t

type 'a test_result_rec =
  {
    print : Format.formatter -> 'a -> unit;
    equal : 'a -> 'a -> bool;
    expected : 'a;
    actual : 'a;
  }

type test_result = Result : 'a test_result_rec -> test_result

let print_test fmt test =
  match test with
  | Ones (p, ns) ->
     Format.fprintf fmt "ones @[%a@] ?= [@[%a@]]"
       Clock.Periodic.print p
       (Utils.print_list_r (Utils.print_pair Enat.print Enat.print) ",") ns
  | On (p, q, r) ->
     Format.fprintf fmt "@[%a@] on @[%a@] ?= @[%a@]"
       Clock.Periodic.print p
       Clock.Periodic.print q
       Clock.Periodic.print r
  | Ladj (p, q) ->
     Format.fprintf fmt "@[%a@]^* ?= @[%a@]"
       Clock.Periodic.print p
       Clock.Periodic.print q
  | Radj (p, q) ->
     Format.fprintf fmt "@[%a@]_* ?= @[%a@]"
       Clock.Periodic.print p
       Clock.Periodic.print q
  | Quick_normalize p ->
     Format.fprintf fmt "quick_normalize @[%a@] ?= @[%a@]"
       Clock.Periodic.print p
       Clock.Periodic.print p

let print_result_rec fmt (test, res) =
  Format.fprintf fmt "test @[<h>%a@]@\n"
    print_test test
  ;
  Format.fprintf fmt "expected:@ @[%a@]@\n"
    res.print res.expected
  ;
  Format.fprintf fmt "obtained:@ @[%a@]"
    res.print res.actual

let print_backtrace_slots fmt slots =
  let open Printexc in
  match slots with
  | None ->
     Format.fprintf fmt "(no backtrace)"
  | Some a ->
     let print_backtrace_slot fmt slot =
       match Slot.format 1 slot with
       | None ->
          ()
       | Some s ->
          Format.fprintf fmt "%s" s
     in
     Utils.print_list_eol print_backtrace_slot fmt @@ Array.to_list a

let result_of_test test =
  match test with
  | Ones (p, ijs) ->
     let is, expected = List.split ijs in
     let actual = List.map (Clock.Periodic.ones p) is in
     let print = Utils.print_list_r Enat.print "," in
     let equal js1 js2 =
       List.for_all (fun (j1, j2) -> Enat.equal j1 j2) @@ List.combine js1 js2
     in
     Result { print; equal; expected; actual; }

  | On (p, q, expected) ->
     let actual = Clock.Periodic.on p q in
     Result { print = Clock.Periodic.print; equal = Clock.Periodic.equal;
              expected; actual; }

  | Ladj (p, expected) ->
     let actual = Clock.Periodic.ladj p in
     Result { print = Clock.Periodic.print; equal = Clock.Periodic.equal;
              expected; actual; }

  | Radj (p, expected) ->
     let actual = Clock.Periodic.radj p in
     Result { print = Clock.Periodic.print; equal = Clock.Periodic.equal;
              expected; actual; }

  | Quick_normalize expected ->
     let actual = Clock.Periodic.quick_normalize expected in
     Result { print = Clock.Periodic.print; equal = Clock.Periodic.equal;
              expected; actual; }

exception Timeout

let timeout = ref 1

let watchdog (f : 'a -> 'b) (x : 'a) =
  let fd_in, fd_out = Unix.pipe () in
  let ch_in = Unix.in_channel_of_descr fd_in in
  let ch_out = Unix.out_channel_of_descr fd_out in

  let close () =
    Unix.close fd_in;
    Unix.close fd_out
  in

  Format.printf "@?";
  flush stdout;
  match Unix.fork () with
  | 0 ->
     let y =
       try
         Utils.Left (f x)
       with exn ->
         let open Printexc in
         let slots = backtrace_slots @@ get_raw_backtrace () in
         Utils.Right (exn_slot_name exn, slots)
     in
     Marshal.to_channel ch_out y [Marshal.Closures];
     flush ch_out;
     exit 0
  | pid ->
     let on_timeout _ =
       close ();
       Unix.kill pid Sys.sigkill;
       raise Timeout
     in
     ignore Sys.(signal sigalrm (Signal_handle on_timeout));
     ignore (Unix.alarm !timeout);
     let (y : ('b, string * Printexc.backtrace_slot array option) Utils.sum) =
       Marshal.from_channel ch_in
     in
     ignore (Unix.waitpid [] pid);
     ignore (Unix.alarm 0);
     close ();
     y

let do_test group count (i, passing, failed) test =
  let print_tick_mark fmt () =
    Format.fprintf fmt "\xE2\x9C\x93"
  in

  let print_ballot_x fmt () =
    Format.fprintf fmt "\xE2\x9C\x97"
  in

  Format.printf " (%.3d/%.3d %s): "
    (i + 1)
    count
    group
  ;

  let passing, failed =
    match watchdog result_of_test test with
    | Utils.Left (Result res) ->
       (
         match res.equal res.expected res.actual with
         | true ->
            Format.printf "%a" print_tick_mark ();
            if !verbose then
              Format.printf "@\n@[ @ @[<h>%a@]@]"
                print_result_rec (test, res)
            ;
            passing + 1, failed
         | false ->
            Format.printf "%a@\n" print_ballot_x ();
            Format.printf "@[  @[<h>%a@]@]"
              print_result_rec (test, res)
            ;
            passing, failed + 1
       )
    | exception Timeout ->
       Format.printf "%a@\n" print_ballot_x ();
       Format.printf "  test @[<h>%a@]@\n" print_test test;
       Format.printf "  timeout";
       passing, failed + 1
    | Utils.Right (exn_name, slots) ->
       Format.printf "%a@\n" print_ballot_x ();
       Format.printf "  test @[<h>%a@]@\n" print_test test;
       Format.printf "  exception %s@\n" exn_name;
       Format.printf "@[   @[<h>%a@]@]"
         print_backtrace_slots slots
       ;
       passing, failed + 1
  in
  Format.printf "@.";
  flush stdout;
  i + 1, passing, failed

let make u v =
  let prefix = Word.of_list u in
  let ppattern = Word.of_list v in
  if Word.weight ppattern = 0
  then Clock.Periodic.make_extremal ~prefix Clock.Periodic.Zero
  else Clock.Periodic.make_pattern ~prefix ~ppattern

let make_omega u =
  let prefix = Word.of_list u in
  Clock.Periodic.make_extremal ~prefix Clock.Periodic.Omega

let tests = Hashtbl.create 10

let register_tests ~name test_list =
  Hashtbl.add tests name test_list

let display_message kind passing failed =
  let total = passing + failed in
  Format.printf "%s: ran %d tests, %d (%.2f%%) passed, %d (%.2f%%) failed@."
    kind
    total
    passing
    (float passing /. float total *. 100.)
    failed
    (float failed /. float total *. 100.)

let run_tests name =
  try
    let tests = Hashtbl.find tests name in
    let test_count = List.length tests in
    let _, passing, failed =
      List.fold_left (do_test name test_count) (0, 0, 0) tests
    in
    display_message ("group \"" ^ name ^ "\"") passing failed;
    passing, failed
  with Not_found ->
    Format.eprintf "Could not find group %s@." name;
    exit 1

let _ =
  register_tests
    ~name:"ladj"
    [
      (* (0 1)^* = (2) *)
      Ladj (make [] [0; 1], make [] [2]);
      (* (2 0)_* = 1(0 2) *)
      Ladj (make [] [2; 0], make [1] [0; 2]);
      (* (0 2)^* = (2 0) *)
      Ladj (make [] [0; 2], make [] [2; 0]);
      (* (w)^* = 1(0) *)
      Ladj (make_omega [], make [1] [0]);
      (* 1 0 2(w)^* = 1 2 0 1(0) *)
      Ladj (make_omega [1; 0; 2], make [1; 2; 0; 1] [0]);
      (* 0(0 1)^* = 3(2) *)
      Ladj (make [0] [0; 1], make [3] [2]);
      (* 0(0 1)^* = 3(2) *)
      Ladj (make [0] [0; 1], make [3] [2]);
      (* (1 0)^* = 1(2) *)
      Ladj (make [] [1; 0], make [1] [2]);
      (* (3 0 1 0)^* = 1(0 0 2 2) *)
      Ladj (make [] [3; 0; 1; 0], make [1] [0; 0; 2; 2]);
      (* (0 1 0)^* = 2(3) *)
      Ladj (make [] [0; 1; 0], make [2] [3]);
      (* 1 3 0(2 0 1)^* = 1 1 0 0 2(0 2 1) *)
      Ladj (make [1; 3; 0] [2; 0; 1], make [1; 1; 0; 0; 2] [0; 2; 1]);
    ]

let _ =
  register_tests
    ~name:"radj"
    [
      (* 0 1 2(0)_* = 1 1 0(w) *)
      Radj (make [0; 1; 2] [0], make_omega [1; 1; 0]);
      (* 0 1 0 2(0)_* = 1 2 0(w) *)
      Radj (make [0; 1; 0; 2] [0], make_omega [1; 2; 0]);
      (* (2 0)_* = 0(0 2) *)
      Radj (make [] [2; 0], make [0] [0; 2]);
      (* (0 2)_* = 1(0 2) *)
      Radj (make [] [0; 2], make [1] [0; 2]);
      (* (1 0 0)_* = 0(3) *)
      Radj (make [] [1; 0; 0], make [0] [3]);
      (* 0(w)_* = 1(0) *)
      Radj (make_omega [0], make [1] [0]);
      (* 0 1 0 0(w)_* = 1 3(0) *)
      Radj (make_omega [0; 1; 0; 0], make [1; 3] [0]);
      (* 1 0 1(w)_* = 0 2 1(0) *)
      Radj (make_omega [1; 0; 1], make [0; 2; 1] [0]);
      (* 1 3(0)_* = 0 1 0 0(w) *)
      Radj (make [1; 3] [0], make_omega [0; 1; 0; 0]);
      (* 1 3 0(2 0 1)_* = 0 1 0 0 2(0 2 1) *)
      Radj (make [1; 3; 0] [2; 0; 1], make [0; 1; 0; 0; 2] [0; 2; 1]);
    ]

let _ =
  register_tests
    ~name:"on"
    [
      (* (w) on 201(0) = 3(0) *)
      On (make_omega [], make [2; 0; 1] [0], make [3] [0]);
      (* 0(01) on 1(w) = 0010(w) *)
      On (make [0] [0; 1], make_omega [1], make_omega [0; 0; 1; 0]);
      (* 0(001) on 1(2) = 0001(002) *)
      On (make [0] [0; 0; 1], make [1] [2], make [0; 0; 0; 1] [0; 0; 2]);
      (* 102(0) on 012(w) = 003(0) *)
      On (make [1; 0; 2] [0], make_omega [0; 1; 2], make [0; 0; 3] [0]);
      (* 103(0) on 012(w) = 00(w) *)
      On (make [1; 0; 3] [0], make_omega [0; 1; 2], make_omega [0; 0]);
      (* (2) on (1 0) = (1) *)
      On (make [] [2], make [] [1; 0], make [] [1]);
      (* (2) on (0 1) = (1) *)
      On (make [] [2], make [] [0; 1], make [] [1]);
      (* (0 2) on (0 2) = (1) *)
      On (make [] [0; 2], make [] [0; 2], make [] [0; 2]);
      (* 1(1 0) on (0 2) = (0 2 0 0) *)
      On (make [1] [1; 0], make [] [0; 2], make [] [0; 2; 0; 0]);
    ]

let _ =
  register_tests
    ~name:"norm"
    [
      Quick_normalize (make [2; 0] [2; 0]);
      Quick_normalize (make [0; 2] [0; 2]);
      Quick_normalize (make [0; 0; 1] [0; 1; 0; 1]);
    ]

let _ =
  let groups = ref [] in
  let args =
    let open Arg in
    [
      "-verbose",
      Set verbose,
      "verbose output";

      "-timeout",
      Set_int timeout,
      "timeout limit for tests";

      "-only",
      String (fun s -> groups := s :: !groups),
      "only perform test belong to the group";
    ]
  in
  Arg.parse args (fun _ -> ()) ("Usage: " ^ Sys.argv.(0));
  if !groups = [] then groups := Hashtbl.fold (fun k _ l -> k :: l) tests [];
  groups := List.rev !groups;

  Printexc.record_backtrace true;
  let test_group (passing, failed) name =
    let local_passing, local_failed = run_tests name in
    passing + local_passing, failed + local_failed
  in

  let passing, failed = List.fold_left test_group (0, 0) !groups in
  display_message "total" passing failed
