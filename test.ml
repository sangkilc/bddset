(** Binary Decision Diagram (BDD) Based Set

    @author Sang Kil Cha <sangkilc\@cmu.edu>
 *)

(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2014, Sang Kil Cha                                      *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(**************************************************************************)

open BddSet

let set_to_string set =
  fold (fun i acc -> Printf.sprintf "%s%d," acc i) set ""

let osname =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname

let print_mem () =
  flush stdout;
  let _ =
    if osname = "Darwin" then
      Sys.command "ps `pgrep test.native` -O rss | grep -v RSS | awk '{ count ++; sum += $2 }; END {count --; print sum/1024, \"MB\" ;};'"
    else
      Sys.command "ps -C test.native -O rss | grep -v RSS | gawk '{ count ++; sum += $2 }; END {count --; print sum/1024, \"MB\" ;};'"
  in
  flush stdout

module IntSet = Set.Make(struct type t = int let compare = compare end)

let basic_test n =
  let () = init n in
  assert (n == get_max());

  let set = empty in
  let s1 = add 10 set in
  let s1 = add 42 s1 in
  let s2 = add 87 set in
  let s2 = add 42 s2 in
  let s2 = add 10 s2 in
  let s2 = remove 87 s2 in
  assert (equal s1 s2);
  print_endline "equality test passed";

  let s1 = remove 42 s1 in
  let s1 = remove 10 s1 in
  assert (is_empty s1);
  print_endline "empty set test passed";

  let s1 = add 10 s1 in
  let s1 = add 20 s1 in
  let s3 = inter s1 s2 in
  let s4 = union s1 s2 in
  let s5 = singleton 10 in
  assert (equal s3 s5);
  assert (cardinal s4 = 3);
  print_endline "inter/union test passed";

  let s4 = add 99 s4 in
  let s4 = add 11 s4 in
  let s4 = add 12 s4 in
  let s4 = add 72 s4 in
  let s4 = add 6  s4 in
  let s4 = add 64 s4 in
  let s4 = add 3  s4 in
  let s4 = add 85 s4 in
  let s4 = add 14 s4 in
  let s4 = add 39 s4 in
  let s = set_to_string s4 in
  assert (s = "3,6,10,11,12,14,20,39,42,64,72,85,99,");
  print_endline "basic test passed"

let intset_test n numiter =
  print_string "intset test: ";
  let start = Unix.gettimeofday () in
  let rec fill_rand s cnt =
    if cnt <= 0 then s
    else fill_rand (IntSet.add (Random.int n) s) (cnt-1)
  in
  let arr = Array.init numiter (fun _i -> fill_rand IntSet.empty n) in
  let () = Gc.full_major () in
  let fin = Unix.gettimeofday () in
  print_mem ();
  assert (Array.length arr = numiter);
  Printf.printf "%f sec.\n" (fin -. start);
  flush stdout

let bddset_test n numiter =
  print_string "bddset test: ";
  let start = Unix.gettimeofday () in
  let () = init n in
  let rec fill_rand s cnt =
    if cnt <= 0 then (s)
    else fill_rand (add (Random.int n) s) (cnt-1)
  in
  let arr = Array.init numiter (fun _i -> fill_rand empty n) in
  let () = Gc.full_major () in
  let fin = Unix.gettimeofday () in
  print_mem ();
  assert (Array.length arr = numiter);
  Printf.printf "%f sec.\n" (fin -. start);
  print_bddstat stderr;
  flush stdout

let _ =
  let n = 400 in
  basic_test n;
  intset_test n 4000;
  bddset_test n 4000;

  print_endline "all test finished"

  (*
  print_to_dot s1 ~file:"bdd1.dot";
  print_to_dot s2 ~file:"bdd2.dot";
  *)

