(** Binary Decision Diagram (BDD) Based Integer Set

    @author Sang Kil Cha <sangkilc\@cmu.edu>
 *)

(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2014, Sang Kil Cha                                      *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Bdd

type t = Bdd.t

(* maximum number of integers *)
let max_n = ref 0
(* maximum number of binary digits *)
let bdd_max = ref 0

let init n =
  max_n := n;
  (* x = log_2 n = log_10 n / log_10 2 *)
  let n = float_of_int n in
  let n = ceil ((log10 n) /. (log10 2.0)) in
  let n = int_of_float n in
  set_max_var n;
  bdd_max := n

let get_max () = !max_n

let empty = zero

let is_empty set = set == zero

let to_binary i n =
  let rec binloop acc i =
    if i == 0 then acc
    else binloop ((i mod 2 == 1)::acc) (i /2)
  in
  let rec fillzero acc c n =
    if c < n then fillzero (false::acc) (c+1) n
    else if c = n then acc
    else failwith "cannot exceed max number"
  in
  let r = binloop [] i in
  fillzero r (List.length r) n

let rec adder bitpos = function
  | bit::rest ->
     if bit then mk bitpos zero (adder (bitpos+1) rest)
     else mk bitpos (adder (bitpos+1) rest) zero
  | [] -> one

let add elt set =
  let bin = to_binary elt !bdd_max in
  let newset = adder 1 bin in
  mk_or newset set

let remove elt set =
  let bin = to_binary elt !bdd_max in
  let newset = adder 1 bin in
  let newset = mk_not newset in
  mk_and newset set

let singleton elt = add elt zero

let inter s1 s2 = mk_and s1 s2
let union s1 s2 = mk_or s1 s2

(** cardinality of the set = sat_count *)
let cardinal set = Int64.to_int (count_sat set)

(** check equality *)
let equal s1 s2 = s1 == s2

let bintoint pos num = 2.0 ** pos +. num

let fold fn set acc =
  let rec visit num pos lastidx set acc =
    let nextpos = pos -. 1.0 in
    match set.node with
      | Zero ->
          acc
      | One ->
          if lastidx <= !bdd_max then
            let acc = visit num nextpos (lastidx+1) set acc in
            visit (bintoint pos num) nextpos (lastidx+1) set acc
          else
            fn (int_of_float num) acc
      | Node (v, l, h) ->
          if lastidx = v then
            let acc = visit num nextpos (lastidx+1) l acc in
            visit (bintoint pos num) nextpos (lastidx+1) h acc
          else
            let acc = visit num nextpos (lastidx+1) set acc in
            visit (bintoint pos num) nextpos (lastidx+1) set acc
  in
  visit 0.0 (float_of_int (!bdd_max - 1)) 1 set acc

let iter fn set =
  fold (fun elt () -> fn elt) set ()

let print_bddstat chan =
  let arr = stats () in
  let livecnt, totalcnt =
    Array.fold_left (fun (liveacc, totalacc) (_, livecnt, totlen, sm, md, lg) ->
      (* Printf.printf "%d,%d,%d;;" sm md lg; *)
      liveacc + livecnt, totalacc + totlen
    ) (0,0) arr
  in
  Printf.fprintf chan "live = %10d, total = %10d\n" livecnt totalcnt

let print_to_dot = print_to_dot

