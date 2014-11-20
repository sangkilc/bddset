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

module type S =
sig

  type elt

  type t

  val init : int -> unit

  (** returns the number of elements that the set can contain *)
  val get_max : unit -> int

  (** returns an empty set *)
  val empty : t

  (** check if the set is empty *)
  val is_empty : t -> bool

  (** add e s adds an element e to s and returns the new set *)
  val add : elt -> t -> t

  (** remove x s returns a set containing all elements of s , except x *)
  val remove : elt -> t -> t

  (** singleton elt returns a set with a single element elt *)
  val singleton : elt -> t

  (** intersection *)
  val inter : t -> t -> t

  (** union *)
  val union : t -> t -> t

  (** set cardinality *)
  val cardinal : t -> int

  (** check equality *)
  val equal : t -> t -> bool

  (** iterate *)
  val iter : (elt -> unit) -> t -> unit

  (** fold *)
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** visualize the current set in BDD *)
  val print_to_dot : t -> file:string -> unit

  (** dump internal bdd stat *)
  val print_bddstat : Pervasives.out_channel -> unit

end

module type IntMappedType =
sig

  (** The type of the set elements *)
  type t

  val to_int : t -> int

  val of_int : int -> t

end

module Make(T: IntMappedType) =
struct

  type elt = T.t

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

  let to_bdd elt =
    let rec loop acc bitpos i =
      if i == 0 then
        if bitpos > 0 then loop (mk bitpos acc zero) (bitpos-1) i
        else acc
      else
        if i mod 2 == 1 then loop (mk bitpos zero acc) (bitpos-1) (i/2)
        else loop (mk bitpos acc zero) (bitpos-1) (i/2)
    in
    loop one !bdd_max (T.to_int elt)

  let add elt set =
    let newset = to_bdd elt in
    mk_or newset set

  let remove elt set =
    let newset = to_bdd elt in
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
              fn (T.of_int (int_of_float num)) acc
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
      Array.fold_left
        (fun (liveacc, totalacc) (_, livecnt, totlen, sm, md, lg) ->
          (* Printf.printf "%d,%d,%d;;" sm md lg; *)
          liveacc + livecnt, totalacc + totlen
        ) (0,0) arr
    in
    Printf.fprintf chan "live = %10d, total = %10d\n" livecnt totalcnt

  let print_to_dot = print_to_dot

end

