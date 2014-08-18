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

type t

(** init n initializes BddSet to have the maximum n elements *)
val init : int -> unit

(** returns the number of elements that the set can contain *)
val get_max : unit -> int

(** returns an empty set *)
val empty : t

(** check if the set is empty *)
val is_empty : t -> bool

(** add e s adds an element e to s and returns the new set *)
val add : int -> t -> t

(** remove x s returns a set containing all elements of s , except x *)
val remove : int -> t -> t

(** singleton elt returns a set with a single element elt *)
val singleton : int -> t

(** intersection *)
val inter : t -> t -> t

(** union *)
val union : t -> t -> t

(** set cardinality *)
val cardinal : t -> int

(** check equality *)
val equal : t -> t -> bool

(** iterate *)
val iter : (int -> unit) -> t -> unit

(** fold *)
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

(** visualize the current set in BDD *)
val print_to_dot : t -> file:string -> unit

(** dump internal bdd stat *)
val print_bddstat : Pervasives.out_channel -> unit

