(** Binary Decision Diagram (BDD) Based Integer Set

    A large portion of the code is taken from Jean-Christophe's BDD
    implementation:
    (https://www.lri.fr/~filliatr/ftp/ocaml/bdd/bdd-0.3.tar.gz).

    @author Sang Kil Cha <sangkilc\@cmu.edu>
 *)

(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2014, Sang Kil Cha                                      *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This file incorporates work covered by the following copyright and    *)
(*  permission notice:                                                    *)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
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

type variable = int
  (** A variable is an integer, ranging from 1 to [max_var]. *)

val set_max_var : ?size:int -> int -> unit
  (** Sets the number [max_var] of variables.
      Additionally, the size of the internal nodes table for each variable
      can be specified. Each table has a default size (7001) and is
      resized when necessary (i.e. when too many collisions occur).
      IMPORTANT NOTE: bdds created before [set_max_var] should not be used
      anymore. *)

(** View *)

type bdd = { tag: int; node : view }
and view = Zero | One | Node of variable * bdd (*low*) * bdd (*high*)

type t = bdd

val view : t -> view
  (** Displays a bdd as a tree. *)

(** Accessors *)

val var : t -> variable
    (** The root variable of a bdd.
        Convention: [Zero] and [One] have variable [max_var+1] *)

val low : t -> t
val high : t -> t
    (** The low and high parts of a bdd, respectively.
        [low] and [high] raise [Invalid_argument] on [Zero] and [One]. *)

(** Constructors *)

val zero : t
val one : t

val mk : variable -> low:t -> high:t -> t
  (** Builds a bdd node.
      Raises [Invalid_argument] is the variable is out of [1..max_var]. *)

val mk_var : variable -> t
  (** Builds the bdd reduced to a single variable. *)

val mk_not : t -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t

val count_sat : t -> Int64.t
  (** Counts the number of different ways to satisfy a bdd. *)

(** Pretty printer *)

val print_to_dot : t -> file:string -> unit

(** Stats *)

val stats : unit -> (int * int * int * int * int * int) array
  (** Return statistics on the internal nodes tables (one for each variable).
      The numbers are, in order:
      table length, number of entries, sum of bucket lengths,
      smallest bucket length, median bucket length, biggest bucket length. *)

