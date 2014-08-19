BddSet
======

BDD-based integer set representation.

This is not a BDD library. This is a memory-efficient set representation when
there exist a lot of redundant and overlapping sets. The idea is derived from a
paper (Efficient Forward Computation of Dynamic Slices Using Reduced Ordered
Binary Decision Diagrams) by Zhang *et al*.

The BDD implementation is simplified and optimized from the original BDD
implementation by Jean-Christophe:
[link](https://www.lri.fr/~filliatr/ftp/ocaml/bdd/bdd-0.3.tar.gz).

Installation
------------

  * ./configure --prefix=*path*
  * make

