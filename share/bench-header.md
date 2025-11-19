Sycamore Persistent Set Benchmarks
==================================

This document contains timing benchmarks and discussion of the
persistent set implementations in Sycamore, FSet (2.0.0-rc2), and
CL-HAMT, along with a baseline of (mutable) ANSI CL Hash-Tables. The
performance ranking is as follows and is generally consistent across
the tested operations:

1. ANSI CL Hash-Tables (fastest construction and find, but mutable only)
2. Sycamore HAMTs (fastest persistent set)
3. FSet2 (CHAMP-variation of HAMTs)
4. Sycamore WB-Trees
5. FSet (WB-Trees)
6. CL-HAMT

Discussion
==========

Hash Array Mapped Tries: Sycamore vs. FSet
------------------------------------------

Sycamore and FSet implement different variations of Hash Array Mapped
Tries (HAMTs) [1] with the potential for certain trade-offs.  Sycamore
memoizes hash codes and stores entries and memoized hash codes in leaf
nodes.  FSet does not memoize hash codes and implements a variation of
Steindorfer's Compressed Hash-Array Mapped Prefix-tree (CHAMP) [2]
modification of HAMTs.  Memoized hash codes require extra memory; in
Syacamore, this is one cons cell per entry to store the fixnum
hash-code and reference to the entry.  However, memoized hash codes
can speed up some operations (e.g., avoiding recomputing hashes when
deepening the HAMT).  In contrast, CHAMP requires (slightly) more
memory per layer to store the extra bitmap that encodes which layer
elements are sublayers and which are entries.  The additional
per-layer memory will (slightly) increase allocations when modifying
the HAMT, though overall memory use will be lower than a memoizing
HAMT, and CHAMP requires a (perhaps trivially) few extra instructions
to consult its second per-layer bitmap.  We might thus expect
CPU-bound applications to favor memoized hash codes with fewer
instructions and allocations on operations, while memory- and
cache-bound applications could benefit from the reduced memory usage
of non-memoizing CHAMP.  The benchmark results below support this
expectation: On operations where Sycamore and FSet use similar
algorithms, Sycamore HAMTs are slightly faster (generally 1.05-1.20x
speedup) on small to medium sets (<2^20 elements), with performance
leveling out and FSet sometimes being slightly faster on larger sets
(>2^20 elements).  For this benchmark set of the independent HAMT
implementations in Sycamore and FSet, Steindorfer's strong claims of
CHAMP's performance are not reproduced.

Sycamore substantially outperforms FSet on construction (1.3-4.5x),
union (1.3-3x), intersection (1.3-2.2x), and difference (1.4-4.7x).
The performance difference for construction is explained by Sycamore's
optimized implementation new HAMT construction using destructive
inserts.  The performance difference for union, intersection, and
difference is explained by Sycamore's more efficient construction of
new layers in these operations: Sycamore directly constructs the final
layer array and fills it in without any heap-allocated intermediate
(soon-to-be-garbage) layers.  In contrast, FSet grows layers
element-by-element in these operations, resulting in more allocations
and generating more garbage.

Weight-Balanced Trees: Sycamore vs. FSet
----------------------------------------

Sycamore and FSet both implement weight-balanced trees that compress
leaves into arrays.  Leaf compression reduces tree height and
generally results in better performance than, e.g. textbook persistent
red-black trees [3].

Trees in Sycamore are uniformly faster than FSet, with varying
speedups depending on the operation.  For many operations, Sycamore's
speedups are in the 1.3-2x range, and larger speedups exist for union,
intersection, and difference.  FSet's algorithms for union,
intersection, and difference are based on [4], while Sycamore
implements a divide-and-conquer approach similar to [5] that appears
on these benchmarks to be more efficient.

A key design difference between Sycamore and FSet is the
implementation of comparison operators.  FSet implements comparisons
with a CLOS-based set interface, while Sycamore's finite sets take a
parameter for a comparison function.  Generic vs. explicit comparison
functions are an aesthetic, flexibility, and performance issue.
FSet's generic comparison functions do not need to be passed
explicitly, while Sycamore requires explicit comparison function
parameters.  Explicit functions permit comparing the same type
differently if needed (e.g., lexicographic vs. fast string
comparison), and explicit functions are more efficient than CLOS
method dispatch.

References
----------

[1] Bagwell, Phil. Ideal Hash Trees. EPFL Technical Report, 2001.

[2] Michael J. Steindorfer and Jurgen J. Vinju. 2015. Optimizing
    hash-array mapped tries for fast and lean immutable JVM
    collections. SIGPLAN Not. 50, 10 (October 2015),
    783–800. https://doi.org/10.1145/2858965.2814312

[3] Okasaki, Chris. "Purely Functional Data Structures." Cambridge
    University Press. June 1999. ISBN: 978-0521663502.

[4] Adams, Stephen., 1992. Implementing sets efficiently in a functional
    language. University of Southampton. Tech Report CSTR 92-10.

[5] OCaml Sets. [Documentation](https://ocaml.org/docs/sets).
    [Code](https://github.com/ocaml/ocaml/blob/trunk/stdlib/set.ml)
