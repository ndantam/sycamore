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

Test Setup
==========

Data
----

- Operations performed for random sets of the stated sizes
- 30 samples (random sets) per size
- Results for SYCAMORE-HAMT are shown as mean run time (from `GET-INTERNAL-RUN-TIME`) and mean memory allocations (from `SB-EXT:GET-BYTES-CONSED`)
- Results for others are shown as relative `(/ OTHER SYCAMORE-HAMT)`, equivalent to the speedup of SYCAMORE-HAMT over OTHER

System
------

- Date: 2025-11-19, 14:05
- Uname: `Linux farnsworth 6.12.48+deb13-amd64 #1 SMP PREEMPT_DYNAMIC Debian 6.12.48-1 (2025-09-20) x86_64 GNU/Linux`
- CPU: `Intel(R) Xeon(R) CPU E3-1275 v6 @ 3.80GHz`
- Lisp Implementation: `SBCL 2.5.2`

CONSTRUCT
=========

TIME
----

| Implementation   | 32      | 64       | 128      | 256      | 512       | 1024      | 2048      | 4096      | 8192      | 16384   | 32768   | 65536   | 131072   | 262144   | 524288   | 1048576   | 2097152   | 4194304 | 8388608 | 16777216 |
|------------------|---------|----------|----------|----------|-----------|-----------|-----------|-----------|-----------|---------|---------|---------|----------|----------|----------|-----------|-----------|---------|---------|----------|
| SYCAMORE-HAMT    | 9.08 us | 17.87 us | 35.11 us | 72.38 us | 149.34 us | 300.52 us | 392.77 us | 557.53 us | 931.50 us | 1.78 ms | 3.94 ms | 7.40 ms | 15.27 ms | 33.15 ms | 80.53 ms | 195.25 ms | 612.01 ms | 1.09 s  | 2.67 s  | 7.31 s   |
| SYCAMORE-WB-TREE | 1.22x   | 1.47x    | 1.51x    | 1.70x    | 1.75x     | 1.87x     | 2.89x     | 3.42x     | 4.59x     | 4.86x   | 4.70x   | 4.97x   | 5.27x    | 5.17x    | 4.64x    | 4.25x     | 3.36x     | 4.51x   | 4.51x   | 4.08x    |
| ANSI-HASH-TABLE  | 0.97x   | 1.02x    | 1.05x    | 1.03x    | 1.00x     | 1.00x     | 1.08x     | 1.16x     | 0.86x     | 0.89x   | 0.69x   | 0.71x   | 0.69x    | 0.67x    | 0.60x    | 0.58x     | 0.44x     | 0.55x   | 0.50x   | 0.41x    |
| FSET             | 1.38x   | 1.58x    | 1.80x    | 1.99x    | 2.27x     | 2.26x     | 3.15x     | 4.55x     | 5.84x     | 6.30x   | 6.18x   | 6.60x   | 7.08x    | 7.12x    | 6.38x    | 5.82x     | 4.53x     | 6.01x   | 5.94x   | 5.26x    |
| FSET2            | 1.28x   | 1.57x    | 1.56x    | 1.59x    | 1.69x     | 1.77x     | 2.72x     | 3.25x     | 4.03x     | 4.31x   | 4.28x   | 4.47x   | 4.56x    | 4.07x    | 3.09x    | 2.68x     | 2.18x     | 2.83x   | 2.67x   | 2.25x    |
| CL-HAMT          | 5.91x   | 5.34x    | 5.92x    | 6.02x    | 5.77x     | 5.78x     | 8.58x     | 11.86x    | 13.93x    | 14.89x  | 13.79x  | 13.79x  | 13.59x   | 15.58x   | 17.26x   | 15.97x    |           |         |         |          |

BYTES
-----

| Implementation   | 32       | 64       | 128      | 256       | 512       | 1024      | 2048       | 4096       | 8192       | 16384    | 32768    | 65536    | 131072    | 262144    | 524288    | 1048576    | 2097152    | 4194304    | 8388608    | 16777216 |
|------------------|----------|----------|----------|-----------|-----------|-----------|------------|------------|------------|----------|----------|----------|-----------|-----------|-----------|------------|------------|------------|------------|----------|
| SYCAMORE-HAMT    | 2.53 KiB | 4.59 KiB | 7.99 KiB | 15.98 KiB | 32.22 KiB | 95.82 KiB | 210.78 KiB | 383.45 KiB | 767.07 KiB | 1.69 MiB | 3.67 MiB | 6.99 MiB | 12.35 MiB | 24.80 MiB | 54.55 MiB | 117.87 MiB | 224.06 MiB | 395.57 MiB | 794.05 MiB | 1.71 GiB |
| SYCAMORE-WB-TREE | 1.97x    | 2.61x    | 3.57x    | 4.50x     | 5.19x     | 4.00x     | 4.23x      | 5.18x      | 5.71x      | 5.55x    | 5.52x    | 6.25x    | 7.57x     | 8.05x     | 7.78x     | 7.63x      | 8.46x      | 10.10x     | 10.55x     | 10.06x   |
| ANSI-HASH-TABLE  | 0.79x    | 0.85x    | 1.48x    | 1.41x     | 1.45x     | 0.97x     | 0.88x      | 0.97x      | 0.97x      | 0.89x    | 0.83x    | 0.89x    | 1.01x     | 1.01x     | 0.92x     | 0.86x      | 0.90x      | 1.02x      | 1.02x      | 0.93x    |
| FSET             | 1.84x    | 2.68x    | 3.90x    | 4.55x     | 5.45x     | 4.08x     | 4.29x      | 5.31x      | 5.86x      | 5.69x    | 5.66x    | 6.40x    | 7.75x     | 8.23x     | 7.96x     | 7.79x      | 8.65x      | 10.32x     | 10.78x     | 10.27x   |
| FSET2            | 2.64x    | 3.86x    | 5.35x    | 5.99x     | 6.93x     | 5.33x     | 5.60x      | 6.82x      | 7.31x      | 7.02x    | 7.04x    | 8.06x    | 9.74x     | 10.14x    | 9.74x     | 9.60x      | 10.76x     | 12.81x     | 13.20x     | 12.52x   |
| CL-HAMT          | 14.92x   | 16.03x   | 18.53x   | 19.10x    | 19.76x    | 13.36x    | 13.03x     | 14.76x     | 15.12x     | 13.72x   | 12.89x   | 13.91x   | 16.16x    | 16.41x    | 15.18x    | 14.34x     |            |            |            |          |

INSERT
======

TIME
----

| Implementation   | 32      | 64      | 128     | 256     | 512     | 1024    | 2048    | 4096    | 8192    | 16384   | 32768   | 65536   | 131072  | 262144  | 524288  | 1048576 | 2097152 | 4194304 | 8388608 | 16777216 |
|------------------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|----------|
| SYCAMORE-HAMT    | 0.34 us | 0.36 us | 0.39 us | 0.40 us | 0.44 us | 0.47 us | 0.40 us | 0.37 us | 0.37 us | 0.39 us | 0.43 us | 0.43 us | 0.44 us | 0.42 us | 0.42 us | 0.42 us | 0.44 us | 0.46 us | 0.48 us | 0.54 us  |
| SYCAMORE-WB-TREE | 1.08x   | 1.11x   | 1.13x   | 1.18x   | 1.15x   | 1.16x   | 1.20x   | 1.34x   | 1.34x   | 1.28x   | 1.22x   | 1.24x   | 1.30x   | 1.44x   | 1.61x   | 1.75x   | 1.89x   | 2.06x   | 2.27x   | 2.29x    |
| FSET             | 1.26x   | 1.34x   | 1.30x   | 1.38x   | 1.41x   | 1.40x   | 1.56x   | 1.64x   | 1.72x   | 1.67x   | 1.64x   | 1.67x   | 1.75x   | 1.99x   | 2.23x   | 2.42x   | 2.58x   | 2.74x   | 2.99x   | 3.00x    |
| FSET2            | 1.15x   | 1.16x   | 1.04x   | 1.05x   | 1.11x   | 1.10x   | 1.09x   | 1.15x   | 1.01x   | 1.03x   | 1.06x   | 1.07x   | 1.05x   | 1.05x   | 1.00x   | 0.99x   | 1.01x   | 1.00x   | 1.00x   | 0.97x    |
| CL-HAMT          | 4.31x   | 4.25x   | 3.80x   | 3.96x   | 3.95x   | 3.91x   | 4.70x   | 4.82x   | 4.81x   | 4.74x   | 4.41x   | 4.28x   | 4.11x   | 4.13x   | 4.27x   | 4.45x   |         |         |         |          |

BYTES
-----

| Implementation   | 32       | 64       | 128      | 256      | 512      | 1024     | 2048     | 4096     | 8192     | 16384    | 32768    | 65536    | 131072   | 262144   | 524288   | 1048576  | 2097152  | 4194304  | 8388608  | 16777216  |
|------------------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|-----------|
| SYCAMORE-HAMT    | 182.48 B | 233.92 B | 274.34 B | 293.97 B | 342.93 B | 396.04 B | 461.87 B | 504.96 B | 537.61 B | 582.87 B | 642.28 B | 695.81 B | 729.23 B | 757.44 B | 801.28 B | 859.11 B | 912.66 B | 945.85 B | 973.31 B | 1017.34 B |
| SYCAMORE-WB-TREE | 0.89x    | 0.85x    | 0.86x    | 0.92x    | 0.91x    | 0.88x    | 0.85x    | 0.85x    | 0.88x    | 0.88x    | 0.85x    | 0.84x    | 0.85x    | 0.87x    | 0.87x    | 0.86x    | 0.85x    | 0.86x    | 0.87x    | 0.87x     |
| FSET             | 0.88x    | 0.83x    | 0.87x    | 0.92x    | 0.91x    | 0.88x    | 0.85x    | 0.86x    | 0.89x    | 0.88x    | 0.86x    | 0.85x    | 0.86x    | 0.88x    | 0.88x    | 0.87x    | 0.86x    | 0.87x    | 0.88x    | 0.88x     |
| FSET2            | 1.25x    | 1.20x    | 1.13x    | 1.11x    | 1.12x    | 1.14x    | 1.13x    | 1.08x    | 1.06x    | 1.08x    | 1.09x    | 1.09x    | 1.07x    | 1.06x    | 1.07x    | 1.08x    | 1.08x    | 1.06x    | 1.06x    | 1.07x     |
| CL-HAMT          | 5.86x    | 4.80x    | 4.27x    | 3.97x    | 3.55x    | 3.21x    | 2.93x    | 2.76x    | 2.63x    | 2.49x    | 2.34x    | 2.24x    | 2.18x    | 2.13x    | 2.06x    | 1.97x    |          |          |          |           |

FIND
====

TIME
----

| Implementation   | 32      | 64      | 128     | 256     | 512     | 1024    | 2048    | 4096    | 8192    | 16384   | 32768   | 65536   | 131072  | 262144  | 524288  | 1048576 | 2097152 | 4194304 | 8388608 | 16777216 |
|------------------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|----------|
| SYCAMORE-HAMT    | 0.17 us | 0.18 us | 0.18 us | 0.18 us | 0.21 us | 0.22 us | 0.13 us | 0.08 us | 0.05 us | 0.05 us | 0.06 us | 0.06 us | 0.05 us | 0.06 us | 0.09 us | 0.14 us | 0.18 us | 0.20 us | 0.21 us | 0.25 us  |
| SYCAMORE-WB-TREE | 1.33x   | 1.30x   | 1.47x   | 1.54x   | 1.37x   | 1.46x   | 1.52x   | 2.02x   | 2.63x   | 2.60x   | 2.66x   | 2.65x   | 3.25x   | 3.39x   | 2.44x   | 2.15x   | 2.20x   | 2.52x   | 2.99x   | 2.94x    |
| ANSI-HASH-TABLE  | 0.85x   | 0.81x   | 0.89x   | 0.89x   | 0.86x   | 0.93x   | 0.96x   | 0.96x   | 0.92x   | 0.76x   | 0.68x   | 0.57x   | 0.70x   | 0.76x   | 0.62x   | 0.51x   | 0.46x   | 0.45x   | 0.47x   | 0.41x    |
| FSET             | 1.79x   | 1.81x   | 1.89x   | 1.98x   | 1.86x   | 2.08x   | 2.79x   | 4.04x   | 4.95x   | 5.47x   | 5.19x   | 5.09x   | 6.05x   | 6.11x   | 4.49x   | 3.74x   | 3.57x   | 3.82x   | 4.31x   | 4.08x    |
| FSET2            | 1.31x   | 1.26x   | 1.23x   | 1.26x   | 1.19x   | 1.22x   | 1.17x   | 1.19x   | 1.18x   | 1.19x   | 1.23x   | 1.27x   | 1.32x   | 1.20x   | 0.89x   | 0.88x   | 0.97x   | 1.03x   | 1.04x   | 0.97x    |
| CL-HAMT          | 2.29x   | 2.19x   | 2.24x   | 2.37x   | 2.19x   | 2.48x   | 3.36x   | 5.12x   | 7.73x   | 7.34x   | 7.55x   | 7.31x   | 8.78x   | 8.75x   | 5.96x   | 4.39x   |         |         |         |          |

BYTES
-----

| Implementation   | 32     | 64     | 128    | 256    | 512    | 1024   | 2048   | 4096   | 8192   | 16384  | 32768  | 65536  | 131072 | 262144 | 524288 | 1048576 | 2097152 | 4194304 | 8388608 | 16777216 |
|------------------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|---------|---------|---------|---------|----------|
| SYCAMORE-HAMT    | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B   |
| SYCAMORE-WB-TREE | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B   |
| ANSI-HASH-TABLE  | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B   |
| FSET             | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B   |
| FSET2            | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B  | 0.00 B   |
| CL-HAMT          | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B | 0.00 B  |         |         |         |          |

REMOVE
======

TIME
----

| Implementation   | 32      | 64      | 128     | 256     | 512     | 1024    | 2048    | 4096    | 8192    | 16384   | 32768   | 65536   | 131072  | 262144  | 524288  | 1048576 | 2097152 | 4194304 | 8388608 | 16777216 |
|------------------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|----------|
| SYCAMORE-HAMT    | 0.27 us | 0.29 us | 0.29 us | 0.30 us | 0.33 us | 0.33 us | 0.23 us | 0.23 us | 0.17 us | 0.18 us | 0.20 us | 0.20 us | 0.21 us | 0.22 us | 0.27 us | 0.31 us | 0.32 us | 0.32 us | 0.33 us | 0.38 us  |
| SYCAMORE-WB-TREE | 1.05x   | 1.09x   | 1.19x   | 1.43x   | 1.33x   | 1.31x   | 1.45x   | 1.27x   | 1.91x   | 1.74x   | 1.76x   | 1.70x   | 1.80x   | 1.92x   | 1.89x   | 1.94x   | 2.13x   | 2.41x   | 2.71x   | 2.70x    |
| FSET             | 1.31x   | 1.32x   | 1.39x   | 1.49x   | 1.51x   | 1.57x   | 2.00x   | 1.82x   | 2.54x   | 2.35x   | 2.30x   | 2.34x   | 2.46x   | 2.66x   | 2.63x   | 2.67x   | 2.91x   | 3.20x   | 3.53x   | 3.47x    |
| FSET2            | 1.06x   | 1.05x   | 1.04x   | 1.05x   | 1.03x   | 1.10x   | 1.11x   | 0.84x   | 1.02x   | 1.05x   | 1.08x   | 1.09x   | 1.10x   | 1.08x   | 0.98x   | 0.96x   | 0.99x   | 1.02x   | 0.99x   | 0.93x    |
| CL-HAMT          | 3.30x   | 3.56x   | 4.08x   | 4.18x   | 4.07x   | 4.63x   | 6.81x   | 6.54x   | 9.10x   | 8.61x   | 8.31x   | 8.07x   | 7.96x   | 7.44x   | 6.33x   | 5.86x   |         |         |         |          |

BYTES
-----

| Implementation   | 32      | 64      | 128     | 256     | 512     | 1024     | 2048     | 4096     | 8192     | 16384    | 32768    | 65536    | 131072   | 262144   | 524288   | 1048576  | 2097152  | 4194304  | 8388608  | 16777216 |
|------------------|---------|---------|---------|---------|---------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|----------|
| SYCAMORE-HAMT    | 45.22 B | 68.07 B | 74.39 B | 91.44 B | 98.90 B | 122.26 B | 143.23 B | 161.45 B | 172.42 B | 186.00 B | 203.91 B | 223.64 B | 237.02 B | 246.33 B | 258.58 B | 276.32 B | 295.42 B | 308.75 B | 318.49 B | 330.60 B |
| SYCAMORE-WB-TREE | 1.34x   | 1.09x   | 1.09x   | 1.07x   | 1.10x   | 1.05x    | 1.01x    | 1.00x    | 1.02x    | 1.02x    | 0.99x    | 0.96x    | 0.96x    | 0.97x    | 0.98x    | 0.96x    | 0.94x    | 0.94x    | 0.95x    | 0.95x    |
| FSET             | 0.83x   | 0.73x   | 0.77x   | 0.80x   | 0.89x   | 0.78x    | 0.83x    | 0.85x    | 0.87x    | 0.88x    | 0.87x    | 0.85x    | 0.85x    | 0.87x    | 0.88x    | 0.87x    | 0.86x    | 0.86x    | 0.87x    | 0.88x    |
| FSET2            | 1.27x   | 1.18x   | 1.07x   | 1.07x   | 1.13x   | 1.09x    | 1.12x    | 1.10x    | 1.08x    | 1.10x    | 1.11x    | 1.10x    | 1.08x    | 1.07x    | 1.09x    | 1.09x    | 1.09x    | 1.08x    | 1.07x    | 1.08x    |
| CL-HAMT          | 11.83x  | 9.58x   | 9.37x   | 8.08x   | 7.92x   | 7.15x    | 7.18x    | 6.86x    | 6.62x    | 6.43x    | 6.32x    | 6.27x    | 6.22x    | 6.11x    | 6.02x    | 5.97x    |          |          |          |          |

UNION
=====

TIME
----

| Implementation   | 32      | 64       | 128      | 256      | 512       | 1024      | 2048      | 4096      | 8192      | 16384     | 32768   | 65536   | 131072  | 262144  | 524288   | 1048576  | 2097152  | 4194304   | 8388608   | 16777216  |
|------------------|---------|----------|----------|----------|-----------|-----------|-----------|-----------|-----------|-----------|---------|---------|---------|---------|----------|----------|----------|-----------|-----------|-----------|
| SYCAMORE-HAMT    | 6.44 us | 13.63 us | 24.50 us | 46.54 us | 113.62 us | 231.70 us | 277.80 us | 339.67 us | 384.87 us | 640.03 us | 1.21 ms | 2.19 ms | 3.56 ms | 5.80 ms | 13.21 ms | 31.14 ms | 63.63 ms | 104.16 ms | 165.80 ms | 529.99 ms |
| SYCAMORE-WB-TREE | 1.35x   | 1.28x    | 1.50x    | 1.64x    | 1.35x     | 1.28x     | 1.44x     | 1.69x     | 2.42x     | 2.55x     | 2.67x   | 2.75x   | 3.25x   | 3.92x   | 3.36x    | 2.79x    | 2.76x    | 3.75x     | 4.69x     | 2.88x     |
| FSET             | 1.99x   | 2.03x    | 2.59x    | 2.94x    | 2.23x     | 2.36x     | 2.69x     | 3.76x     | 6.05x     | 6.83x     | 7.69x   | 7.76x   | 9.35x   | 11.40x  | 9.95x    | 8.40x    | 8.21x    | 11.08x    | 14.11x    | 8.84x     |
| FSET2            | 1.35x   | 1.25x    | 1.48x    | 1.55x    | 1.29x     | 1.28x     | 1.27x     | 1.52x     | 1.93x     | 2.13x     | 2.11x   | 2.09x   | 2.69x   | 2.96x   | 2.73x    | 2.34x    | 2.20x    | 2.71x     | 3.10x     | 2.61x     |

BYTES
-----

| Implementation   | 32       | 64       | 128      | 256       | 512       | 1024      | 2048      | 4096      | 8192       | 16384      | 32768      | 65536    | 131072   | 262144   | 524288    | 1048576   | 2097152   | 4194304   | 8388608    | 16777216   |
|------------------|----------|----------|----------|-----------|-----------|-----------|-----------|-----------|------------|------------|------------|----------|----------|----------|-----------|-----------|-----------|-----------|------------|------------|
| SYCAMORE-HAMT    | 0.00 B   | 0.00 B   | 0.00 B   | 0.00 B    | 0.00 B    | 0.00 B    | 31.96 KiB | 63.92 KiB | 127.74 KiB | 319.29 KiB | 734.82 KiB | 1.56 MiB | 2.81 MiB | 4.65 MiB | 10.18 MiB | 23.28 MiB | 50.26 MiB | 90.32 MiB | 149.20 MiB | 326.41 MiB |
| SYCAMORE-WB-TREE | 1.60 KiB | 3.46 KiB | 7.06 KiB | 14.38 KiB | 31.96 KiB | 63.94 KiB | 4.03x     | 4.40x     | 4.55x      | 3.70x      | 3.24x      | 2.99x    | 3.32x    | 4.02x    | 3.67x     | 3.21x     | 2.98x     | 3.31x     | 4.01x      | 3.66x      |
| FSET             | 955.08 B | 2.00 KiB | 4.00 KiB | 8.00 KiB  | 15.99 KiB | 31.98 KiB | 3.00x     | 3.50x     | 3.93x      | 3.16x      | 2.82x      | 2.60x    | 2.90x    | 3.52x    | 3.21x     | 2.81x     | 2.61x     | 2.90x     | 3.51x      | 3.21x      |
| FSET2            | 818.18 B | 1.93 KiB | 0.00 B   | 0.00 B    | 11.18 KiB | 31.96 KiB | 2.00x     | 1.50x     | 1.25x      | 1.60x      | 1.70x      | 1.42x    | 1.26x    | 1.17x    | 1.63x     | 1.70x     | 1.42x     | 1.26x     | 1.16x      | 1.63x      |

INTERSECTION
============

TIME
----

| Implementation   | 32      | 64       | 128      | 256      | 512      | 1024      | 2048      | 4096      | 8192      | 16384     | 32768     | 65536   | 131072  | 262144  | 524288  | 1048576  | 2097152  | 4194304  | 8388608  | 16777216  |
|------------------|---------|----------|----------|----------|----------|-----------|-----------|-----------|-----------|-----------|-----------|---------|---------|---------|---------|----------|----------|----------|----------|-----------|
| SYCAMORE-HAMT    | 5.15 us | 10.99 us | 19.55 us | 38.37 us | 99.48 us | 204.67 us | 230.83 us | 269.20 us | 306.00 us | 436.70 us | 802.70 us | 1.46 ms | 2.39 ms | 3.14 ms | 7.94 ms | 19.41 ms | 40.25 ms | 63.68 ms | 93.70 ms | 235.05 ms |
| SYCAMORE-WB-TREE | 1.40x   | 1.43x    | 1.62x    | 1.67x    | 1.45x    | 1.38x     | 1.52x     | 1.75x     | 2.39x     | 2.79x     | 2.99x     | 2.85x   | 3.57x   | 5.30x   | 4.06x   | 3.28x    | 3.15x    | 3.90x    | 5.05x    | 3.87x     |
| FSET             | 2.16x   | 2.13x    | 2.66x    | 2.93x    | 2.22x    | 2.10x     | 2.75x     | 4.30x     | 6.06x     | 9.12x     | 9.31x     | 9.34x   | 11.11x  | 16.56x  | 13.01x  | 10.62x   | 10.20x   | 12.85x   | 17.47x   | 13.99x    |
| FSET2            | 1.42x   | 1.39x    | 1.57x    | 1.48x    | 1.21x    | 1.28x     | 1.29x     | 1.33x     | 1.34x     | 1.48x     | 1.58x     | 2.01x   | 1.79x   | 2.15x   | 1.69x   | 1.73x    | 1.87x    | 1.96x    | 2.02x    | 1.98x     |

BYTES
-----

| Implementation   | 32     | 64       | 128      | 256      | 512       | 1024      | 2048      | 4096       | 8192      | 16384     | 32768      | 65536      | 131072     | 262144   | 524288   | 1048576  | 2097152  | 4194304   | 8388608   | 16777216  |
|------------------|--------|----------|----------|----------|-----------|-----------|-----------|------------|-----------|-----------|------------|------------|------------|----------|----------|----------|----------|-----------|-----------|-----------|
| SYCAMORE-HAMT    | 0.00 B | 0.00 B   | 0.00 B   | 0.00 B   | 0.00 B    | 0.00 B    | 0.00 B    | 0.00 B     | 31.96 KiB | 63.93 KiB | 121.45 KiB | 255.69 KiB | 607.34 KiB | 1.22 MiB | 2.22 MiB | 4.00 MiB | 8.47 MiB | 19.23 MiB | 39.49 MiB | 71.35 MiB |
| SYCAMORE-WB-TREE | 0.00 B | 750.00 B | 3.46 KiB | 7.99 KiB | 15.98 KiB | 31.97 KiB | 63.93 KiB | 159.84 KiB | 10.04x    | 10.55x    | 11.32x     | 10.85x     | 9.17x      | 8.94x    | 9.83x    | 10.91x   | 10.30x   | 9.07x     | 8.84x     | 9.78x     |
| FSET             | 0.00 B | 0.00 B   | 0.00 B   | 0.00 B   | 0.00 B    | 0.00 B    | 31.98 KiB | 63.97 KiB  | 4.00x     | 5.00x     | 5.27x      | 5.17x      | 4.43x      | 4.33x    | 4.77x    | 5.30x    | 5.01x    | 4.41x     | 4.30x     | 4.76x     |
| FSET2            | 0.00 B | 409.37 B | 1.20 KiB | 0.00 B   | 0.00 B    | 0.00 B    | 31.96 KiB | 95.89 KiB  | 5.00x     | 3.76x     | 4.47x      | 7.00x      | 6.52x      | 4.74x    | 3.59x    | 4.39x    | 6.64x    | 6.46x     | 4.68x     | 3.58x     |

DIFFERENCE
==========

TIME
----

| Implementation   | 32      | 64       | 128      | 256      | 512       | 1024      | 2048      | 4096      | 8192      | 16384     | 32768     | 65536   | 131072  | 262144  | 524288   | 1048576  | 2097152  | 4194304   | 8388608   | 16777216  |
|------------------|---------|----------|----------|----------|-----------|-----------|-----------|-----------|-----------|-----------|-----------|---------|---------|---------|----------|----------|----------|-----------|-----------|-----------|
| SYCAMORE-HAMT    | 5.66 us | 13.03 us | 26.48 us | 48.58 us | 103.27 us | 219.40 us | 277.20 us | 319.07 us | 378.27 us | 570.43 us | 993.60 us | 2.18 ms | 3.61 ms | 6.18 ms | 11.82 ms | 25.45 ms | 57.36 ms | 106.87 ms | 169.78 ms | 352.30 ms |
| SYCAMORE-WB-TREE | 1.45x   | 1.26x    | 1.37x    | 1.52x    | 1.49x     | 1.42x     | 1.38x     | 1.77x     | 2.33x     | 2.75x     | 3.19x     | 2.67x   | 3.15x   | 3.50x   | 3.61x    | 3.29x    | 2.91x    | 3.04x     | 3.86x     | 3.78x     |
| FSET             | 2.26x   | 2.11x    | 2.47x    | 2.51x    | 2.49x     | 2.24x     | 2.84x     | 4.12x     | 8.01x     | 7.99x     | 10.85x    | 8.23x   | 9.68x   | 11.18x  | 11.59x   | 10.72x   | 9.52x    | 10.54x    | 13.68x    | 14.42x    |
| FSET2            | 1.48x   | 1.43x    | 1.39x    | 1.46x    | 1.41x     | 1.36x     | 1.40x     | 1.74x     | 2.59x     | 2.42x     | 3.11x     | 2.68x   | 3.21x   | 3.17x   | 3.19x    | 3.13x    | 3.04x    | 4.00x     | 4.65x     | 3.61x     |

BYTES
-----

| Implementation   | 32       | 64       | 128      | 256       | 512       | 1024      | 2048       | 4096      | 8192      | 16384     | 32768      | 65536      | 131072   | 262144   | 524288   | 1048576  | 2097152   | 4194304   | 8388608   | 16777216   |
|------------------|----------|----------|----------|-----------|-----------|-----------|------------|-----------|-----------|-----------|------------|------------|----------|----------|----------|----------|-----------|-----------|-----------|------------|
| SYCAMORE-HAMT    | 0.00 B   | 0.00 B   | 0.00 B   | 0.00 B    | 0.00 B    | 0.00 B    | 0.00 B     | 31.97 KiB | 63.93 KiB | 95.85 KiB | 223.54 KiB | 671.14 KiB | 1.62 MiB | 2.63 MiB | 3.87 MiB | 7.08 MiB | 21.64 MiB | 52.65 MiB | 84.82 MiB | 124.28 MiB |
| SYCAMORE-WB-TREE | 170.55 B | 1.60 KiB | 4.00 KiB | 7.99 KiB  | 15.99 KiB | 31.97 KiB | 69.27 KiB  | 5.33x     | 5.67x     | 7.80x     | 6.81x      | 4.56x      | 3.69x    | 4.56x    | 6.21x    | 6.78x    | 4.44x     | 3.65x     | 4.53x     | 6.19x      |
| FSET             | 1.23 KiB | 3.07 KiB | 7.46 KiB | 15.99 KiB | 31.98 KiB | 63.96 KiB | 159.92 KiB | 10.44x    | 10.79x    | 14.86x    | 12.83x     | 8.61x      | 6.96x    | 8.61x    | 11.71x   | 12.80x   | 8.38x     | 6.89x     | 8.55x     | 11.68x     |
| FSET2            | 1.03 KiB | 3.33 KiB | 7.06 KiB | 7.99 KiB  | 15.97 KiB | 31.92 KiB | 127.82 KiB | 8.00x     | 6.50x     | 7.33x     | 7.10x      | 6.37x      | 5.28x    | 5.05x    | 5.87x    | 7.05x    | 6.19x     | 5.21x     | 5.02x     | 5.85x      |
