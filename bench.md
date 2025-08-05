Sycamore Persistent Set Benchmarks
==================================

This document contains timing benchmarks of the persistent set implementations in Sycamore, FSET, and CL-HAMT, along with a baseline of (mutable) ANSI CL Hash-Tables. The performance ranking is as follows and is generally consistent across the tested operations:

1. ANSI CL Hash-Tables (fastest construction and find, but mutable only)
2. Sycamore HAMTs (fastest persistent set)
3. Sycamore WB-Trees
4. FSet
5. CL-HAMT

Test Setup
==========

Data
----
- Operations performed for random sets of the stated sizes
- 30 samples (random sets) per size
- Results for SYCAMORE-HAMT are shown as mean run time
- Results for others are shown as relative run time (/ TIME-OTHER TIME-SYCAMORE-HAMT), equivalent to the speedup of SYCAMORE-HAMT over OTHER

System
------
- Date: 2025-08-05, 13:00
- Uname: Linux farnsworth 6.1.0-37-amd64 #1 SMP PREEMPT_DYNAMIC Debian 6.1.140-1 (2025-05-22) x86_64 GNU/Linux
- CPU: Intel(R) Xeon(R) CPU E3-1275 v6 @ 3.80GHz
- Lisp Implementation: SBCL 2.5.2

CONSTRUCT
=========

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 3.76 us | 7.11 us | 13.77 us | 30.25 us | 61.22 us | 132.38 us | 227.85 us | 399.50 us | 808.80 us | 1.64 ms | 3.80 ms | 8.82 ms | 16.40 ms | 37.05 ms | 120.60 ms | 248.30 ms |
| SYCAMORE-WB-TREE | 1.77x | 2.06x | 2.52x | 3.05x | 3.39x | 3.29x | 4.26x | 5.05x | 5.23x | 5.74x | 5.57x | 5.33x | 5.42x | 4.70x | 3.53x | 4.64x |
| ANSI-HASH-TABLE | 0.83x | 1.04x | 1.09x | 1.03x | 1.00x | 0.92x | 0.92x | 0.92x | 0.86x | 0.82x | 0.72x | 0.62x | 0.72x | 0.63x | 0.46x | 0.61x |
| FSET | 3.10x | 2.86x | 3.72x | 3.97x | 4.94x | 5.13x | 6.36x | 7.57x | 7.67x | 9.06x | 8.88x | 8.13x | 8.39x | 7.86x | 5.77x | 7.32x |
| CL-HAMT | 15.87x | 13.86x | 14.31x | 13.64x | 14.67x | 12.92x | 15.70x | 18.15x | 18.71x | 18.40x | 17.13x | 18.69x | 21.85x | 21.80x | 15.14x | 14.67x |

INSERT
======

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 0.16 us | 0.19 us | 0.22 us | 0.25 us | 0.27 us | 0.32 us | 0.33 us | 0.33 us | 0.33 us | 0.40 us | 0.44 us | 0.50 us | 0.42 us | 0.36 us | 0.38 us | 0.56 us |
| SYCAMORE-WB-TREE | 1.26x | 1.30x | 1.28x | 1.36x | 1.42x | 1.29x | 1.37x | 1.40x | 1.40x | 1.40x | 1.38x | 1.30x | 1.53x | 1.74x | 1.87x | 1.87x |
| FSET | 2.05x | 2.05x | 1.99x | 2.09x | 1.93x | 2.07x | 2.12x | 2.11x | 2.19x | 2.16x | 2.20x | 2.02x | 2.38x | 2.82x | 2.97x | 2.87x |
| CL-HAMT | 8.52x | 7.96x | 7.64x | 6.85x | 6.01x | 5.92x | 5.78x | 6.26x | 6.45x | 5.38x | 5.37x | 4.27x | 4.72x | 5.40x | 5.31x | 4.37x |

FIND
====

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 0.03 us | 0.03 us | 0.04 us | 0.04 us | 0.05 us | 0.06 us | 0.06 us | 0.05 us | 0.04 us | 0.05 us | 0.07 us | 0.07 us | 0.07 us | 0.08 us | 0.12 us | 0.20 us |
| SYCAMORE-WB-TREE | 1.63x | 2.02x | 2.37x | 2.78x | 2.34x | 2.31x | 2.05x | 2.80x | 3.39x | 2.77x | 2.27x | 2.35x | 2.82x | 2.79x | 2.23x | 2.19x |
| ANSI-HASH-TABLE | 0.60x | 0.75x | 0.71x | 0.88x | 0.57x | 0.70x | 0.66x | 0.84x | 0.88x | 0.67x | 0.60x | 0.50x | 0.66x | 0.62x | 0.53x | 0.51x |
| FSET | 4.66x | 5.45x | 5.38x | 6.02x | 4.83x | 4.65x | 4.90x | 6.71x | 8.89x | 6.86x | 5.05x | 5.32x | 5.56x | 5.54x | 4.18x | 3.98x |
| CL-HAMT | 8.25x | 7.64x | 8.07x | 9.01x | 5.89x | 7.10x | 7.74x | 9.50x | 11.48x | 9.04x | 8.07x | 7.84x | 8.29x | 7.85x | 5.40x | 4.41x |

REMOVE
======

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 0.08 us | 0.10 us | 0.11 us | 0.12 us | 0.14 us | 0.15 us | 0.16 us | 0.15 us | 0.16 us | 0.17 us | 0.19 us | 0.24 us | 0.24 us | 0.24 us | 0.25 us | 0.39 us |
| SYCAMORE-WB-TREE | 1.61x | 1.53x | 1.57x | 1.76x | 1.82x | 1.75x | 1.75x | 1.95x | 2.00x | 1.93x | 1.84x | 1.75x | 1.80x | 2.03x | 2.12x | 2.08x |
| FSET | 2.52x | 2.66x | 2.72x | 2.96x | 2.93x | 2.73x | 2.67x | 3.03x | 3.15x | 3.02x | 2.98x | 2.76x | 2.80x | 3.10x | 3.30x | 3.03x |
| CL-HAMT | 9.99x | 8.91x | 10.21x | 9.70x | 8.75x | 9.31x | 9.57x | 11.13x | 11.10x | 10.85x | 10.32x | 8.39x | 7.55x | 7.53x | 7.36x | 5.85x |

UNION
=====

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 1.29 us | 3.13 us | 4.99 us | 8.88 us | 24.82 us | 53.57 us | 99.63 us | 154.50 us | 217.87 us | 492.47 us | 1.09 ms | 2.09 ms | 3.73 ms | 5.56 ms | 13.42 ms | 34.44 ms |
| SYCAMORE-WB-TREE | 2.55x | 2.36x | 2.74x | 3.53x | 2.88x | 2.71x | 2.42x | 2.86x | 3.91x | 3.29x | 3.03x | 3.00x | 3.22x | 4.23x | 3.55x | 3.05x |
| FSET | 9.20x | 6.34x | 9.34x | 12.47x | 8.62x | 8.05x | 8.08x | 10.28x | 13.04x | 13.63x | 12.47x | 11.16x | 11.51x | 16.24x | 12.39x | 10.68x |

INTERSECTION
============

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 0.81 us | 1.98 us | 3.23 us | 6.28 us | 16.37 us | 39.40 us | 76.77 us | 122.77 us | 150.97 us | 315.47 us | 692.83 us | 1.46 ms | 2.22 ms | 3.23 ms | 8.37 ms | 21.73 ms |
| SYCAMORE-WB-TREE | 2.27x | 2.33x | 3.25x | 3.87x | 3.50x | 2.90x | 2.51x | 2.75x | 4.16x | 3.63x | 3.42x | 3.19x | 4.18x | 5.40x | 4.32x | 3.55x |
| FSET | 11.05x | 8.17x | 10.12x | 11.36x | 9.81x | 7.50x | 9.05x | 10.40x | 16.92x | 13.27x | 11.94x | 10.27x | 16.82x | 19.92x | 15.07x | 12.26x |

DIFFERENCE
==========

| Implementation | 32 | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 | 65536 | 131072 | 262144 | 524288 | 1048576 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| SYCAMORE-HAMT | 1.02 us | 2.80 us | 4.97 us | 8.19 us | 23.20 us | 53.77 us | 102.10 us | 160.03 us | 221.77 us | 415.77 us | 905.03 us | 2.04 ms | 3.88 ms | 6.16 ms | 12.37 ms | 29.32 ms |
| SYCAMORE-WB-TREE | 2.67x | 2.45x | 2.86x | 3.67x | 2.95x | 2.56x | 2.22x | 2.78x | 3.49x | 3.60x | 3.30x | 2.82x | 2.98x | 3.84x | 3.68x | 3.25x |
| FSET | 12.25x | 9.28x | 8.65x | 12.30x | 10.24x | 8.44x | 9.06x | 9.71x | 13.17x | 14.25x | 14.11x | 11.36x | 11.62x | 13.91x | 14.11x | 12.76x |