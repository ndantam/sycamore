/*
* Copyright (c) 2012, Georgia Tech Research Corporation
* All rights reserved.
*
* Author(s): Neil T. Dantam <ntd@gatech.edu>
* Georgia Tech Humanoid Robotics Lab
* Under Direction of Prof. Mike Stilman
*
* This file is provided under the following "BSD-style" License:
*
*   Redistribution and use in source and binary forms, with or
*   without modification, are permitted provided that the following
*   conditions are met:
*   * Redistributions of source code must retain the above
*     copyright notice, this list of conditions and the following
*     disclaimer.
*   * Redistributions in binary form must reproduce the above
*     copyright notice, this list of conditions and the following
*     disclaimer in the documentation and/or other materials
*     provided with the distribution.
*
*   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
*   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
*   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
*   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
*   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
*   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
*   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
*   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
*   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <set>
#include <vector>
#include <cstdio>
#include <time.h>
#include <stdarg.h>
#include <algorithm>

/* Benchmark STL sets.
 *
 * Note: it is difficult to measure the performance cost of heap
 * allocation here.  GLIBC malloc() will be very fast. GLIBC free()
 * will usually be very fast because it batches work.  Occasionally,
 * free() will reorganize the heap (and be very slow).
 */

using namespace std;

#define MODULO(a,b) (((a) % (b)) + (b)) % (b);

static struct timespec aa_tick_tock_start;

static inline struct timespec
aa_tm_make( time_t sec, long nsec ) {
    struct timespec t;
    t.tv_sec = sec;
    t.tv_nsec = nsec;
    return t;
}

static inline struct timespec
aa_tm_make_norm( time_t sec, long nsec ) {
    long nsp = MODULO( (long)nsec, (long)1000000000 );
    return aa_tm_make( sec + (nsec - nsp)/1e9, nsp );
}

static inline struct timespec
aa_tm_sub( const struct timespec a, const struct timespec b ) {
    return aa_tm_make_norm( a.tv_sec - b.tv_sec,
                            a.tv_nsec - b.tv_nsec );
}

static void tic()
{
    clock_gettime( CLOCK_MONOTONIC, &aa_tick_tock_start );
}

struct timespec toc( const char fmt[], ...)
 {
    struct timespec now;
    clock_gettime( CLOCK_MONOTONIC, &now );
    struct timespec t = aa_tm_sub( now, aa_tick_tock_start );
    double dt = (double)t.tv_sec + (double)t.tv_nsec / 1e9;

    va_list argp;
    va_start( argp, fmt );
    vfprintf( stderr, fmt, argp );
    va_end( argp );


    fprintf( stderr, ": %f s\n", (double)t.tv_sec + (double)t.tv_nsec/1e9 );
    return t;
}



static void load_data( const char *fname, vector<int> &dat ) {
    FILE * f = fopen(fname, "r");
    int d;
    while( 1 == fscanf(f, "%d", &d) ) {
        dat.push_back(d);
    }
    fclose(f);
}

static void build( vector<int> &dat, set<int> &tree ) {
    for( vector<int>::iterator p = dat.begin(); p != dat.end(); p++ ) {
        tree.insert(*p);
    }
}


static void insert( set<int> &tree1, const set<int> &tree2 ) {
    for( set<int>::iterator p = tree2.begin(); p != tree2.end(); p++ ) {
        tree1.insert(*p);
    }
}


static void do_tree_union( const set<int> &tree1, const set<int> &tree2, set<int> tree_union ) {

    vector<int> vec_union(tree1.size() + tree2.size() );
    vector<int>::iterator it = set_union( tree1.begin(), tree1.end(),
                                          tree2.begin(), tree2.end(),
                                          vec_union.begin() );
    tree_union = set<int>(vec_union.begin(), it );
}

static void do_tree_intersection( const set<int> &tree1, const set<int> &tree2, set<int> tree ) {
    vector<int> vec(std::min(tree1.size() , tree2.size()) );
    vector<int>::iterator it = set_intersection( tree1.begin(), tree1.end(),
                                                 tree2.begin(), tree2.end(),
                                                 vec.begin() );
    tree = set<int>(vec.begin(), it );
}

/* Benchmark the STL for operations we care about */
int main(int argc, char **argv) {
    int d;

    printf("CLOCKS_PER_SEC: %lu\n", CLOCKS_PER_SEC);

    // read data
    vector<int> dat1, dat2;
    load_data("/tmp/sycamore-bench-1.dat", dat1);
    load_data("/tmp/sycamore-bench-2.dat", dat2);

    printf("data size 1: %lu\n", dat1.size() );
    printf("data size 2: %lu\n", dat2.size() );

    // build map
    set<int> tree1, tree2;
    tic();
    build( dat1, tree1 );
    toc("build 1");

    tic();
    build( dat2, tree2 );
    toc("build 2");

    printf("\n");
    printf("set size 1: %lu\n", tree1.size() );
    printf("set size 2: %lu\n", tree2.size() );

    vector<int> vec1(tree1.begin(), tree1.end());
    vector<int> vec2(tree2.begin(), tree2.end());

    printf("vec size 1: %lu\n", vec1.size() );
    printf("vec size 2: %lu\n", vec2.size() );

    printf("t1_0: %d, v1_0: %d\n", *tree1.begin(), *vec1.begin());
    printf("t2_0: %d, v2_0: %d\n", *tree2.begin(), *vec2.begin());

    printf("t1_1: %d, v1_1: %d\n", *(--tree1.end()), *(--vec1.end()));
    printf("t2_1: %d, v2_1: %d\n", *(--tree2.end()), *(--vec2.end()));

    printf("\n");
    // Insert
    {
        set<int> copy1 = tree1;
        set<int> copy2 = tree2;
        tic();
        insert( copy1, tree2 );
        toc("Insert 2 into 1");
        tic();
        insert( copy2, tree1 );
        toc("Insert 1 into 2");
    }

    printf("\n");
    // Union
    {
        set<int> u1, u2;
        {
            tic();
            do_tree_union( tree1, tree2, u1 );
            toc("Tree Union 2 into 1");

            tic();
            do_tree_union( tree2, tree1, u2 );
            toc("Tree Union 1 into 2");
        }

        {
            tic();
            vector<int> dat(tree1.size() + tree2.size() );
            vector<int>::iterator it1 = set_union( vec1.begin(), vec1.end(),
                                                   vec2.begin(), vec2.end(),
                                                   dat.begin() );
            toc( "Vec Union 1 2" );
        }

        tic();
        {
            vector<int> dat(tree1.size() + tree2.size() );
            vector<int>::iterator it2 = set_union( vec2.begin(), vec2.end(),
                                                   vec1.begin(), vec1.end(),
                                                   dat.begin() );
            toc( "Vec Union 2 1" );
        }
    }

    printf("\n");
    // Intersection
    {
        set<int> u1, u2;
        {
            tic();
            do_tree_intersection( tree1, tree2, u1 );
            toc("Tree Intersection 1 2");

            tic();
            do_tree_intersection( tree2, tree1, u2 );
            toc("Tree Intersection 2 1");
        }

        {
            tic();
            vector<int> dat(std::min(tree1.size() , tree2.size() ));
            vector<int>::iterator it1 = set_intersection( vec1.begin(), vec1.end(),
                                                          vec2.begin(), vec2.end(),
                                                          dat.begin() );
            toc( "Vec Intersection 1 2" );
        }

        tic();
        {
            vector<int> dat(std::min(tree1.size() , tree2.size() ));
            vector<int>::iterator it2 = set_intersection( vec2.begin(), vec2.end(),
                                                          vec1.begin(), vec1.end(),
                                                          dat.begin() );
            toc( "Vec Intersection 2 1" );
        }
    }

    printf("\n");
    // difference
    {
        {
            tic();
            vector<int> dat(tree1.size());
            vector<int>::iterator it1 = set_difference( vec1.begin(), vec1.end(),
                                                        vec2.begin(), vec2.end(),
                                                        dat.begin() );
            set<int> d(dat.begin(), it1);
            toc( "Set Difference 1 2" );
        }

        tic();
        {
            vector<int> dat(tree1.size());
            vector<int>::iterator it2 = set_difference( vec2.begin(), vec2.end(),
                                                        vec1.begin(), vec1.end(),
                                                        dat.begin() );
            set<int> d(dat.begin(), it2);
            toc( "Set Difference 2 1" );
        }

        {
            tic();
            vector<int> dat(tree1.size());
            vector<int>::iterator it1 = set_difference( vec1.begin(), vec1.end(),
                                                        vec2.begin(), vec2.end(),
                                                        dat.begin() );
            toc( "Vec Difference 1 2" );
        }

        tic();
        {
            vector<int> dat(tree1.size());
            vector<int>::iterator it2 = set_difference( vec2.begin(), vec2.end(),
                                                        vec1.begin(), vec1.end(),
                                                        dat.begin() );
            toc( "Vec Difference 2 1" );
        }
    }


    // // search map
    // vector<int> dat1(dat.size());
    // t0 = clock();
    // for( size_t i = 0; i < dat.size(); i++ ) {
    //     dat1[i] = (tree.find(dat[i]) != tree.end());
    // }
    // t1 = clock();
    // printf("Search Time for %d elements: %fs\n", dat.size(), ((double)(t1-t0)) / CLOCKS_PER_SEC);

    // // // union maps
    // set<int> tree0, tree1;
    // for( size_t i = 0; i < dat.size() / 2; i++ )
    //     tree0.insert(dat[i]);
    // for( size_t i = dat.size()/2; i < dat.size() ; i++ )
    //     tree1.insert(dat[i]);

    // vector<int> vec_union(dat.size());
    // t0 = clock();
    // vector<int>::iterator it = set_union( tree0.begin(), tree0.end(),
    //                                       tree1.begin(), tree1.end(),
    //                                       vec_union.begin() );
    // set<int> tree_union(vec_union.begin(), it );
    // t1 = clock();
    // printf("Union Time for %d elements: %fs\n", dat.size(), ((double)(t1-t0)) / CLOCKS_PER_SEC);
}
