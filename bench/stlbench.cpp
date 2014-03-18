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
#include <algorithm>

using namespace std;


/* Benchmark the STL for operations we care about */
int main(int argc, char **argv) {
    vector<int> dat;
    int d;

    // read data
    FILE * f = fopen("/tmp/sycamore-bench.dat", "r");
    while( 1 == fscanf(f, "%d", &d) ) {
        dat.push_back(d);
    }
    fclose(f);

    // build map
    clock_t t0 = clock();
    set<int> tree;
    for( vector<int>::iterator p = dat.begin(); p != dat.end(); p++ ) {
        tree.insert(*p);
    }
    clock_t t1 = clock();
    printf("Creation Time for %d elements: %fs\n", dat.size(), ((double)(t1-t0)) / CLOCKS_PER_SEC);

    // search map
    vector<int> dat1(dat.size());
    t0 = clock();
    for( size_t i = 0; i < dat.size(); i++ ) {
        dat1[i] = (tree.find(dat[i]) != tree.end());
    }
    t1 = clock();
    printf("Search Time for %d elements: %fs\n", dat.size(), ((double)(t1-t0)) / CLOCKS_PER_SEC);

    // // union maps
    set<int> tree0, tree1;
    for( size_t i = 0; i < dat.size() / 2; i++ )
        tree0.insert(dat[i]);
    for( size_t i = dat.size()/2; i < dat.size() ; i++ )
        tree1.insert(dat[i]);

    vector<int> vec_union(dat.size());
    t0 = clock();
    vector<int>::iterator it = set_union( tree0.begin(), tree0.end(),
                                          tree1.begin(), tree1.end(),
                                          vec_union.begin() );
    set<int> tree_union(vec_union.begin(), it );
    t1 = clock();
    printf("Union Time for %d elements: %fs\n", dat.size(), ((double)(t1-t0)) / CLOCKS_PER_SEC);
}
