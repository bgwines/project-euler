/* Criss Cross
Problem 166
A 4x4 grid is filled with digits d, 0 ≤ d ≤ 9.

It can be seen that in the grid

6 3 3 0
5 0 4 3
0 7 1 4
1 2 4 5

the sum of each row and each column has the value 12. Moreover the sum of each diagonal is also 12.

In how many ways can you fill a 4x4 grid with the digits d, 0 ≤ d ≤ 9 so that each row, each column, and both diagonals have the same sum?

problem166/ [master] > g++ problem166.cpp -o problem166; time ./problem166
7130034
real    0m18.497s
user    0m18.433s
sys     0m0.033s
*/

#include <cmath>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

const size_t A1 = 0;
const size_t A2 = 1;
const size_t A3 = 2;
const size_t A4 = 3;
const size_t A5 = 4;
const size_t A6 = 5;
const size_t A7 = 6;
const size_t A10 = 7;
const size_t A11 = 8;

long count_matrices() {
    long num_viable_matrices = 0;
    for (short a1 = 0; a1 <= 9; a1++) {
    for (short a2 = 0; a2 <= 9; a2++) {
    for (short a3 = 0; a3 <= 9; a3++) {
    for (short a4 = 0; a4 <= 9; a4++) {
    for (short a5 = 0; a5 <= 9; a5++) {
    for (short a6 = 0; a6 <= 9; a6++) {
    for (short a7 = 0; a7 <= 9; a7++) {
    for (short a10 = 0; a10 <= 9; a10++) {
    for (short a11 = 0; a11 <= 9; a11++) {
        short s = a1 + a2 + a3 + a4;

        short a8  = s - (a5 + a6  + a7 );
        short a13 = s - (a7 + a10 + a4 );
        short a9  = s - (a1 + a5  + a13);
        short a12 = s - (a9 + a10 + a11);
        short a14 = s - (a2 + a6  + a10);
        short a15 = s - (a3 + a7  + a11);
        short a16 = s - (a1 + a6  + a11);

        // horizontal
        short s2 = a5  + a6  + a7  + a8;
        short s3 = a9  + a10 + a11 + a12;
        short s4 = a13 + a14 + a15 + a16;
        // vertical
        short s5 = a1  + a5  + a9  + a13;
        short s6 = a2  + a6  + a10 + a14;
        short s7 = a3  + a7  + a11 + a15;
        short s8 = a4  + a8  + a12 + a16;
        // diagonal
        short s9  = a1  + a6  + a11 + a16;
        short s10 = a4  + a7  + a10 + a13;

        if (   (s == s2)
            && (s == s3)
            && (s == s4)
            && (s == s5)
            && (s == s6)
            && (s == s7)
            && (s == s8)
            && (s == s9)
            && (s == s10)
            && ((0 <= a8 ) && (a8  <= 9))
            && ((0 <= a9 ) && (a9  <= 9))
            && ((0 <= a12) && (a12 <= 9))
            && ((0 <= a13) && (a13 <= 9))
            && ((0 <= a14) && (a14 <= 9))
            && ((0 <= a15) && (a15 <= 9))
            && ((0 <= a16) && (a16 <= 9)) )
        {
            num_viable_matrices++;
        }
    }}}}}}}}}
    return num_viable_matrices;
}

int main(int argc, char const *argv[]) {
    printf("%lu", count_matrices());
}
