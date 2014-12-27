/* Let's call a lattice point (x, y) inadmissible if x, y and x + y are all
positive perfect squares. For example, (9, 16) is inadmissible, while (0, 4),
(3, 1) and (9, 4) are not.

Consider a path from point (x1, y1) to point (x2, y2) using only unit steps
north or east. Let's call such a path admissible if none of its intermediate
points are inadmissible.

P(n) be the number of admissible paths from (0, 0) to (n, n).
It can be verified that P(5) = 252, P(16) = 596994440 and
P(1000) mod 1 000 000 007 = 341920854.

Find P(10 000 000) mod 1 000 000 007.

Note: this solution requires FLINT (http://www.flintlib.org/)
*/

#include <vector>
#include <map>
#include <cmath>

#include <stdlib.h>
#include <stdio.h>
#include "flint.h"
#include "fmpz.h"
#include "fmpz_mat.h"
#include "arith.h"

using namespace std;

const long N = 10000000;
const long K = 1000000007;

typedef pair<long, long> point;

point LIMIT_PT = point(N, N);
point ORIGIN = point(0, 0);

//////////////////////////////////////////

void fmpz_mul_mod(fmpz_t& result, const fmpz_t& a, const fmpz_t& b) {
	fmpz_mul(result, a, b);
	fmpz_mod_ui(result, result, K);
}

bool geq_ui(fmpz_t& a, long b) {
	return fmpz_cmp_ui(a, b) >= 0;
}

bool leq_ui(fmpz_t& a, long b) {
	return fmpz_cmp_ui(a, b) <= 0;
}

void fmpz_add_mod(fmpz_t& result, const fmpz_t& a, const fmpz_t& b) {
	fmpz_add(result, a, b);
	if (geq_ui(result, K)) {
		fmpz_sub_ui(result, result, K);
	}
}

void fmpz_sub_mod(fmpz_t& result, const fmpz_t& a, const fmpz_t& b) {
	fmpz_sub(result, a, b);
	if (leq_ui(result, 0)) {
		fmpz_add_ui(result, result, K);
	}
}

long div(long numerator_long, long denominator_long) {
	fmpz_t numerator;
	fmpz_init(numerator);
	fmpz_set_ui(numerator, numerator_long);

	fmpz_t denominator;
	fmpz_init(denominator);
	fmpz_set_ui(denominator, denominator_long);
	fmpz_t KK;
	fmpz_init(KK);
	fmpz_set_ui(KK, K);
	fmpz_invmod(denominator, denominator, KK);

	fmpz_t result;
	fmpz_init(result);
	fmpz_mul_mod(result, numerator, denominator);
	return fmpz_get_ui(result);
}

const int ADD = 0;
const int SUB = 1;
const int MUL = 2;
long op(long a_long, long b_long, int op) {
	fmpz_t a;
	fmpz_init(a);
	fmpz_set_ui(a, a_long);

	fmpz_t b;
	fmpz_init(b);
	fmpz_set_ui(b, b_long);

	if (op == ADD) fmpz_add_mod(a, a, b);
	if (op == SUB) fmpz_sub_mod(a, a, b);
	if (op == MUL) fmpz_mul_mod(a, a, b);

	return fmpz_get_ui(a);
}

long mul(long a, long b) {
	return op(a, b, MUL);
}

long add(long a, long b) {
	return op(a, b, ADD);
}

long sub(long a, long b) {
	return op(a, b, SUB);
}

//////////////////////////////////////////

long abs(long n) {
	return (n < 0) ? (-1 * n) : n;
}

bool is_square(long n) {
	return sqrt(n) == floor(sqrt(n));
}

// largest it will ever be called with is N*2, which is has << 64 bits
map<long, long> fact_memo = map<long, long>();
long fact(long n) {
	return fact_memo[n];
}

void fact_init() {
	fact_memo[0] = 1;
	for (long i=1; i<=2*N; i++) {
		fact_memo[i] = mul(i, fact(i - 1));
	}
}

long choose(long n, long k) {
	return div(fact(n), mul(fact(k), fact(n-k)));
}

bool pt_eq(point& a, point& b) {
	return (a.first == b.first)
		&& (a.second == b.second);
}

bool pt_less(point& a, point& b) {
	return (a.first <= b.first)
		&& (a.second <= b.second)
		&& !pt_eq(a, b);
}

vector<long> gen_squares() {
	vector<long> squares;
	for (long i=1; i<=sqrt(N); i++) {
		// N == 10 000 000, which is ~45 bits, so no % K needed
		squares.push_back(pow(i, 2));
	}
	return squares;
}

vector<point> gen_inadmissable_points() {
	vector<long> squares = gen_squares();

	vector<point> inadmissable_points;
	for (size_t i=0; i<squares.size(); i++) {
		for (size_t j=0; j<squares.size(); j++) {
			// 30 bits + 30 bits <= 31 bits, so no % K needed
			if (is_square(squares[i] + squares[j])) {
				inadmissable_points.push_back(point(squares[i], squares[j]));
			}
		}
	}
	return inadmissable_points;
}

long count_num_paths_from_pt_to_pt(point& p1, point& p2) {
	long num_steps = abs(p1.first - p2.first) + abs(p1.second - p2.second);
	return choose(num_steps, abs(p1.first - p2.first));
}

point swap(point& p) {
	return point(p.second, p.first);
}

map<point, long> dp_cache;
long count_num_admissible_paths_to(
	point& pt,
	vector<point>& inadmissable_points)
{
	if (dp_cache.find(pt) != dp_cache.end()) {
		return dp_cache[pt];
	}

	long npaths = count_num_paths_from_pt_to_pt(LIMIT_PT, pt);
	for (size_t j=0; j<inadmissable_points.size(); j++) {
		point inad_pt = inadmissable_points[j];
		if (!pt_less(pt, inad_pt)) {
			continue;
		}

		npaths = sub(
			npaths,
			mul(
				count_num_admissible_paths_to(inad_pt, inadmissable_points),
				count_num_paths_from_pt_to_pt(inad_pt, pt)
			)
		);
	}

	dp_cache[pt] = npaths;
	dp_cache[swap(pt)] = npaths;
	return npaths;
}

int main(int argc, char const *argv[]) {
	fact_init();

	vector<point> inadmissable_points = gen_inadmissable_points();

	long npaths = count_num_admissible_paths_to(ORIGIN, inadmissable_points);

	printf("Number of admissable paths from (%lu, %lu) to origin: %lu\n",
		N, N, npaths);

	return 0;
}
