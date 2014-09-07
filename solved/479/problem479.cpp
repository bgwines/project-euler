/* Note: this solution requires FLINT (http://www.flintlib.org/)

   Problem solved together with Alex Quach */

#include <cmath>

#include <stdlib.h>
#include <stdio.h>
#include "flint.h"
#include "fmpz.h"
#include "fmpz_mat.h"
#include "arith.h"

const long K = 1000000007;
const long N = pow(10, 6);

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
const int POW = 3;
long op(long a_long, long b_long, int op) {
	fmpz_t KK;
	fmpz_init(KK);
	fmpz_set_ui(KK, K);

	fmpz_t a;
	fmpz_init(a);
	fmpz_set_ui(a, a_long);

	fmpz_t b;
	fmpz_init(b);
	fmpz_set_ui(b, b_long);

	if (op == ADD) fmpz_add_mod(a, a, b);
	if (op == SUB) fmpz_sub_mod(a, a, b);
	if (op == MUL) fmpz_mul_mod(a, a, b);
	if (op == POW) fmpz_powm_ui(a, a, fmpz_get_ui(b), KK);

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

long mul3(long a, long b, long c) {
	return mul(mul(a, b), c);
}

long pow_mod(long base, long exp) {
	return op(base, exp, POW);
}

////////////////////////////////////////////////

// Z(k) = (a_k + b_k) * (b_k + c_k) * (c_k + a_k)
long Z(long k) {
	return sub(1, mul(k, k));
}

long geosum(long k, long n) {
	long numerator = sub(1, pow_mod(Z(k), n+1));
	long denominator = sub(1, Z(k));
	return div(numerator, denominator);
}

long S(long n) {
	long sum = 0;
	for (int k=1; k<=n; k++) {
		sum = add(sum, geosum(k, n));
	}
	return sub(sum, n);
}

int main(int argc, char const *argv[]) {
	printf("S(%lu) = %lu\n", N, S(N));
	return 0;
}
