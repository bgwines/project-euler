/* Uphill paths
Problem 411
Let n be a positive integer. Suppose there are stations at the coordinates (x, y) = (2^i mod n, 3^i mod n) for 0 ≤ i ≤ 2*n. We will consider stations with the same coordinates as the same station.

We wish to form a path from (0, 0) to (n, n) such that the x and y coordinates never decrease.
Let S(n) be the maximum number of stations such a path can pass through.

For example, if n = 22, there are 11 distinct stations, and a valid path can pass through at most 5 stations. Therefore, S(22) = 5. The case is illustrated below, with an example of an optimal path:

It can also be verified that S(123) = 14 and S(10000) = 48.

Find ∑ S(k^5) for 1 ≤ k ≤ 30.

Note: this solution requires FLINT (http://www.flintlib.org/)
*/

#include <set>
#include <map>
#include <vector>

#include <cmath>

#include <stdlib.h>
#include <stdio.h>
#include "flint.h"
#include "fmpz.h"
#include "fmpz_mat.h"
#include "arith.h"

using namespace std;

typedef pair<long, long> point;

long exp(long a_long, long b_long, long N) {
    fmpz_t a;
    fmpz_init(a);
    fmpz_set_ui(a, a_long);

    fmpz_t b;
    fmpz_init(b);
    fmpz_set_ui(b, b_long);

    fmpz_t modulus;
    fmpz_init(modulus);
    fmpz_set_ui(modulus, N);
    fmpz_powm(a, a, b, modulus);

    return fmpz_get_ui(a);
}

struct less_than_key {
    inline bool operator() (const point& p1, const point& p2) {
        // sort by x-values
        return (p1.first != p2.first)
            ? (p1.first < p2.first)
            : (p1.second < p2.second);
    }
};
vector<point> gen_stations(long N) {
    vector<point> stations;
    set<point> seen_stations;
    for (long i = 0; i <= (2 * N); i++) {
        long x = exp(2, i, N);
        long y = exp(3, i, N);

        point station = point(x, y);
        if (seen_stations.count(station)) {
            // all points after will also be dupes
            break;
        }
        stations.push_back(station);
        seen_stations.insert(station);
    }
    sort(stations.begin(), stations.end(), less_than_key());
    return stations;
}

long snd(const point &p) {
    return p.second;
}

vector<long> get_longest_increasing_noncontiguous_subsequence(vector<long> elems) {
    // code adapted from Wikipedia
    long n = elems.size();
    vector<long> p(elems.size());
    vector<long> m(elems.size() + 1);
    long l = 0;
    for (int i = 0; i < n; i++) {
        // Binary search on lengths:
        //   bsearch for the largest positive j <= L such that elems[m[j]] < elems[i]
        // this finds us a guy we can extend
        long lo = 1;
        long hi = l;
        while (lo <= hi) {
            long mid = lo;
            if (lo != hi) {
                mid = ceil((lo + hi) / 2.0);
            }
            if (elems[m[mid]] <= elems[i]) {
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }
        // After searching, lo is 1 greater than the
        // length of the longest prefielems of elems[i]
        long newL = lo;
        // The predecessor of elems[i] is the last indeelems of
        // the noncontiguous subsequence of length newL-1
        p[i] = m[newL - 1];
        // why don't you need to check that elems[i] < elems[m[newL]]?
        // m stores the indices of the least elems-values but it's obvious that
        // the above inequality holds
        m[newL] = i;
        if (newL > l) {
            // If we found a noncontiguous subsequence longer than any we've
            // found yet, update L
            l = newL;
        }
    }

    // Reconstruct the longest increasing noncontiguous subsequence
    vector<long> s;
    size_t k = m[l];
    for (int i = l - 1; i >= 0; i--) {
        s.push_back(elems[k]);
        k = p[k];
    }
    reverse(s.begin(), s.end());
    return s;
}

long s(long N) {
    vector<point> stations = gen_stations(N);

    vector<long> y_values;
    std::transform(
        stations.begin(),
        stations.end(),
        std::back_inserter(y_values),
        snd
    );

    vector<long> path = get_longest_increasing_noncontiguous_subsequence(y_values);
    return path.size();
}

int main(int argc, char const *argv[]) {
    long sum = 0;
    for (long k = 1; k <= 30; k++) {
        long result = s(pow(k, 5));
        printf("S(%lu^5) == %lu\n", k, result);
        sum += result;
    }
    printf("%lu", sum);
    return 0;
}
