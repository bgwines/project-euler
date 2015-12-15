/* The radical of n, rad(n), is the product of distinct prime factors of n. For example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
a < b
a + b = c
rad(abc) < c
For example, (5, 27, 32) is an abc-hit, because:

GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
5 < 27
5 + 27 = 32
rad(4320) = 30 < 32
It turns out that abc-hits are quite rare and there are only thirty-one abc-hits for c < 1000, with ∑c = 12523.

Find ∑c for c < 120000. */

/*  Observation 1:
		GCD(a, b) = 1
		=> GCD(a, a+b) = 1
		   GCD(b, a+b) = 1 (symmetrically)

		(can't factor anything out of a+b that's in a, otherwise a  wouldn't be coprime to be)		

	Observation 2:
		rad(xy) = rad(x) * rad(y)

		(from definition of rad)		

		It follows that:
			rad(abc) < c
			=> rad(ab) < c / rad(c)

	Thing:
		let c = 225 (a = 1, b = 2^5 * 7^1 = 224)
		consider `c / rad(c)`: it is 15
		consider `rad(a(c-a)) = rad(a) * rad(c-a)`: it must be < 15

---------------------------------------------------
1 = 1			8 = 2^3				9 = 3^2
---------------------------------------------------
5 = 5^1			27 = 3^3			32 = 2^5
---------------------------------------------------
1 = 1			48 = 2^4 * 3^1		49 = 7^2
---------------------------------------------------
1 = 1			63 = 3^2 * 7^1		64 = 2^6
---------------------------------------------------
1 = 1			80 = 2^4 * 5^1		81 = 3^4
---------------------------------------------------
32 = 2^5		49 = 7^2			81 = 3^4
---------------------------------------------------
4 = 2^2			121 = 11^2			125 = 5^3
---------------------------------------------------
3 = 3^1			125 = 5^3			128 = 2^7
---------------------------------------------------
1 = 1			224 = 2^5 * 7^1		225 = 3^2 * 5^2
---------------------------------------------------
1 = 1			242 = 2^1 * 11^2	243 = 3^5
---------------------------------------------------
2 = 2^1			243 = 3^5			245 = 5^1 * 7^2
---------------------------------------------------
7 = 7^1			243 = 3^5			250 = 2^1 * 5^3
---------------------------------------------------
13 = 13^1		243 = 3^5			256 = 2^8
---------------------------------------------------
81 = 3^4		175 = 5^2 * 7^1		256 = 2^8
---------------------------------------------------
1 = 1			288 = 2^5 * 3^2		289 = 17^2
---------------------------------------------------
100 = 2^2 * 5^2 243 = 3^5			343 = 7^3
---------------------------------------------------

 */	

for any c,
	how many coprime-to-c a <= |_c/2_|
	are s.t. rad(ac(c-a)) < c?
*/

import scala.math

object Problem127 {
	val primes = List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997,1009,1013,1019,1021,1031,1033,1039,1049,1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,1129,1151,1153,1163,1171,1181,1187,1193,1201,1213,1217,1223)

	def factorRec(n: Int, primes: List[Int]): List[(Int, Int)] = {
		if (n == 1) {
			Nil
		} else {
			val remainingPrimes: List[Int] = primes.dropWhile(p => (n % p) != 0)
			val p: Int = remainingPrimes.head
			val a: Int
				= (1 to n)
				.takeWhile(a => n % scala.math.pow(p, a) == 0)
				.last
			val nUpdated: Int = ((n: Double) / scala.math.pow(p, a)).toInt
			(p, a) :: factorRec(nUpdated, remainingPrimes)
		}
	}

	def factor(n: Int): List[(Int, Int)] = {
		factorRec(n, primes)
	}

	def rad(a: Int, b: Int): Int = {
		val x = factor(a * b).map(_._1)
		val y = factor(a + b).map(_._1)
		val z = (x ++ y).distinct
		z.product
	}

	def gcd(a: Int, b: Int): Int = {
		if (b == 0) {
			a
		} else {
			gcd(b, a % b)
		}
	}

	def isABCHit(a: Int, b: Int): Boolean = {
		rad(a, b) < (a + b)
	}

	def countABCHitsBelow(ub: Int): Int = {
		fareySequence(ub)
			.filter(ab => ab._1 + ab._2 <= ub)
			.tail
			.count(ab => isABCHit(ab._1, ab._2))
	}

	def fareySequence(n: Int): List[(Int, Int)] = {
		var a = 0
		var b = 1
		var c = 1
		var d = n
		var sequence = List((a, b))
		while (c <= n) {
			val k = ((n + b) / d).toInt

			val aOld = a
			val bOld = b
			a = c
			b = d
			c = ((k * c) - aOld)
			d = ((k * d) - bOld)

			sequence = (a, b) :: sequence
		}
		sequence.reverse
	}

	def coprimeMapUpTo(n: Int): Map[Int,List[Int]] = {
		fareySequence(n)
			.sortWith((a, b) => a._2 < b._2)
			.groupBy(_._2)
			.mapValues(x => x.map(_._1))
	}

	def abcHitsBelow(ub: Int): List[(Int, Int, Int)] = {
		val coprimeMap = coprimeMapUpTo(ub)
		(2 to ub)
			.toList
			.flatMap(c => abcHitsForC(c, coprimeMap.get(c).get))
	}

	def abcHitsForC(c: Int, coprimes: List[Int]): List[(Int, Int, Int)] = {
		coprimes
			.filter(a => (a <= c - a) && isABCHit(a, c - a))
			.map(a => (a, c - a, c))
	}

	def main(args: Array[String]): Unit = {
		countABCHitsBelow(1000)
	}
}