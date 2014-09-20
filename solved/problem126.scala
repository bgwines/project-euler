/* The minimum number of cubes to cover every visible face on a cuboid measuring 3 x 2 x 1 is twenty-two.

If we then add a second layer to this solid it would require forty-six cubes to cover every visible face, the third layer would require seventy-eight cubes, and the fourth layer would require one-hundred and eighteen cubes to cover every visible face.

However, the first layer on a cuboid measuring 5 x 1 x 1 also requires twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1, 7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.

We shall define C(n) to represent the number of cuboids that contain n cubes in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.

It turns out that 154 is the least value of n for which C(n) = 10.

Find the least value of n for which C(n) = 1000. */

object Problem126 {
	def triangleNum(n: Int): Int = {
		n * (n-1) / 2
	}

	def numCubesToCover(a: Int, b:Int, c:Int, k: Int): Int = {
		2 * ((a * b) + (a * c) + (b * c)) + // protrude original sides
		4 * (k - 1) * (a + b + c) + // stairs in between sides
		8 * (triangleNum(k - 1))    // 1x1x1 blocks in corner areas
	}

	def lhs(abc: (Int, Int, Int), k: Int): Int = {
		val (a, b, c) = abc
		(a * b) + (b * c) + (a * c) + (2 * (k-1) * (a + b + c))
	}

	def bsearchUBForB(a: Int, k: Int, aUB:Int, rhs: Int): Int = {
		var lb: Int = a
		var ub: Int = aUB
		var curr = (lb + ub) / 2
		while (ub - lb > 5) {
			val x = lhs((a, curr, 1), k)
			if (x < rhs) lb = curr
			if (x >= rhs) ub = curr

			curr = (lb + ub) / 2
		}

		for (b <- lb to ub) {
			if (lhs((a, b, 1), k) > rhs)
				return b
		}
		ub
	}

	def fixC(a: Int, b: Int, k: Int, rhs: Int): Option[Int] = {
		val numerator = rhs - 2*(k-1)*(a + b) - a*b
		val denominator = a + b + 2*(k-1)
		if (numerator % denominator == 0) {
			val div = (numerator / denominator)
			if (div >= b) {
				Some(div)
			} else {
				None
			}
		} else {
			None
		}
	}

	/* # (a, b, c) s.t.
		• ab + bc + ac + 2(k-1)(a + b + c) = n/2 - 4triangleNum(k-1)
		• 0 < a <= b <= c
	 */
	def numSolutions(n: Int, k: Int): Int = {
		val rhs = (n / 2) - (4 * triangleNum(k - 1))
		val aUB = (rhs - 2 - 4*(k-1)) / (2*k - 1)
		val abcs =
			for (
				a <- (1 to aUB).toList;
				b <- (a to bsearchUBForB(a, k, aUB, rhs)).toList;
				c = fixC(a, b, k, rhs)
				) yield (a, b, c)

		abcs.filter(abc => abc._3 != None)
			.map(abc => (abc._1, abc._2, abc._3.get))
			.count(abc => lhs(abc, k) == rhs)
	}

	def C(n: Int):Int = {
		val ks = (1 to n)
			.toList
			.takeWhile(k => 8 * triangleNum(k - 1) < n)
		ks.map(numSolutions(n, _)).sum
	}

	def main(x: Array[String]) {
		println((1 to 20000).find(C(_) == 1000))
	}
}
