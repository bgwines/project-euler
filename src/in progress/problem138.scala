/* Consider the isosceles triangle with base length, b = 16, and legs, L = 17.


By using the Pythagorean theorem it can be seen that the height of the triangle, h = √(172 − 82) = 15, which is one less than the base length.

With b = 272 and L = 305, we get h = 273, which is one more than the base length, and this is the second smallest isosceles triangle with the property that h = b ± 1.

Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1 and b, L are positive integers. */

/* Equations:
    b^2 + (2b+-1)^2 = l^2

    1 + 4b + 5b^2 == l^2

    Some kind of quadratic Diophantine equation...

    FindInstance[1 + 4 b + 5 b^2 == l^2 && 0 < b && 0 < l, {b, l}, Integers, 3]
    {{b -> 538535038241952362226934695959853295780111695919015884561671053\
2445858762250957339719874081155912009298010552361437510941190135300798\
7816234240714353528115172106407371189464750086677705153399816833759554\
2345777197485112445775533267391989055055371190529653915240193433118265\
8152665444017209288572856314701917838045132901906612811311908932621305\
2961855299058898676935979760562184526235014838100022703813587406176948\
4883861180288667114104501926313113983374886218943658897432020379799560\
6650795026921688888961849338409261284691805804111406809197849678459908\
5575371976105849221607670416620366842877730696694165079433037926120191\
8816057775334622605876219358318743231345726405472, l ->
    ...
 */

object Problem138 {
	def isPerfectSquare(x: Int): Boolean = {
		math.pow(math.round(math.sqrt(x)), 2) == x
	}

  def hasIntegerLength(bh: (Int, Int)): Boolean = {
    if (bh._1 % 100000 == 0) {
      println(bh._1)
    }
    isPerfectSquare(lengthSquared(bh))
  }

	def baseAndHeights(b: Int): List[(Int, Int)] = {
		(b, b + 1) :: (b, b - 1) :: Nil
	}

  def lengthSquared(bh: (Int, Int)): Int = {
    math.pow(bh._1 / 2, 2).intValue + math.pow(bh._2, 2).intValue
  }

  def length(bh: (Int, Int)): Int = {
    math.sqrt(lengthSquared(bh)).intValue
  }

	def main(args: Array[String]): Unit = {
	  println(
	  	Stream.from(1)
	  		.filter(_ % 2 == 0)
	  		.flatMap(baseAndHeights(_))
	  		.filter(hasIntegerLength(_))
        .map(bh => (bh._1, bh._2, length(bh)))
		  	.take(3)
		  	.toList
		 )
	}
}