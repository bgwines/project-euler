/* Taking three different letters from the 26 letters of the alphabet, character strings of length three can be formed.
Examples are 'abc', 'hat' and 'zyx'.
When we study these three examples we see that for 'abc' two characters come lexicographically after its neighbour to the left.
For 'hat' there is exactly one character that comes lexicographically after its neighbour to the left. For 'zyx' there are zero characters that come lexicographically after its neighbour to the left.
In all there are 10400 strings of length 3 for which exactly one character comes lexicographically after its neighbour to the left.

We now consider strings of n ≤ 26 different characters from the alphabet.
For every n, p(n) is the number of strings of length n for which exactly one character comes lexicographically after its neighbour to the left.

What is the maximum value of p(n)? */
class Rational(numerator: BigInt, denominator: BigInt) {
  val n = numerator   / (gcd(numerator.abs, denominator.abs))
  val d = denominator / (gcd(numerator.abs, denominator.abs))

  def add(other: Rational): Rational = {
    new Rational(
      (n * other.d) + (other.n * d),
      d * other.d
    )
  }

  def sub(other: Rational): Rational = {
    new Rational(
      (n * other.d) - (other.n * d),
      d * other.d
    )   
  }

  def mul(other: Rational): Rational = {
    new Rational(
      n * other.n,
      d * other.d
    )
  }

  def div(other: Rational): Rational = {
    mul(new Rational(other.d, other.n))
  }

  def scale(scalar: BigInt): Rational = {
    new Rational(n * scalar, d)
  }

  def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }
  }

  def isInt(): Boolean = {
    d == 1
  }

  def toInt(): BigInt = {
    n
  }

  override def toString(): String = {
    if (d != 1) {
      n + "/" + d
    } else {
      n + ""
    }
  }
}

def fromPair(nd: (Int, Int)): Rational = {
  new Rational(BigInt(nd._1), BigInt(nd._2))
}

def rational(n: BigInt): Rational = {
  new Rational(n, BigInt(1))
}

object Problem158 {
  type MutableRatArray = scala.collection.mutable.ArrayBuffer[Rational]
  type MutableArrayArray = scala.collection.mutable.ArrayBuffer[MutableRatArray]

  def initArray(value: BigInt): MutableRatArRat = {
    val array: MutableIntArray = new scala.collection.mutable.ArrayBuffer(26)
    for (i <- 0 to 25) {
      array += rational(value)
    }
    array
  }

  def initArrayArray(value: BigInt): MutableArrayArray = {
    val array: MutableArrayArray = new scala.collection.mutable.ArrayBuffer(26)
    for (i <- 0 to 25) {
      array += initArray(value)
    }
    array
  }

  def countsForStringsOfLengthDEV(len: Int): (MutableArrayArray, MutableRatArray) = {
    def sortedToSemisortedMultiplier(len: Int, i: Int, j: Int): Rational = {
      if (len <= 2) {
        rational(BigInt(1))
      } else {
        // (j < i) and (i onwards is strictly decreasing), so range
        // of possible collisions is [(i-1)..0]
        val collidableRangeSize = i - 1 + 1 // + 1 for the 0 at the end
        val numCollidable = len - 2
        // 1 - P(collision) is 1 - (numCollidable / collidableRangeSize)
        new Rational(collidableRangeSize - numCollidable, collidableRangeSize)
      }
    }

    if (len == 1) {
      (initArray(0), initArray(1))
    } else {
      val (oldSemisorted, oldSorted) = countsForStringsOfLength(len - 1)
      val (semisorted, sorted) = (initArrayArray(0), initArray(0))

      // sorted -> semisorted
      for (i <- 0 to 25) {
        for (j <- 0 to (i - 1)) { // pre-pending j, so j < i to make i lexicographically after.
          // multiply by proportion that don't have have j as a repeated char -- problem is that currently this seems to be done right but doesn't give the right answer
          semisorted(j)(j) += oldSorted(i).mul(sortedToSemisortedMultiplier(len, i, j))
        }
      }

      println()
      // semisorted -> semisorted
      for (i <- 0 to 25) {
        for (j <- (i + 1) to 25) {
          val y = oldSemisorted(i).mul(semisortedToSemisortedMultiplier(len, i, j))
          val x = semisorted(j).add(y)

          if (j == jj) {
            println("semisorted(" + j + ") +=    oldSemisorted(" + i + "){" + oldSemisorted(i) + "} * " + semisortedToSemisortedMultiplier(len, i, j) + " {" + y + "}    -> " + x)
          }

          semisorted(j) = semisorted(j).add(oldSemisorted(i).mul(semisortedToSemisortedMultiplier(len, i, j)))
        }
      }
      println()

      // sorted -> sorted
      for (i <- 0 to 25) {
        for (j <- (i + 1) to 25) {
          sorted(j) = sorted(j).add(oldSorted(i))
        }
      }

      (semisorted, sorted)
    }
  }

  // "sorted" = increasing
  // "semisorted" = increasing except for 1 elem
  // ( starting elems of semisorted strings of that length
  // , starting elems of sorted strings of that length
  def countsForStringsOfLength(len: Int): (MutableRatArray, MutableRatArray) = {
    def sortedToSemisortedMultiplier(len: Int, i: Int, j: Int): Rational = {
      if (len <= 2) {
        rational(BigInt(1))
      } else {
        // (j < i) and (i onwards is strictly decreasing), so range
        // of possible collisions is [(i-1)..0]
        val collidableRangeSize = i - 1 + 1 // + 1 for the 0 at the end
        val numCollidable = len - 2
        // 1 - P(collision) is 1 - (numCollidable / collidableRangeSize)
        new Rational(collidableRangeSize - numCollidable, collidableRangeSize)
      }
    }

    def semisortedToSemisortedMultiplier(len: Int, i: Int, j: Int): Rational = {
      if (len <= 2) {
        rational(BigInt(1))
      } else {
        // (j > i) and (i onwards is semisorted), so only 1 number 
        // after i appears
        val rangeOfCollidingValue = 25 - i
        new Rational(rangeOfCollidingValue - 1, rangeOfCollidingValue)
      }
    }

    if (len == 1) {
      (initArray(0), initArray(1))
    } else {
      val (oldSemisorted, oldSorted) = countsForStringsOfLength(len - 1)
      val (semisorted, sorted) = (initArray(0), initArray(0))
      
      // sorted -> semisorted
      for (i <- 0 to 25) {
        for (j <- 0 to (i - 1)) { // pre-pending j, so j < i to make i lexicographically after.
          // multiply by proportion that don't have have j as a repeated char -- problem is that currently this seems to be done right but doesn't give the right answer
          semisorted(j) = semisorted(j).add(oldSorted(i).mul(sortedToSemisortedMultiplier(len, i, j)))
        }
      }

      // semisorted -> semisorted
      for (i <- 0 to 25) {
        for (j <- (i + 1) to 25) {
          semisorted(j) = semisorted(j).add(oldSemisorted(i).mul(semisortedToSemisortedMultiplier(len, i, j)))
        }
      }

      // sorted -> sorted
      for (i <- 0 to 25) {
        for (j <- (i + 1) to 25) {
          sorted(j) = sorted(j).add(oldSorted(i))
        }
      }

      (semisorted, sorted)
    }
  }

  def numsemisortedStringsOfLength(len: Int): BigInt = {
    countsForStringsOfLength(len)._1.map(_.toInt).sum
  }

  def main(args: Array[String]) {
  }
}