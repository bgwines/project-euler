/* Consider the infinite polynomial series AF(x) = xF1 + x2F2 + x3F3 + ..., where Fk is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that is, Fk = Fk−1 + Fk−2, F1 = 1 and F2 = 1.

For this problem we shall be BigInterested in values of x for which AF(x) is a positive BigInteger.

Surprisingly AF(1/2)   =  (1/2).1 + (1/2)2.1 + (1/2)3.2 + (1/2)4.3 + (1/2)5.5 + ...
   =  1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
   =  2
The corresponding values of x for the first five natural numbers are shown below.

x AF(x)
√2−1  1
1/2 2
(√13−2)/3 3
(√89−5)/8 4
(√34−3)/5 5

We shall call AF(x) a golden nugget if x is rational, because they become increasingly rarer; for example, the 10th golden nugget is 74049690.

Find the 15th golden nugget. */

/* Generating function for Fibonacci sequence:

         x
    -----------
    1 - x - x^2

Evaluating this function for a particular x gives what the sequence converges to. For example, evaluating for x = 1/2 yields 2. */

object Problem137 {
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
      n + "/" + d
    }
  }

  def fromPair(nd: (Int, Int)): Rational = {
    new Rational(BigInt(nd._1), BigInt(nd._2))
  }

  def fareySequence(n: Int): List[(Int, Int)] = {
    var a = 0
    var b = 1
    var c = 1
    var d = n
    var sequence = (a, b) :: List.empty
    while (c <= n) {
      val k = ((n + b) / d)

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

  def limit(x: Rational): Rational = {
    x.div((new Rational(1, 1)).sub(x).sub(x.mul(x)))
  }

  def validRationals(n: Int): List[Rational] = {
    fareySequence(n)
      .map(fromPair(_))
      .filter(limit(_).isInt)
      .toList
  }

  def fibs(length: Int): List[Int] = {
    var cache: Map[Int, Int] = Map.empty
    def fib(n: Int): Int = {
      if (!cache.contains(n)) {
        if (n <= 0) {
          cache = cache.+((n, 1))
        } else {
          cache = cache.+((n, fib(n-1) + fib(n-2)))
        }
      }
      cache.get(n).get
    }

    (-1 to length).map(fib(_)).toList
  }

  def main(args: Array[String]) {
    val oddFibPairs
      = fibs(33)
      .zip(fibs(33).tail)
      .zip((0 to 33).toList)
      .filter(_._2 % 2 == 1)
      .map(_._1)

    println(limit(fromPair(oddFibPairs(15-1))).toInt)
  }
}
