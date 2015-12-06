/* Consider the infinite polynomial series AG(x) = xG1 + x2G2 + x3G3 + ..., where Gk is the kth term of the second order recurrence relation Gk = Gk−1 + Gk−2, G1 = 1 and G2 = 4; that is, 1, 4, 5, 9, 14, 23, ... .

For this problem we shall be concerned with values of x for which AG(x) is a positive integer.

The corresponding values of x for the first five natural numbers are shown below.

x   AG(x)
(√5−1)/4    1
2/5 2
(√22−2)/6   3
(√137−5)/14 4
1/2 5

We shall call AG(x) a golden nugget if x is rational, because they become increasingly rarer; for example, the 20th golden nugget is 211345365.

Find the sum of the first thirty golden nuggets. */

/* define
    F(x) = f0 + f1x + f2x^2 + f3x^3 + f4x^4 + ...

derive generating function for
    <1, 4, f1 + f0, f2 + f1, f3 + f2, ...>

<0, 1,  0,       0,       0,       ...>  +     <->   x
<0, 0,  3,       0,       0,       ...>  +     <->   3x^2
<0, 0,  f0,      f1,      f2,      ...>  +     <->   x^2F(x)
<0, f0, f1,      f2,      f3,      ...>        <->   xF(x)
---------------------------------------------------------------
<0, 1,  4,       f2 + f1, f2 + f1, f3 + f2, ...>         <->
                                       x + 3x^2 + x^2F(x) + xF(x)

    F(x) = x + 3x^2 + x^2F(x) + xF(x)
    F(x)(1 - x^2 - x) = x + 3x^2
    F(x) = (x + 3x^2) / (1 - x^2 - x)

Evaluating this function for a particular x gives what the sequence converges to. For example, evaluating for x = 2/5 yields 2.

Notice:
    scala> Problem140.validRationals(1000)
    res28: List[Rational] = List(0/1, 2/5, 1/2, 7/12, 3/5, 19/31, 8/13, 50/81, 21/34, 131/212, 55/89, 343/555, 144/233, 377/610)

    sequence 1: Fibonacci
    sequence 2: f(n+1) = (f(n).n + f(n).d) / (f(n+1).n + f(n).d)
  */

object Problem140 {
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

  // (x + 3x^2) / (1 - x^2 - x)
  def limit(x: Rational): Rational = {
    val one = new Rational(1, 1)
    val numerator = x.add(x.mul(x).scale(3))
    val denominator = one.sub(x.mul(x)).sub(x)
    numerator.div(denominator)
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

  def secondSequence(length: Int): List[Rational] = {
    def f(n: Int): Rational = {
      if (n == 0) {
        new Rational(2, 5)
      } else {
        val prev = f(n-1)
        val numerator = prev.n + prev.d
        new Rational(numerator, numerator + prev.d)
      }
    }

    (0 to length - 1).map(f(_)).toList
  }

  def main(args: Array[String]) {
    val nFibs = 15 * 2 - 1
    val oddFibPairs
      = fibs(nFibs)
      .zip(fibs(nFibs).tail)
      .zip((0 to nFibs).toList)
      .filter(_._2 % 2 == 1)
      .map(fromPair(_._1))

    val secondSequencePairs = secondSequence(15)

    val total = oddFibPairs.map(x => limit(x).toInt).sum +
        secondSequencePairs.map(x => limit(x).toInt).sum

    println(total)
  }
}
