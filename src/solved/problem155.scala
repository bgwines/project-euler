
object Problem155 {
class Rational(numerator: Int, denominator: Int) {
    val nd_gcd = gcd(numerator.abs, denominator.abs)
    val n = numerator   / nd_gcd
    val d = denominator / nd_gcd

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

    def scale(scalar: Int): Rational = {
      new Rational(n * scalar, d)
    }

    def reciprocal(): Rational = {
      new Rational(d, n)
    }

    def gcd(a: Int, b: Int): Int = {
      if (b == 0) {
        a
      } else {
        gcd(b, a % b)
      }
    }

    def isInt(): Boolean = {
      d == 1
    }

    def toInt(): Int = {
      n
    }

    override def toString(): String = {
      n + "/" + d
    }

    override def equals(o: Any) = o match {
      case that: Rational => (that.n == n) && (that.d == d)
      case _ => false
    }

    override def hashCode = (n * d).hashCode
  }

  def fromPair(nd: (Int, Int)): Rational = {
    new Rational(nd._1, nd._2)
  }

  def rational(n: Int): Rational = {
    new Rational(n, 1)
  }

  def series(x: Rational, y: Rational): Rational = {
    rational(1).div(x.reciprocal().add(y.reciprocal()))
  }

  def parallel(x: Rational, y: Rational): Rational = {
    x.add(y)
  }

  var dpCache: scala.collection.mutable.Map[Int, scala.collection.mutable.Set[Rational]] = scala.collection.mutable.Map.empty
  def capacitanceValues(n: Int): scala.collection.mutable.Set[Rational] = {
    if (n == 1) {
      scala.collection.mutable.Set(rational(60))
    } else {
      if (!dpCache.contains(n)) {
        println(n)
        var capValues: scala.collection.mutable.Set[Rational] = scala.collection.mutable.Set.empty
        for (k <- 1 to (n / 2)) {
          for (c1 <- capacitanceValues(k)) {
            for (c2 <- capacitanceValues(n - k)) {
              capValues.+=(series(c1, c2))
              capValues.+=(parallel(c1, c2))
            }
          }
        }
        dpCache.+=((n, capValues))
      }
      dpCache.get(n).get
    }
  }

  def D(n: Int): Int = {
    var capValues: scala.collection.mutable.Set[Rational] = scala.collection.mutable.Set.empty
    for (k <- 1 to n) {
      capValues = capValues.union(capacitanceValues(k))
    }
    capValues.size
  }

  def main(args: Array[String]) {
    println(D(18))
  }
}