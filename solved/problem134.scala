/* Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified that 1219 is the smallest number such that the last digits are formed by p1 whilst also being divisible by p2.

In fact, with the exception of p1 = 3 and p2 = 5, for every pair of consecutive primes, p2 > p1, there exist values of n for which the last digits are formed by p1 and n is divisible by p2. Let S be the smallest of these values of n.

Find ∑ S for every pair of consecutive primes with 5 ≤ p1 ≤ 1000000. */

object Problem134 {
  val sieve = {
    val SIEVE_LIMIT = 1000004
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i * i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).toSet
  }

  def solves(p1: BigInt, p2: BigInt, suffix: BigInt): Boolean = {
    val numDigitsInP1: Int = math.ceil(math.log(p1.doubleValue) / math.log(10)).intValue
    val base: BigInt = 10
    val modulus: BigInt = base.pow(numDigitsInP1)
    ((p2 * suffix) % modulus) == p1
  }

  def findMultipleWithSuffix(p1: BigInt, p2: BigInt): BigInt = {
    var suffix: BigInt = 0
    var numDigits: Int = 1
    while (!solves(p1, p2, suffix)) {
      val x1 = math.pow(10, numDigits).intValue
      val x2 = math.pow(10, numDigits - 1).intValue
      val digit = (0 to 9).find(d =>
        (p1 % x1) == ((d * x2 + suffix) * p2) % x1
      ).get

      suffix = digit * x2 + suffix
      numDigits += 1
    }
    suffix * p2
  }

  def main(args: Array[String]): Unit = {
    val primes = sieve.toList.sorted.drop(2)
    println(
      primes.zip(primes.tail)
      .map(pp => findMultipleWithSuffix(pp._1, pp._2))
      .sum)
  }
}