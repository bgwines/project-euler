/* A henagonal tile with number 1 is surrounded by a ring of sin henagonal tiles, starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise direction.

New rings are added in the same fashion, with the nent rings being numbered 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below shows the first three rings.


By finding the difference between tile n and each of its sin neighbours we shall define PD(n) to be the number of those differences which are prime.

For enample, working clockwise around tile 8 the differences are 12, 29, 11, 6, 1, and 13. So PD(8) = 3.

In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10, hence PD(17) = 2.

It can be shown that the manimum value of PD(n) is 3.

If all of the tiles for which PD(n) = 3 are listed in ascending order to form a sequence, the 10th tile would be 271.

Find the 2000th tile in this sequence. */

object Problem128 {
  def prime(n: BigInt): Boolean = {
    if (n < 2) false
    if (n == 2) true
    if (n % 2 != 0) false
    
    (3 to math.ceil(math.sqrt(n.toDouble)).toInt)
      .toList
      .count(x => n % x == 0) == 0
  }

  def end(ringIndex: BigInt): BigInt = {
    start(ringIndex) + (6 * ringIndex) - 1
  }

  def start(ringIndex: BigInt): BigInt = {
    2 + (6 * ringIndex * (ringIndex - 1) / 2)
  }

  def penultimate(ringIndex: BigInt): BigInt = {
    end(ringIndex) - 1
  }

  def allDiffsPrime(list: List[BigInt], toDiff: BigInt): Boolean = {
    list
      .map(x => (x - toDiff).abs)
      .filter(prime(_))
      .size == list.size   
  }

  def isEndPD3(ringIndex: BigInt): Boolean = {
    allDiffsPrime(
      List(start(ringIndex)
         , start(ringIndex - 1)
         , penultimate(ringIndex + 1))
      , end(ringIndex))
  }

  def isStartPD3(ringIndex: BigInt): Boolean = {
    allDiffsPrime(
      List(end(ringIndex)
         , end(ringIndex + 1)
         , start(ringIndex + 1) + 1)
      , start(ringIndex))
  }

  def identifyPD3s2(nPD3s: BigInt): List[BigInt] = {
    var ringIndex: BigInt = 2
    var pd3s: List[BigInt] = Nil
    while (pd3s.size < nPD3s) {
      if (isStartPD3(ringIndex)) {
        pd3s = start(ringIndex) :: pd3s
        println(pd3s.size + 1, start(ringIndex))
      }

      if (isEndPD3(ringIndex)) {
        pd3s = end(ringIndex) :: pd3s
        println(pd3s.size + 1, end(ringIndex))
      }
      ringIndex += 1
    }
    pd3s.reverse
  }

  def main(args: Array[String]): Unit = {
    identifyPD3s2(2000)
  }
}