import scala.collection.BitSet

object Day3 extends StringSolver {

  def dayNo: Int = 3

  private val prioritityMap: Map[Char, Int] = (('a' to 'z') ++ ('A' to 'Z')).zip(1 to 52).toMap

  private def createBitSet(sequence: String): BitSet = {
    val priorities = sequence.map(prioritityMap(_))
    BitSet.fromSpecific(priorities)
  }

  private def findCommonPriority(ss: List[String]): Int = ss match {
    case hd :: tl =>
      val common: BitSet = tl.foldLeft(createBitSet(hd))((acc, next) => acc.&(createBitSet(next)))
      prioritityMap.values.find(common.contains).get
    case _ => throw new IllegalArgumentException(ss.toString())
  }

  def solve1(input: List[String] = realData): Int = {
    def parseInput(s: String): List[String] = s.splitAt(s.length / 2).toList

    input
      .map(parseInput)
      .map(findCommonPriority)
      .sum

  }

  def solve2(input: List[IN] = realData): Int =
    input
      .grouped(3)
      .map(findCommonPriority)
      .sum
}
