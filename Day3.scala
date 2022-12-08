import scala.collection.BitSet

object Day3 extends StringSolver {

  def dayNo: Int = 3

  private val prioritityMap: Map[Char, Int] = (('a' to 'z') ++ ('A' to 'Z')).zip(1 to 52).toMap

  private def findCommon(init: (String, String)): Char = {
    def loop(in: (List[Char], List[Char])): Char = in match {
      case (a :: as, b :: bs) if a == b => a
      case (as, b :: bs)                => loop(as, bs)
      case (_ :: as, Nil)               => loop(as, init._2.toList)
      case (Nil, _)                     => throw Exception(s"no common letter found in $init")
    }
    loop(init._1.toList, init._2.toList)
  }

  // private def findCommon2(init: (String, String)): Option[Char] = {
  //   def loop(in: (List[Char], List[Char])): Char = in match {
  //     case (a :: as, b :: bs) if a == b => Some(a)
  //     case (as, b :: bs)                => loop(as, bs)
  //     case (_ :: as, Nil)               => loop(as, init._2.toList)
  //     case (Nil, _)                     => None
  //   }
  //   loop(init._1.toList, init._2.toList)
  // }
  private def createBitSet(sequence: String): BitSet = {
    val priorities = sequence.map(prioritityMap(_))
    BitSet.fromSpecific(priorities)
  }

  private def findCommon3(ss: List[String]): Int = ss match {
    case hd :: tl =>
      val common: BitSet = tl.foldLeft(createBitSet(hd))((acc, next) => acc.&(createBitSet(next)))
      prioritityMap.values.find(common.contains).get
    case _ => throw new IllegalArgumentException(ss.toString())
  }

  def solve1(input: List[IN] = realData): Int = {
    val decode: String => (String, String) = s => s.splitAt(s.length / 2)

    input.map(decode).map(findCommon.andThen(prioritityMap(_))).sum
  }

  def solve1a(input: List[String] = realData): Int = {

    val parseInput: String => List[String] = s =>
      val split = s.splitAt(s.length / 2).toList
      split.toList
    //List(split._1, split._2)

    // def findScore(b: BitSet): Int  = prioritityMap.values.map(idx => if (b.get(idx)) idx else 0).sum
    // def findScore2(b: BitSet): Int = prioritityMap.values.find(idx => b.get(idx)).sum

    input
      .map(parseInput)
      .map(findCommon3)
      .sum

  }

  def solve2(input: List[IN] = realData): Int =
    // def findCommonG(init: List[String]): Char = {
    //   def loop(in: List[String], found: Option[Char]): Char = (in, found) match {
    //     case (a :: b :: as, None) =>
    //       findCommon(a, b) match {
    //         case c: Some => loop(as, c)
    //         case None => ??? // goose chase
    //       }
    //     case
    //     case Nil => ???
    //   }
    // }
    input
      .grouped(3)
      .map(findCommon3)
      .sum
}
