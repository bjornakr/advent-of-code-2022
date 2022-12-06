object Day3 extends StringSolver {

  def dayNo: Int = 3
  def solve1(input: List[IN] = realData): Int = {
    val decode: String => (String, String) = s => s.splitAt(s.length / 2)

    val prioritityMap: Map[Char, Int] = (('a' to 'z') ++ ('A' to 'Z')).zip(1 to 52).toMap

    def findCommon(init: (String, String)): Char = {
      def loop(in: (List[Char], List[Char])): Char = in match {
        case (a :: as, b :: bs) if a == b => a
        case (as, b :: bs)                => loop(as, bs)
        case (_ :: as, Nil)               => loop(as, init._2.toList)
        case (Nil, _)                     => throw Exception(s"no common letter found in $init")
      }
      loop(init._1.toList, init._2.toList)
    }

    input.map(decode).map(findCommon.andThen(prioritityMap(_))).sum
  }
  def solve2(input: List[IN] = realData): Int = {
    val groups = input.grouped(3)
    ???
  }
}
