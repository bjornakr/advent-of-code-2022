import scala.util.chaining.*

object Day4 extends Solver2 {
  type IN = (Range, Range)

  def parse(s: String): IN = {
    def toRange(t: String): Range = {
      val parts = t.split('-')
      parts(0).toInt to parts(1).toInt
    }
    val parts = s.split(',')
    (toRange(parts(0)), toRange(parts(1)))
  }

  def dayNo: Int = 4

  def solve1(input: List[IN] = realData): Int =
    input.count((r1, r2) => r1.diff(r2).isEmpty || r2.diff(r1).isEmpty)
  // input.count((r1, r2) => (r1.toSet -- r2.toSet).isEmpty || (r2.toSet -- r1.toSet).isEmpty)

  def solve2(input: List[IN] = realData): Int =
    input.count((r1, r2) => r1.intersect(r2).nonEmpty)

}
