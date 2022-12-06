def readData(dayNo: Int) = scala.io.Source.fromFile(s"data/day$dayNo.txt").getLines().toList

trait Solver[A] {
  val decode: String => A
  def dayNo: Int
  def realData = readData(dayNo).map(decode)
  def solve1(input: List[A]): Int
  def solve2(input: List[A]): Int
}

trait Solver2 {
  type IN
  val decode: String => IN
  def dayNo: Int
  def realData: List[IN] = readData(dayNo).map(decode)
  def solve1(input: List[IN]): Int
  def solve2(input: List[IN]): Int
  def solve1Raw(input: List[String]): Int = solve1(input.map(decode))
  def solve2Raw(input: List[String]): Int = solve2(input.map(decode))

}

trait StringSolver extends Solver2 {
  override type IN = String
  override val decode   = identity
  override def realData = readData(dayNo)
}
