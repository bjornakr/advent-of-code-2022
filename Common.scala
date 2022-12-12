//> using scala "3.2.1"

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
   type OUT = Int
   def parse(s: String): IN
   def dayNo: Int
   def realData: List[IN] = readData(dayNo).map(parse)
   def solve1(input: List[IN]): OUT
   def solve2(input: List[IN]): OUT
   def solve1Raw(input: List[String]): OUT = solve1(input.map(parse))
   def solve2Raw(input: List[String]): OUT = solve2(input.map(parse))

}

trait StringSolver extends Solver2 {
   override type IN = String
   override def parse(s: String): String = s
   override def realData                 = readData(dayNo)
}
