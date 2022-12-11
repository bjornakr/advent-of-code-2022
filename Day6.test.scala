//> using lib "org.scalatest::scalatest::3.2.9"

class Day6Test extends org.scalatest.funsuite.AnyFunSuite {
   test("bvwbjplbgvbhsrlpgdmjqwftvncz") {
      val res = Day6.solve1Raw(List("bvwbjplbgvbhsrlpgdmjqwftvncz"))
      assert(res == 5)
   }

}
