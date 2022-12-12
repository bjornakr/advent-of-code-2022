class Day4Test extends org.scalatest.funsuite.AnyFunSuite {
  val input = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8",
  )

  val i2 = List()

  test("std input") {
    val res1 = Day4.solve1Raw(input)
    assert(res1 == 2)

    val res2 = Day4.solve2Raw(input)
    assert(res2 == 4)
  }
  test("alt input: inclusive") {
    val input = List("2-2,2-2", "2-3,2-2", "2-2,2-3", "1-9,1-1", "1-1,1-9")
    val res   = Day4.solve1Raw(input)
    assert(res == input.length)
  }

  test("alt input 2: exclusive") {
    val input = List("2-3,1-1", "1-1,2-3")
    val res   = Day4.solve1Raw(input)
    assert(res == 0)
  }

}
