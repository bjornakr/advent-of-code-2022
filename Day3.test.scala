//> using lib "org.scalatest::scalatest::3.2.9"

class Day3Test extends org.scalatest.funsuite.AnyFunSuite {

  val input = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw",
  )

  test("hi") {
    assert(Day3.solve1Raw(input) == 157)
  }
}
