class Day7Test extends org.scalatest.funsuite.AnyFunSuite {

   val input = List(
     "$ cd /",
     "$ ls",
     "dir a",
     "14848514 b.txt",
     "8504156 c.dat",
     "dir d",
     "$ cd a",
     "$ ls",
     "dir e",
     "29116 f",
     "2557 g",
     "62596 h.lst",
     "$ cd e",
     "$ ls",
     "584 i",
     "$ cd ..",
     "$ cd ..",
     "$ cd d",
     "$ ls",
     "4060174 j",
     "8033020 d.log",
     "5626152 d.ext",
     "7214296 a",
   )

   test("t1") {
      val res = Day7.solve1Raw(input)
      assert(res == 95437)
   }

   test("t2") {
      val res = Day7.solve2Raw(input)
      assert(res == 24933642)
   }

}
