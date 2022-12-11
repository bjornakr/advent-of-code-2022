object Day6 extends Solver2 {
   type IN  = List[Char]
   type OUT = Int

   def dayNo: Int = 6

   def parse(s: String): IN = s.toCharArray().toList

   def procLine(s: IN, markerLength: Int, processed: Int = 0): Int = {
      if s.take(markerLength).toSet.size == markerLength then processed + markerLength
      else procLine(s.tail, markerLength, processed + 1)
   }

   def solve1(input: List[List[Char]] = realData): OUT =
      input.map(procLine(_, 4)).sum

   def solve2(input: List[List[Char]] = realData): OUT =
      input.map(procLine(_, 14)).sum

}
