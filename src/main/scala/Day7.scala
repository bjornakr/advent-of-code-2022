object Day7 extends Solver2 {

   override type IN  = Action | Output
   override type OUT = Int

   override def dayNo: OUT = 7

   opaque type DirId = String

   case class Dir(id: DirId, subdirs: List[Dir], size: Int) {
      def filter(f: Dir => Boolean): Option[Dir] = f(this) match {
         case false => None
         case true =>
            val subdirs1 = subdirs.map(_.filter(f)).flatten
            val r        = Some(this.copy(subdirs = subdirs1))
            println(r)
            r
      }

      def deepsize: Int = size + subdirs.map(_.deepsize).sum

   }

   val rootDir: Dir = Dir("/", List.empty, 0)

   enum Action {
      case ChangeDirDown(id: DirId)
      case ChangeDirUp
      case ChangeDirToRoot
      case ListFiles
   }

   import Action.*

   type CdAction = ChangeDirDown | ChangeDirUp.type | ChangeDirToRoot.type

   enum Output {
      case DirOutput(dirId: DirId)
      case FileOutput(fileName: String, size: Int)
   }
   import Output.*

   def parseCd(cs: List[Char]): CdAction = cs match {
      case '/' :: Nil        => ChangeDirToRoot
      case '.' :: '.' :: Nil => ChangeDirUp
      case id                => ChangeDirDown(id.mkString)
   }

   def parseFileOutput(s: String): FileOutput = {
      val parts = s.split(' ')
      FileOutput(fileName = parts(1), size = parts(0).toInt)
   }

   override def parse(s: String): IN = {
      // print(s"$s  -- ")
      val res: IN = s.stripPrefix("$ ").toCharArray.toList match {
         case 'c' :: 'd' :: rest        => parseCd(rest.tail)
         case 'l' :: 's' :: Nil         => ListFiles
         case 'd' :: 'i' :: 'r' :: rest => DirOutput(rest.tail.mkString)
         case cs if cs.head.isDigit     => parseFileOutput(cs.mkString)
         case _                         => throw new Exception(s"bad input: $s")
      }
      // println(res)
      res
   }

   def proc(ins: List[IN], dir: Dir): (Dir, List[IN]) = {
      ins match {
         case Nil                                       => (dir, Nil)
         case Output.FileOutput(fileName, size) :: rest => proc(rest, dir.copy(size = dir.size + size))
         case Output.DirOutput(dirId) :: rest           => proc(rest, dir)
         case (a @ Action.ChangeDirDown(dirId)) :: rest0 =>
            val newDir        = Dir(dirId, subdirs = List.empty, 0)
            val (down, rest1) = proc(rest0, newDir)
            proc(rest1, dir.copy(subdirs = down :: dir.subdirs))
         case Action.ChangeDirUp :: rest     => (dir, rest)
         case Action.ChangeDirToRoot :: rest => proc(rest, dir)
         case Action.ListFiles :: rest       => proc(rest, dir)
      }
   }

   def find(dir: Dir, p: Dir => Boolean): List[Dir] = p(dir) match {
      case false => dir.subdirs.flatMap(find(_, p))
      case true  => dir :: dir.subdirs.flatMap(find(_, p))
   }

   override def solve1(input: List[IN] = realData): OUT = {

      val (dir, _) = proc(input, rootDir)
      val df       = find(dir, _.deepsize <= 100_000)
      val res      = df.map(_.deepsize).sum
      println(df)
      res
   }

   override def solve2(input: List[IN] = realData): OUT =
      val (dir, _)        = proc(input, rootDir)
      val totalSpace      = 70_000_000
      val usedSpace       = dir.deepsize
      val freeSpace       = totalSpace - usedSpace
      val neededForUpdate = 30_000_000 - freeSpace
      val df              = find(dir, _.deepsize >= neededForUpdate)
      val res             = df.map(_.deepsize).sorted
      println(res)
      res.min

}
