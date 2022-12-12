object Day7 extends Solver2 {

   override type IN  = Action | Output
   override type OUT = Int

   override def dayNo: OUT = 7

   opaque type DirId = String

   case class Dir(id: DirId, subdirs: List[Dir], size: Int)

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
      case FileOutput(size: Int)
   }
   import Output.*

   def parseCd(cs: List[Char]): CdAction = cs match {
      case '/' :: Nil        => ChangeDirToRoot
      case '.' :: '.' :: Nil => ChangeDirUp
      case id                => ChangeDirDown(id.mkString)
   }

   def parseFileOutput(s: String): FileOutput =
      FileOutput(s.split(' ')(0).toInt)

   override def parse(s: String): IN = {
      print(s"$s  -- ")
      val res: IN = s.stripPrefix("$ ").toCharArray.toList match {
         case 'c' :: 'd' :: rest        => parseCd(rest.tail)
         case 'l' :: 's' :: Nil         => ListFiles
         case 'd' :: 'i' :: 'r' :: rest => DirOutput(rest.tail.mkString)
         case cs if cs.head.isDigit     => parseFileOutput(cs.mkString)
         case _                         => throw new Exception(s"bad input: $s")
      }
      println(res)
      res
   }

   override def solve1(input: List[IN] = realData): OUT = {

//      def processAction(action: Action) = {
//
//      }

      def proc(ins: List[IN], dir: Dir): Dir = {
         ins match {
            case Output.FileOutput(size) :: rest => proc(rest, dir.copy(size = dir.size + size))
            case Output.DirOutput(dirId) :: rest =>
               proc(rest, dir)
//               val newDir = Dir(dirId, subdirs = List.empty, 0)
//               proc(rest, dir.copy(subdirs = newDir :: dir.subdirs))
            case Action.ChangeDirDown(dirId) :: rest =>
               val newDir = Dir(dirId, subdirs = List.empty, 0)
               val down   = proc(rest, newDir)
               dir.copy(subdirs = down :: dir.subdirs)
            case Action.ChangeDirUp :: rest => dir

         }
      }

      ???
   }

   override def solve2(input: List[IN] = realData): OUT = ???
}
