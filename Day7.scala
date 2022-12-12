object Day7 extends Solver2 {

   opaque type DirId = String

   val rootId: DirId = "/"

   case class Dir(id: DirId, subdirs: List[Dir], size: Int)

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

   type IN = Action | Output

   def parseCd(cs: List[Char]): CdAction = cs match {
      case '/' :: Nil => ChangeDirToRoot
      case '.' :: '.' :: Nil => ChangeDirUp
      case id => ChangeDirDown(id)
   }

   def parse(s: String): IN => s.toCharArray.toList match {
      case 'c' :: 'd' :: ' ' :: rest => parseCd(rest)
      case _ => ???
   }
}
