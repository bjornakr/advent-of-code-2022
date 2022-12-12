//> using lib "org.typelevel::cats-core:2.9.0"

import cats.implicits.*

object Day5 {

   opaque type Position = Int
   opaque type Crates   = Map[Position, List[Char]]

   object Crates {
      def moveGroup(crates: Crates, moveOrder: MoveOrder): Crates = {
         val (lifted, remaining) = crates(moveOrder.from).splitAt(moveOrder.amount)
         crates
            .updatedWith(moveOrder.from)(cs => Some(remaining))
            .updatedWith(moveOrder.to)(cs => Some(cs.map(lifted ++ _)).get)
      }

      def moveOneByOne(crates: Crates, moveOrder: MoveOrder): Crates = moveOrder match {
         case MoveOrder(0, from, to) => crates
         case MoveOrder(amount, from, to) =>
            val moved = moveGroup(crates, MoveOrder(1, from, to))
            moveOneByOne(moved, MoveOrder(amount - 1, from, to))
      }
   }
   import Crates.*

   case class MoveOrder(amount: Int, from: Position, to: Position)

   type IN = (Crates, List[MoveOrder])

   type OUT = String

   def parseSingleCrate(s: String): Option[Char] =
      s.toCharArray.toList match {
         case '[' :: c :: _ => Some(c)
         case ' ' :: _      => None
         case _             => throw new Exception(s"Bad crate: $s")
      }

   def parseCrateLine(
       s: String,
       pos: Position = 1,
       acc: Map[Position, Char] = Map.empty,
   ): Map[Position, Char] =
      s match {
         case "" => acc
         case _ =>
            val next = s.take(3)
            val x    = parseSingleCrate(next)
            parseCrateLine(s.drop(4), pos + 1, x.fold(acc)(acc.updated(pos, _)))
      }

   def parseCrates(lines: List[String]) = {
      val crates = lines.map(parseCrateLine(_))
      crates.foldLeft(Map.empty[Position, List[Char]]) { (acc, next) =>
         acc.combine(next.view.mapValues(List(_)).toMap)
      }
   }

   def parseMoveOrder(s: String) = {
      val parts = s.split(" ")
      MoveOrder(parts(1).toInt, parts(3).toInt, parts(5).toInt)
   }

   def parse(lines: List[String]): IN = {
      val crateLines     = lines.takeWhile(s => s.charAt(1) != '1')
      val crates         = parseCrates(crateLines)
      val moveOrderLines = lines.drop(crateLines.length + 2)
      val moveOrders     = moveOrderLines.map(parseMoveOrder)
      (crates, moveOrders)
   }

   def skimTop(crates: Map[Position, List[Char]]): String = {
      crates.toList.sorted.map(_._2.headOption).toList.flatten.mkString
   }

   def solve1(input: List[String] = readData(dayNo = 5)): OUT = {
      val (crates, moveOrders) = parse(input)
      val res                  = moveOrders.foldLeft(crates)((acc, next) => moveOneByOne(acc, next))
      skimTop(res)
   }

   def solve2(input: List[String] = readData(dayNo = 5)): OUT = {
      val (crates, moveOrders) = parse(input)
      val res                  = moveOrders.foldLeft(crates)((acc, next) => moveGroup(acc, next))
      skimTop(res)
   }

}
