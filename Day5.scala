//> using lib "org.typelevel::cats-core:2.9.0"

import scala.collection.mutable
import cats.implicits.*
import scala.collection.mutable.ListBuffer
import scala.collection.MapView

object Day5 {

   opaque type Position = Int

   opaque type Crates = mutable.Map[Position, mutable.Stack[Char]]

   val initSetup = Map.empty[Position, List[Char]]

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
       acc: mutable.Map[Position, Char] = mutable.Map.empty,
   ): mutable.Map[Position, Char] =
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
         acc.combine(next.mapValues(List(_)).toMap)
      }
   }

   def parseMoveOrder(s: String) = {
      val parts = s.split(" ")
      MoveOrder(parts(1).toInt, parts(3).toInt, parts(5).toInt)
   }

   def parse(lines: List[String]): IN = {
      val crateLines      = lines.takeWhile(s => s.charAt(1) != '1')
      val cratesImmutable = parseCrates(crateLines)
      val cratesMutable   = mutable.Map() ++ cratesImmutable.mapValues(mutable.Stack.empty ++ _)
      val moveOrderLines  = lines.drop(crateLines.length + 2)
      val moveOrders      = moveOrderLines.map(parseMoveOrder)
      (cratesMutable, moveOrders)
   }

   def peekTop(crates: mutable.Map[Position, mutable.Stack[Char]]): String = {
      import scala.util.Try
      val a: List[Char] = crates.values.map(v => Try(v.top).toOption).toList.flatten
      a.mkString
   }

   def solve1(input: List[String] = readData(dayNo = 5)): OUT = {
      def executeMoveOrder(
          moveOrder: MoveOrder,
          crates: mutable.Map[Position, mutable.Stack[Char]],
      ): mutable.Map[Position, mutable.Stack[Char]] = moveOrder match {
         case MoveOrder(0, _, _) => crates
         case MoveOrder(amount, from, to) =>
            val crate = crates(from).pop
            crates(moveOrder.to).push(crate)
            executeMoveOrder(MoveOrder(amount - 1, from, to), crates)
      }

      val (crates, moveOrders) = parse(input)
      val res                  = moveOrders.foldLeft(crates)((acc, next) => executeMoveOrder(next, acc))
      peekTop(res)
   }

   def solve2(input: List[String] = realData(dayNo = 5)): OUT = {

      def executeMoveOrder(
          moveOrder: MoveOrder,
          crates: mutable.Map[Position, mutable.ListBuffer[Char]],
      ): mutable.Map[Position, mutable.ListBuffer[Char]] = moveOrder match {
         case MoveOrder(0, _, _) => crates
         case MoveOrder(amount, from, to) =>
            val (lifted, remaining)         = crates(from).splitAt(amount)
            val a: mutable.ListBuffer[Char] = crates(to)
            a.addAll(lifted)
            crates.put(from, remaining)
            executeMoveOrder(MoveOrder(amount - 1, from, to), crates)
      }

      val (crates, moveOrders) = parse(input)
      val mv2: Map[Position, ListBuffer[Char]] =
         crates.mapValuesInPlace((v: mutable.Stack[Char]) => mutable.ListBuffer.from(v))
      val res = moveOrders.foldLeft(mv2)((acc, next) => executeMoveOrder(next, acc))
      peekTop(res)

   }

}
