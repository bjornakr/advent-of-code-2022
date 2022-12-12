object Day1 extends Solver[Option[Int]] {

  override val dayNo = 1

  override val decode: String => Option[Int] = {
    case "" => None
    case x  => Some(x.toInt)
  }

  private def res(input: List[Option[Int]]): List[Int] = input.foldLeft[List[Int]](Nil) {
    case (ys, None)         => 0 :: ys
    case (y :: ys, Some(i)) => (y + i) :: ys
    case (Nil, Some(i))     => i :: Nil
  }

  override def solve1(input: List[Option[Int]] = realData): Int =
    res(input).max

  override def solve2(input: List[Option[Int]] = realData): Int =
    res(input).sorted(Ordering.Int.reverse).take(3).sum

}
