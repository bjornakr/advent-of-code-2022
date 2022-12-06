object Day2 extends Solver[(Char, Char)] {
  override val dayNo = 2

  override val decode: String => (Char, Char) = { s =>
    val split = s.split(" ")
    (split(0).charAt(0), split(1).charAt(0))
  }

  enum Action(val points: Int) {
    case Rock extends Action(1)
    case Paper extends Action(2)
    case Scissors extends Action(3)
  }
  import Action.*

  val toAction: Char => Action = {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors
    case other     => throw Exception(s"invalid action: $other")
  }

  def solve1(input: List[(Char, Char)] = realData) = {

    val game: List[(Action, Action)] = input.map((a, x) => (toAction(a), toAction(x)))

    val scoreRound: (Action, Action) => Int = {
      case (Rock, Scissors)  => 6
      case (Paper, Rock)     => 6
      case (Scissors, Paper) => 6
      case (a, x) if a == x  => 3
      case _                 => 0
    }

    game.map((a, x) => scoreRound(x, a) + x.points).sum
  }

  def solve2(input: List[(Char, Char)] = realData) = {
    enum Outcome {
      case Win, Lose, Draw
    }
    import Outcome.*

    val toOutome: Char => Outcome = {
      case 'X'   => Lose
      case 'Y'   => Draw
      case 'Z'   => Win
      case other => throw Exception(s"invalid action: $other")
    }

    val game: List[(Action, Outcome)] = input.map((a, x) => (toAction(a), toOutome(x)))

    val scoreRound: (Action, Outcome) => Int = {
      case (Rock, Lose)     => Scissors.points
      case (Rock, Win)      => 6 + Paper.points
      case (Scissors, Lose) => Paper.points
      case (Scissors, Win)  => 6 + Rock.points
      case (Paper, Lose)    => Rock.points
      case (Paper, Win)     => 6 + Scissors.points
      case (action, Draw)   => 3 + action.points
    }

    game.map((a, o) => scoreRound(a, o)).sum

  }
}
