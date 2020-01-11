import State.State.{modify, sequence, get}
import State.State

package object CandyDispenser {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(
      (m: Machine) => {
        val nm = inputs.foldLeft(m)(update)
        ((nm.coins, nm.candies), nm)
    }
  )

  private def update(m: Machine, i: Input): Machine = update(i)(m)

  private def update = (i: Input) => (m: Machine) =>
    (i, m) match {
      case (Coin, Machine(_, candies, coins)) => Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(
        locked = true,
        if (candies > 0) candies - 1 else 0,
        coins
      )
      case (Turn, m) => m
    }

  def simulateMachineAnswer(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map { modify[Machine] _ compose update })
      s <- get
    } yield (s.coins, s.candies)
//  sequence(inputs map { modify[Machine] compose update }) flatMap { _ =>
//    get map { s =>
//      (s.coins, s.candies)
//    }
//  }
}
