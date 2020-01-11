import RNG.RNG._
import RNG._
import State._
import CandyDispenser._

object Main {
  def rollDie: Rand[Int] = RNG.map(nonNegativeLessThan(6)){ _ + 1 }

  def main(argv: Array[String]): Unit = {
    // Mediocre impure non-functional approach, containing blasphemous side-effects
//    val rng = new scala.util.Random
//    println(rng.nextDouble)
//    println(rng.nextDouble)
//    println(rng.nextInt)
//    println(rng.nextInt(4))

    // Elite, right, pure, holy & referentially transparent functional approach
    val rng1 = SimpleRNG(42)
    println(rng1)
    val (n1, rng2) = rng1.nextInt
    println(n1)
    println(rng2)
    val (n2, rng3) = rng2.nextInt
    println(n2)
    println(rng3)
    println(rng3.nextInt._1)

    println("\nnonNegativeInt")
    println(rng1.nonNegativeInt._1)
    println(rng2.nonNegativeInt._1)
    println(rng3.nonNegativeInt._1)

    println("\nnextDoubleBetweenZeroOne")
    println(rng1.nextDoubleBetweenZeroOne._1)
    println(rng2.nextDoubleBetweenZeroOne._1)
    println(rng3.nextDoubleBetweenZeroOne._1)

    println("\ndouble")
    println(double(rng1)._1)
    println(double(rng2)._1)
    println(double(rng3)._1)

    println("\nnextIntDouble")
    println(rng1.nextIntDouble._1)
    println("randIntDouble")
    println(randIntDouble(rng1)._1)
    println("\nnextDoubleInt")
    println(rng1.nextDoubleInt._1)
    println("randDoubleInt")
    println(randDoubleInt(rng1)._1)
    println("\nnextDouble3")
    println(rng1.nextDouble3._1)

    println("\nints")
    println(rng1.ints(10)._1)
    println("\nints (using sequence)")
    println(ints(10)(rng1)._1)

    println("\nsequence")
    println(sequence(List[Rand[Int]](
      rng => rng.nextInt,
      rng => rng.nextInt,
      rng => rng.nextInt,
    ))(rng1)._1)

    println("\nnonNegativeLessThan")
    println(nonNegativeLessThan(11)(rng1)._1)

    println("\n\nUnderstanding flatMap")
    val ri = State((rng: RNG) => rng.nextInt)
    val rd = State((rng: RNG) => rng.nextDoubleBetweenZeroOne)

    val res = ri flatMap { i =>
      rd
    }

    println(ri.run(rng1))
    println(rd.run(rng1))
    println(rd.run(rng1.nextInt._2))
    println(res.run(rng1))

    println("\n\nCandy dispenser")
    val m = Machine(locked = true, 5, 10)

    println(simulateMachine(
      List(Turn, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn))
      .run(m)._1
    )
    println(simulateMachineAnswer(
      List(Turn, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn))
      .run(m)._1
    )
  }
}
