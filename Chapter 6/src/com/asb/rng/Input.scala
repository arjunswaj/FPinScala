package com.asb.rng

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyStore {

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  // Pass list of states to sequence
    sequence(
      // We have inputs, not lists, so map it
      inputs map (i =>
        // Modify the state of the Machine after processing every input
        modify[Machine](m => {
          // Do pattern matching on input and current state of machine
          // and return new state of machine
          (i, m) match {
            case (_, Machine(_, 0, _)) => m
            case (Coin, Machine(false, _, _)) => m
            case (Turn, Machine(true, _, _)) => m
            case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
            case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
          }
          // sequence returns State of Machine and List of Units
          // (coz modify didn't modify the value, only states)
        }))) flatMap (_ =>
      // Don't care about the value, get the (state, state)
      // and map value part to (int, int) from state
      get map (s =>
        (s.coins, s.candies)))

  // Curried function. Takes Input and returns a function
  // that takes Machine and returns Machine
  // Same as def update(i: Input)(m: Machine)
  def update = (i: Input) => (m: Machine) => {
    (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
    }
  }

  def elegantSimulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (i => modify[Machine](update(i))))
      s <- get
    } yield (s.coins, s.candies)
}
