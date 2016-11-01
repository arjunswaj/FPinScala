package com.asb.rng.hfdp

import com.asb.rng.State

sealed trait MState {
  def insertQuarter(machine: GumballMachine): Either[Exception, GumballMachine] = machine.state match {
    case Sold => e("Wait! We are giving you the candy.")
    case SoldOut => e("Sold out! No more candies!")
    case HasQuarter => e("Although I'd love it, master says you cant insert another quarter")
    case Winner => e("You are a winner, you cant insert a quarter")
    // Valid case
    case NoQuarter => Right(GumballMachine(HasQuarter, machine.candies, machine.quarters + 1))
  }

  def ejectQuarter(machine: GumballMachine): Either[Exception, GumballMachine] = machine.state match {
    case Sold => e("Haha. You've already turned the crank. No backsies.")
    case SoldOut => e("Sold out! No more candies!")
    case NoQuarter => e("Duh! You think this machine is dumb?")
    case Winner => e("You are a winner, just turn the crank.")
    // Valid case
    case HasQuarter => Right(GumballMachine(NoQuarter, machine.candies, machine.quarters - 1))
  }

  def turnCrank(machine: GumballMachine): Either[Exception, GumballMachine] = machine.state match {
    case Sold => e("You cannot get another gumball when you turn it twice.")
    case SoldOut => e("Sold out! No more candies!")
    case NoQuarter => e("No charity. Pay money and get your Gumball.")
    case Winner => e("You are getting 2. You won't get more.")
    // Valid case
    case HasQuarter =>
      if (machine.candies % 3 == 0)
        Right(GumballMachine(Winner, machine.candies, machine.quarters))
      else
        Right(GumballMachine(Sold, machine.candies, machine.quarters))
  }

  def dispense(machine: GumballMachine): Either[Exception, GumballMachine] = machine.state match {
    case SoldOut => e("Sold Out. No candy to dispense.")
    case NoQuarter => e("Cannot dispense now.")
    case HasQuarter => e("Turn the crank to dispense.")
    // Valid cases
    case Sold =>
      if (machine.candies > 0) {
        machine.releaseBall
        Right(GumballMachine(NoQuarter, machine.candies - 1, machine.quarters))
      } else {
        Right(GumballMachine(SoldOut, 0, machine.quarters))
      }
    case Winner =>
      if (machine.candies > 0) {
        machine.releaseBall
        dispense(GumballMachine(Sold, machine.candies - 1, machine.quarters))
      } else {
        Right(GumballMachine(SoldOut, 0, machine.quarters))
      }
  }

  private def e(err: String): Left[Exception, GumballMachine] = Left(new Exception(err))

}

case object Sold extends MState

case object SoldOut extends MState

case object NoQuarter extends MState

case object HasQuarter extends MState

case object Winner extends MState

case class GumballMachine(state: MState, candies: Int, quarters: Int) {
  def insertQuarter: Either[Exception, GumballMachine] = state insertQuarter this

  def ejectQuarter: Either[Exception, GumballMachine] = state ejectQuarter this

  def turnCrank: Either[Exception, GumballMachine] =
    state turnCrank this fold(Left(_), m => m.state.dispense(m))

  def releaseBall = println("Candies in machine: " + candies + ", Releasing a candy.")

}

sealed trait Input

case object Insert extends Input

case object Eject extends Input

case object TurnCrank extends Input

object Gumball {

  import com.asb.rng.State._

  def simulateGumballMachine(inputs: List[Input]): State[GumballMachine, (Int, Int)] = {

    def exHandler(e: Exception, m: GumballMachine): GumballMachine = {
      println(e.getMessage)
      m
    }

    def update = (input: Input) => (machine: GumballMachine) => {
      input match {
        case Insert => machine.insertQuarter.fold(exHandler(_, machine), identity)
        case Eject => machine.ejectQuarter.fold(exHandler(_, machine), identity)
        case TurnCrank => machine.turnCrank.fold(exHandler(_, machine), identity)
      }
    }

    for {
      _ <- sequence(inputs map (input => modify[GumballMachine](update(input))))
      s <- get
    } yield (s.quarters, s.candies)

  }

}
