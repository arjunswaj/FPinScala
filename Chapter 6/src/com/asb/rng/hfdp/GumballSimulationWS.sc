import com.asb.rng.hfdp._
import com.asb.rng.hfdp.Gumball._

val gumballMachine = GumballMachine(NoQuarter, 15, 0)
val inputs = List(Insert, TurnCrank, Eject, Insert, Insert, TurnCrank, Insert, TurnCrank)

simulateGumballMachine(inputs).run(gumballMachine)

