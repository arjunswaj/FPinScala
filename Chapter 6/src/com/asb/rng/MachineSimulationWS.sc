import com.asb.rng.{Coin, Machine, Turn}
import com.asb.rng.CandyStore._

val machine = Machine(true, 5, 10)
val operations = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
simulateMachine(operations).run(machine)
