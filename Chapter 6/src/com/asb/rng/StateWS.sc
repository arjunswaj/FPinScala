import com.asb.rng.State.Rand
import com.asb.rng.{SimpleRNG, State}
import com.asb.rng.State._

val rng = SimpleRNG(42)

int.run(rng)
ints(50).run(rng)

val ns: Rand[List[Int]] =
  int.flatMap(x =>
    int.flatMap(y =>
      ints(x).map(xs =>
        xs.map(_ % y)
      )
    )
  )

val ns2: Rand[List[Int]] =
  for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)