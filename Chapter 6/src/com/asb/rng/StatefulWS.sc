import com.asb.rng.SimpleRNG

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt