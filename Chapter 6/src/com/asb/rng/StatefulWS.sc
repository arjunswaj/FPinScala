import com.asb.rng.{SimpleRNG, random}

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt

val (s1, s2) = random.sameRandomPair(rng)
val ((m1, m2), rng4) = random.randomPair(rng)