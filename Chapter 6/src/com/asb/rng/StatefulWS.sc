import com.asb.rng.random.Rand
import com.asb.rng.{SimpleRNG, random}

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt

val (s1, s2) = random.sameRandomPair(rng)
val ((m1, m2), rng4) = random.randomPair(rng)

random.nonNegativeInt(random
  .nonNegativeInt(random
    .nonNegativeInt(rng)._2)._2)

random.double(rng)
random.doubleElegant(rng)

random.intDouble(rng)
random.doubleInt(rng)
random.double3(rng)

random.ints(50)(rng)

val int: Rand[Int] = _.nextInt
random.nonNegativeEven(rng)