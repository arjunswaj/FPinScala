import com.asb.pbt.Gen
import com.asb.rng.SimpleRNG

val rng = SimpleRNG(42)

Gen.listOfN(3, Gen.unit(5)).sample.run(rng)
Gen.listOfN(3, Gen.boolean).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(3, Gen.unit(3))).sample.run(rng)
Gen.listOfN(5, Gen.choose(2, 100)).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(5, Gen.choose(2, 285))).sample.run(rng)