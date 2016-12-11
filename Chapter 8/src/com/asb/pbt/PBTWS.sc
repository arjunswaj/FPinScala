import com.asb.pbt.Gen
import com.asb.rng.SimpleRNG

val rng = SimpleRNG(42)

Gen.listOfN(3, Gen.unit(5)).sample.run(rng)
Gen.listOfN(3, Gen.boolean).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(3, Gen.unit(3))).sample.run(rng)
Gen.listOfN(5, Gen.choose(2, 100)).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(5, Gen.choose(2, 285))).sample.run(rng)

// Single int in a range
Gen.choose(2, 5).sample.run(rng)

// Pair in a range
Gen.tuple2(Gen.choose(45, 563)).sample.run(rng)

// Option[A] to A
Gen.unit(None).sample.map(i => i.getOrElse(4)).run(rng)
Gen.unit(Some(2)).sample.map(i => i.getOrElse(4)).run(rng)

// A to Option
Gen.choose(2, 10).sample.map(i => Some(i)).run(rng)

// Generate strings. Do they mean this?
Gen.listOfN(5, Gen.choose(1, 10)).sample.map {
  case i  if i.length < 2 => "Lol"
  case _ => "TeeHee"
}.run(rng)