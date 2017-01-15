import com.asb.pbt.{Gen, Prop}
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
  case i if i.length < 2 => "Lol"
  case _ => "TeeHee"
}.run(rng)

val smallInt = Gen.choose(-10, 10)
val maxProp = Prop.forAll(Gen.listOf(smallInt)) {
  ns =>
    val max = if (ns.isEmpty) 0 else ns.max
    !ns.exists(_ > max)
}

Prop.run(maxProp)

val maxProp1 = Prop.forAll(Gen.listOf1(smallInt)) {
  l =>
    val max = l.max
    !l.exists(_ > max)
}

Prop.run(maxProp1)

val sortedProp = Prop.forAll(Gen.listOf(smallInt)) {
  l =>
  val sl = l.sorted
    sl.foldLeft((Int.MinValue, true))((acc, ele) => (ele, acc._2 && acc._1 <= ele))._2 &&
    !l.exists(k => !sl.contains(k)) &&
    !sl.exists(k => !l.contains(k))
}

Prop.run(sortedProp)