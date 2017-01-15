import java.util.concurrent.{ExecutorService, Executors}

import com.asb.parallelism.Par
import com.asb.pbt.{Gen, Prop}
import com.asb.rng.SimpleRNG

val rng = SimpleRNG(42)

Gen.listOfN(3, Prop.unit(5)).sample.run(rng)
Gen.listOfN(3, Gen.boolean).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(3, Prop.unit(3))).sample.run(rng)
Gen.listOfN(5, Prop.choose(2, 100)).sample.run(rng)
Gen.listOfN(3, Gen.listOfN(5, Prop.choose(2, 285))).sample.run(rng)

// Single int in a range
Prop.choose(2, 5).sample.run(rng)

// Pair in a range
Gen.tuple2(Prop.choose(45, 563)).sample.run(rng)

// Option[A] to A
Prop.unit(None).sample.map(i => i.getOrElse(4)).run(rng)
Prop.unit(Some(2)).sample.map(i => i.getOrElse(4)).run(rng)

// A to Option
Prop.choose(2, 10).sample.map(i => Some(i)).run(rng)

// Generate strings. Do they mean this?
Gen.listOfN(5, Prop.choose(1, 10)).sample.map {
  case i if i.length < 2 => "Lol"
  case _ => "TeeHee"
}.run(rng)

val smallInt = Prop.choose(-10, 10)
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

val ES: ExecutorService = Executors.newCachedThreadPool

val p3 = Prop.check {
  Prop.equal(
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )(ES).get
}
p3.run(1, 1, rng)

val p2 = Prop.checkPar{
  Prop.equal(
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )
}
p2.run(1, 1, rng)