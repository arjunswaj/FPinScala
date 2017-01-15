package com.asb.pbt

import com.asb.parallelism.Par
import com.asb.parallelism.Par.Par
import com.asb.rng.{RNG, SimpleRNG, State, random}
import com.asb.snl.Stream

/**
  * Gen
  * Created by arjun on 04/12/16.
  */
case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (i => listOfN(i))

  def unsized: SGen[A] = SGen(i => this)
}

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize.andThen(genA => genA map f))

  def flatMap1[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize.andThen(genA => genA flatMap f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap {
        f(_).forSize(n)
      }
    }
    SGen(g2)
  }

  def listOfN(size: Int): SGen[List[A]] =
    SGen(forSize.andThen(gen => gen.listOfN(size)))

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int


  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = true
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop {
      (m, n, rng) =>
        run(m, n, rng) match {
          case Passed | Proved => p.run(m, n, rng)
          case x => x
        }
    }

    def ||(p: Prop): Prop = Prop {
      (m, n, rng) =>
        run(m, n, rng) match {
          case Falsified(f, _) => p.run(m, n, rng) match {
            case Falsified(f2, c) => Falsified(f + "\n" + f2, c)
            case x => x
          }
          case x => x
        }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop {
          (max, _, rng) => p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Passed else Falsified(" () ", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(random.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val r1 = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(random.double)).flatMap(k => if (k < r1) g1._1 else g2._1)
  }

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

}

//trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//
//  def &&(p: Prop): Prop = new Prop {
//    def check: Either[(FailedCase, SuccessCount), SuccessCount] =
//      Prop.this.check.right.flatMap(sc => p.check.fold(err => Left((err._1, err._2 + sc)),
//        succ => Right(sc + succ)))
//  }
//
//}

object Gen {

  def listOf1[A](a: Gen[A]): SGen[List[A]] =
    SGen(n => a.listOfN(n max 1))

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  def tuple2[A](a: Gen[A]): Gen[(A, A)] =
    Gen(State.sequence(List.fill(2)(a.sample)).map(list => (list.head, list(1))))

  def boolean: Gen[Boolean] = Gen(State(random.boolean))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

}
