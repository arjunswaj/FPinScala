package com.asb.pbt

import com.asb.pbt.Prop.{FailedCase, SuccessCount, TestCases}
import com.asb.rng.{RNG, State, random}
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

case class SGen[+A](forSize: Int => Gen[A])

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = true
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Falsified(f, _) => p.run(n, rng) match {
          case Falsified(f2, c) => Falsified(f + "\n" + f2, c)
          case x => x
        }
        case x => x
      }
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

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def tuple2[A](a: Gen[A]): Gen[(A, A)] =
    Gen(State.sequence(List.fill(2)(a.sample)).map(list => (list.head, list(1))))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(k => k.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(random.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(random.boolean))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val r1 = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(random.double)).flatMap(k => if (k < r1) g1._1 else g2._1)
  }

}
