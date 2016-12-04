package com.asb.pbt

import com.asb.pbt.Prop.{FailedCase, SuccessCount}
import com.asb.rng.{RNG, State, random}

import scala.util.Left

/**
  * Gen
  * Created by arjun on 04/12/16.
  */
case class Gen[A](sample: State[RNG, A])

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      Prop.this.check.right.flatMap(sc => p.check.fold(err => Left((err._1, err._2 + sc)),
        succ => Right(sc + succ)))
  }

}

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(random.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(random.boolean))

}
